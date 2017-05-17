open Lwt.Infix

module Log = Rpc.Log

let qid_tag = Logs.Tag.def "qid" Uint32.printer

module Builder = Schema.Builder
module Reader = Schema.Reader

(* todo: ID lifecycle in both directions. See QuestionId definition in schema, etc. *)

module Questions = Table.Allocating ( )
module Answers = Table.Tracking ( )
module Exports = Table.Allocating ( )
module Imports = Table.Tracking ( )

let pp_call f call =
  let open Reader in
  let interface_id = Call.interface_id_get call in
  let method_id = Call.method_id_get call in
  Capnp.RPC.Registry.pp_method f (interface_id, method_id)

type t = {
  endpoint : Endpoint.t;
  questions : (Promise.promise * (Rpc.resp_msg * Rpc.value Ro_array.t) Lwt.u) Questions.t;
  answers : Rpc.value Answers.t;
  exports : Rpc.value Exports.t;
  my_bootstrap : Exports.key option;
  tags : Logs.Tag.set;
}

let async_tagged label fn =
  Lwt.async
    (fun () ->
      Lwt.catch fn
        (fun ex ->
           Log.warn (fun f -> f "Uncaught async exception in %S: %a" label Fmt.exn ex);
           Lwt.return_unit
        )
    )

let unfinished _ _ _ =
  raise (Failure "Attempt to use value still under construction! (cycle detected)")

let with_qid t qid = Logs.Tag.add qid_tag (Questions.uint32 qid) t.tags
let with_aid t qid = Logs.Tag.add qid_tag (Answers.uint32 qid) t.tags

let write_caps_array t caps payload =
  let open Builder in
  let builder = Payload.cap_table_init payload (Ro_array.length caps) in
  caps |> Ro_array.iteri (fun i x ->
      let slot = Capnp.Array.get builder i in
      (* TODO: simplify path if possible *)
      let export_id = Exports.alloc t.exports x in
      CapDescriptor.sender_hosted_set slot (Exports.uint32 export_id);
      Log.info (fun f -> f ~tags:t.tags "New export %a (with msg index %d)" Exports.pp_key export_id i);
    )

(* Forwards calls to the remote, as that has the best chance of resolving them. *)
let rec remote_call_handler t target xforms c caps =
  let open Builder in
  let c = Rpc.writable_req c in
  let value = new Promise.promise unfinished in
  let msg, msg_waker = Lwt.wait () in
  let qid = Questions.alloc t.questions (value, msg_waker) in
  value#set_handler (remote_call_handler t (`PromisedAnswer qid));
  Call.question_id_set c (Questions.uint32 qid);
  let tags = with_qid t qid in
  Log.info (fun f -> f ~tags "Forwarding call %a to remote" pp_call (Call.to_reader c));
  let () = (* Set target *)
    let msg_target = Call.target_init c in
    match target with
    | `PromisedAnswer qid ->
      let pa = MessageTarget.promised_answer_init msg_target in
      PromisedAnswer.question_id_set pa (Questions.uint32 qid);
      let xforms_builder = PromisedAnswer.transform_init pa (List.length xforms) in
      xforms |> List.iteri (fun i (Xform.Field f) ->
          let b = Capnp.Array.get xforms_builder i in
          PromisedAnswer.Op.get_pointer_field_set_exn b f
        )
    | `ImportedCap id ->
      if xforms <> [] then failwith "Can't transform a capability!";
      MessageTarget.imported_cap_set msg_target (Imports.uint32 id)
  in
  let p = Call.params_get c in
  write_caps_array t caps p;
  Lwt.async (fun () ->
      let m = Call.to_message c in
      Endpoint.send t.endpoint m
    );
  (value :> Rpc.value), msg

let bootstrap t =
  let value = new Promise.promise unfinished in
  let _, dont_care = Lwt.wait () in
  let qid = Questions.alloc t.questions (value, dont_care) in
  value#set_handler (remote_call_handler t (`PromisedAnswer qid));
  let open Builder in
  let b = Message.init_root () in
  let boot = Message.bootstrap_init b in
  Bootstrap.question_id_set boot (Questions.uint32 qid);
  Log.info (fun f -> f ~tags:(with_qid t qid) "Requesting bootstrap service");
  async_tagged "Send bootstrap"
    (fun () -> Endpoint.send t.endpoint (Message.to_message b));
  (value :> Rpc.value)

let rec parse_xforms = function
  | [] -> []
  | x :: xs ->
    let open Reader.PromisedAnswer in
    match Op.get x with
    | Op.Noop -> parse_xforms xs
    | Op.GetPointerField x -> Xform.Field x :: parse_xforms xs
    | _ -> failwith "parse_xforms: unsupported op type"

let value_of_promised_answer t pa =
  let open Reader in
  let qid = PromisedAnswer.question_id_get pa |> Answers.of_uint32 in
  let trans = PromisedAnswer.transform_get_list pa |> parse_xforms in
  Log.info (fun f ->
      let tags = with_aid t qid in
      f ~tags "Look up answers table (and apply %a)" (Fmt.Dump.list Xform.pp) trans
    );
  let value = Answers.find_exn t.answers qid in
  Promise.apply trans value

(* Convert a list of CapDescriptors into a list of values. *) 
let import_caps t ds =
  ds |> Ro_array.map (fun d ->
      let open Reader in
      match CapDescriptor.get d with
      | CapDescriptor.None -> Promise.broken "Blank capability received!"
      | CapDescriptor.SenderHosted id
      | CapDescriptor.SenderPromise id ->
        let id = Imports.of_uint32 id in
        Log.info (fun f -> f ~tags:t.tags "Added import %a" Imports.pp_key id);
        let value = new Promise.promise (remote_call_handler t (`ImportedCap id)) in
        (value :> Rpc.value)
      | CapDescriptor.ReceiverHosted id ->
        let id = Exports.of_uint32 id in
        Exports.find_exn t.exports id
      | CapDescriptor.ReceiverAnswer p ->
        value_of_promised_answer t p
      | CapDescriptor.ThirdPartyHosted tp ->
        let id = ThirdPartyCapDescriptor.vine_id_get tp |> Imports.of_uint32 in
        (* todo: for level 3, we should establish a direct connection rather than proxying
           through the vine *)
        let value = new Promise.promise (remote_call_handler t (`ImportedCap id)) in
        (value :> Rpc.value)
      | CapDescriptor.Undefined _ -> failwith "Unknown cap descriptor type"
    )

let handle_return t return =
  let open Reader in
  let qid = Return.answer_id_get return |> Questions.of_uint32 in
  let question, msg_waker = Questions.find_exn t.questions qid in
  let tags = with_qid t qid in
  (* TODO: remove from table on GC *)
  match Return.get return with
  | Return.Results results ->
    let content = Payload.content_get results in
    let caps = import_caps t (Payload.cap_table_get_list results |> Ro_array.of_list) in
    let answer =
      let open Capnp.Runtime in
      match content with
      | None ->
        Log.info (fun f -> f ~tags "Got empty results answer");
        Payload_caps.value Ro_array.empty
      | Some ptr ->
        let ptr = (ptr :> Capnp.Message.ro Capnp.BytesMessage.Slice.t) in
          match Pointer.decode (Capnp.BytesMessage.Slice.get_int64 ptr 0) with
          | Pointer.Other (OtherPointer.Capability i) ->
            Log.info (fun f -> f ~tags "Got exported capability answer");
            Ro_array.get caps (Uint32.to_int i)
          | _ ->
            Log.info (fun f -> f ~tags "Got struct results answer");
            Payload_caps.of_content ~caps content
    in
    question#resolve answer;
    Lwt.wakeup msg_waker (Rpc.Readonly return, caps)
  | Return.Exception ex ->
    let reason = Exception.reason_get ex in
    Log.info (fun f -> f ~tags "Got exception %S" reason);
    question#resolve (Promise.broken reason);
    Lwt.wakeup_exn msg_waker (Failure reason)
  | _ ->
    Log.warn (fun f -> f ~tags "Got unknown return type");
    failwith "Unexpected return type received"

let init_exn ~reason ~ty ret =
  let open Builder in
  let ex = Return.exception_init ret in
  Exception.type_set ex ty;
  match reason with
  | Failure msg -> Exception.reason_set ex msg
  | x -> Exception.reason_set ex (Printexc.to_string x)

let results_of_return ret =
  let open Builder in
  match Return.get ret with
  | Return.Results r -> r
  | _ -> failwith "results_of_return: not results!"

(* We have received a question from our peer. *)
let handle_call t call =
  let open Reader in
  let aid = Call.question_id_get call |> Answers.of_uint32 in
  let tags = with_aid t aid in
  let open Reader in
  (* Resolve capabilities *)
  let p = Call.params_get call in
  let caps = import_caps t (Payload.cap_table_get_list p |> Ro_array.of_list) in
  (* Get target *)
  let target =
    match Call.target_get call |> MessageTarget.get with
    | MessageTarget.Undefined _ -> failwith "Bad MessageTarget"
    | MessageTarget.ImportedCap id ->
      let id = Exports.of_uint32 id in
      Log.info (fun f -> f ~tags "Got call : export[%a].%a" Exports.pp_key id pp_call call);
      Exports.find_exn t.exports id
    | MessageTarget.PromisedAnswer x ->
      Log.info (fun f ->
          let id = PromisedAnswer.question_id_get x |> Answers.of_uint32 in
          f ~tags "Got call : answer[%a].%a" Answers.pp_key id pp_call call
        );
      value_of_promised_answer t x
  in
  (* Do call *)
  let answer, resp = target#call [] (Rpc.Readonly call) caps in
  (* Record answer *)
  Answers.set t.answers aid answer;
  (* Transmit answer *)
  Lwt.async
    (fun () ->
       let open Builder in
       resp >>= fun (ret, caps) ->
       (* [resp] has payload filled in, but nothing else. *)
       let ret = Rpc.writable_resp ret in
       Return.answer_id_set ret (Answers.uint32 aid);
       let p = results_of_return ret in
       write_caps_array t caps p;
       Log.info (fun f -> f ~tags "Sending reply to %a" pp_call call);
       Endpoint.send t.endpoint (Builder.Return.to_message ret)
    )

let handle_bootstrap t boot =
  let open Builder in
  let m = Message.init_root () in
  let ret = Message.return_init m in
  let open Reader in
  let qid = Bootstrap.question_id_get boot |> Answers.of_uint32 in
  let tags = with_aid t qid in
  Log.info (fun f -> f ~tags "Got bootstrap request");
  Builder.Return.answer_id_set ret (Answers.uint32 qid);
  let service =
    let open Builder in
    match t.my_bootstrap with
    | None ->
      let ex = "No bootstrap object" in
      init_exn ~reason:(Failure ex) ~ty:Exception.Type.Failed ret;
      Promise.broken ex
    | Some my_bootstrap ->
      let res = Return.results_init ret in
      Payload.content_set_interface res (Some Uint32.zero);       (* Cap index 0 *)
      let caps = Payload.cap_table_init res 1 in
      let cap0 = Capnp.Array.get caps 0 in
      CapDescriptor.sender_hosted_set cap0 (Exports.uint32 my_bootstrap);
      Exports.find_exn t.exports my_bootstrap
  in
  Log.info (fun f -> f ~tags "Sending reply to bootstrap");
  Answers.set t.answers qid service;
  async_tagged "Reply to bootstrap"
    (fun () ->
       let open Builder in
       Endpoint.send t.endpoint (Message.to_message m)
    )

let listen t =
  let rec loop () =
    Endpoint.recv t.endpoint >>= function
    | Error e -> Lwt.return e
    | Ok msg ->
      begin
        let open Reader in
        let msg = Reader.Message.of_message msg in
        match Message.get msg with
        | Message.Call call ->
          handle_call t call;
          loop ()
        | Message.Bootstrap boot ->
          handle_bootstrap t boot;
          loop ()
        | Message.Return return ->
          handle_return t return;
          loop ()
        | _ -> failwith "Not a call!"
      end
  in
  loop ()

let of_endpoint ?offer ?(tags=Logs.Tag.empty) ~switch endpoint =
  let exports = Exports.make () in
  let my_bootstrap =
    match offer with
      | Some service -> Some (Exports.alloc exports service)
      | None -> None
  in
  let t = {
    endpoint;
    my_bootstrap;
    questions = Questions.make ();
    answers = Answers.make ();
    exports;
    tags
  } in
  Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
           listen t >|= fun `Closed -> ()
        )
        (fun ex ->
           Log.warn (fun f ->
               f ~tags "Uncaught exception handling CapTP connection: %a (dropping connection)" Fmt.exn ex
             );
           Lwt.return_unit
        )
      >>= fun () ->
      Log.info (fun f -> f ~tags "Connection closed");
      Lwt_switch.turn_off switch
      (* todo: notify users *)
    );
  t
