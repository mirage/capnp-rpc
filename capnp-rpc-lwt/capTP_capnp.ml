open Lwt.Infix
open Capnp_core

module Log = Rpc.Log

module Builder = Schema.Builder
module Reader = Schema.Reader
module RO_array = Capnp_rpc.RO_array

module Table_types = struct
  module QuestionId = Capnp_rpc.Id.Make ( )
  module AnswerId = Capnp_rpc.Id.Make ( )
  module ExportId = Capnp_rpc.Id.Make ( )
  module ImportId = Capnp_rpc.Id.Make ( )
end
module Proto = Protocol.Make(Table_types)
module Conn = CapTP.Make(Proto)
include Table_types

type t = {
  endpoint : Endpoint.t;
  conn : Conn.t;
}

let bootstrap t = Conn.bootstrap t.conn

let async_tagged label fn =
  Lwt.async
    (fun () ->
      Lwt.catch fn
        (fun ex ->
           Log.warn (fun f -> f "Uncaught async exception in %S: %a" label Fmt.exn ex);
           Lwt.return_unit
        )
    )

let pp_call f call =
  let open Reader in
  let interface_id = Call.interface_id_get call in
  let method_id = Call.method_id_get call in
  Capnp.RPC.Registry.pp_method f (interface_id, method_id)

let tags ?qid ?aid t = Conn.tags ?qid ?aid t.conn

let write_promised_answer pa (qid, xforms) =
  let open Builder in
  PromisedAnswer.question_id_set pa (QuestionId.uint32 qid);
  let xforms_builder = PromisedAnswer.transform_init pa (List.length xforms) in
  xforms |> List.iteri (fun i (Xform.Field f) ->
      let b = Capnp.Array.get xforms_builder i in
      PromisedAnswer.Op.get_pointer_field_set_exn b f
    )

let write_caps_array caps payload =
  let open Builder in
  let builder = Payload.cap_table_init payload (RO_array.length caps) in
  caps |> RO_array.iteri (fun i x ->
      let slot = Capnp.Array.get builder i in
      match x with
      | `None -> ()
      | `ReceiverHosted id -> CapDescriptor.receiver_hosted_set slot (ImportId.uint32 id)
      | `ReceiverAnswer x -> write_promised_answer (CapDescriptor.receiver_answer_init slot) x
      | `SenderHosted id -> CapDescriptor.sender_hosted_set slot (ExportId.uint32 id)
      | _ -> failwith "TODO: write_caps_array"
    )

let parse_xform x =
  let open Reader.PromisedAnswer.Op in
  match get x with
  | Noop -> []
  | GetPointerField y -> [Xform.Field y]
  | Undefined _ -> failwith "Unknown transform type"

let parse_promised_answer pa =
  let open Reader in
  let qid = PromisedAnswer.question_id_get pa |> AnswerId.of_uint32 in
  let trans = PromisedAnswer.transform_get_list pa |> List.map parse_xform |> List.concat in
  `ReceiverAnswer (qid, trans)

let parse_desc d =
  let open Reader in
  match CapDescriptor.get d with
  | CapDescriptor.None -> `None
  | CapDescriptor.SenderHosted id -> `SenderHosted (ImportId.of_uint32 id)
  | CapDescriptor.SenderPromise id -> `SenderPromise (ImportId.of_uint32 id)
  | CapDescriptor.ReceiverHosted id -> `ReceiverHosted (ExportId.of_uint32 id)
  | CapDescriptor.ReceiverAnswer p -> parse_promised_answer p
  | CapDescriptor.ThirdPartyHosted tp ->
    let vine_id = ThirdPartyCapDescriptor.vine_id_get tp |> ImportId.of_uint32 in
    (* todo: for level 3, we should establish a direct connection rather than proxying
       through the vine *)
    `ThirdPartyHosted {Protocol.id = `TODO_3rd_party; vine_id}
  | CapDescriptor.Undefined _ -> failwith "Unknown cap descriptor type"

let parse_descs = RO_array.map parse_desc

let handle_return t return =
  let open Reader in
  let qid = Return.answer_id_get return |> QuestionId.of_uint32 in
  match Return.get return with
  | Return.Results results ->
    let descs = parse_descs (Payload.cap_table_get_list results |> RO_array.of_list) in
    Conn.handle_msg t.conn (`Return (qid, `Results (Rpc.Readonly return, descs)))
  | Return.Exception ex ->
    let reason = Exception.reason_get ex in
    Log.info (fun f -> f ~tags:(tags ~qid t) "Got exception %S" reason);
    Conn.handle_msg t.conn (`Return (qid, `Exception reason))
  | Return.Canceled ->
    Conn.handle_msg t.conn (`Return (qid, `Cancelled))
  | _ ->
    Log.warn (fun f -> f ~tags:(tags ~qid t) "Got unknown return type");
    failwith "Unexpected return type received"

let handle_finish t finish =
  let open Reader in
  let aid = Finish.question_id_get finish |> AnswerId.of_uint32 in
  let release = Finish.release_result_caps_get finish in
  Conn.handle_msg t.conn (`Finish (aid, release))

let results_of_return ret =
  let open Builder in
  match Return.get ret with
  | Return.Results r -> r
  | _ -> failwith "results_of_return: not results!"

let parse_target msg_target =
  let open Reader in
  match MessageTarget.get msg_target with
  | MessageTarget.Undefined _ -> failwith "Bad MessageTarget"
  | MessageTarget.ImportedCap id -> `ReceiverHosted (ExportId.of_uint32 id)
  | MessageTarget.PromisedAnswer x -> parse_promised_answer x

(* We have received a question from our peer. *)
let handle_call t call =
  let open Reader in
  let aid = Call.question_id_get call |> AnswerId.of_uint32 in
  Log.info (fun f -> f ~tags:(tags ~aid t) "Received call %a" pp_call call);
  (* Resolve capabilities *)
  let p = Call.params_get call in
  let descs = parse_descs (Payload.cap_table_get_list p |> RO_array.of_list) in
  (* Get target *)
  let target = parse_target (Call.target_get call) in
  let msg = Rpc.Readonly call in
  Conn.handle_msg t.conn (`Call (aid, target, msg, descs))

let handle_bootstrap t boot =
  let open Reader in
  let qid = Bootstrap.question_id_get boot |> AnswerId.of_uint32 in
  Conn.handle_msg t.conn (`Bootstrap qid)

let handle_disembargo t x =
  let open Reader in
  let target = parse_target (Disembargo.target_get x) in
  let ctx = Disembargo.context_get x in
  match Disembargo.Context.get ctx with
  | Disembargo.Context.SenderLoopback embargo_id ->
    let embargo_id = Protocol.EmbargoId.of_uint32 embargo_id in
    begin match target with
    | `ReceiverAnswer (aid, path) ->
      let req = `Loopback ((aid, path), embargo_id) in
      Conn.handle_msg t.conn (`Disembargo_request req)
    | `ReceiverHosted _ -> failwith "TODO: handle_disembargo: ReceiverHosted"   (* Can this happen? *)
    end
  | Disembargo.Context.ReceiverLoopback embargo_id ->
    let embargo_id = Protocol.EmbargoId.of_uint32 embargo_id in
    begin match target with
    | `ReceiverHosted id ->
      Conn.handle_msg t.conn (`Disembargo_reply ((`ReceiverHosted id), embargo_id))
    | `ReceiverAnswer _ -> failwith "TODO: handle_disembargo: ReceiverAnswer"        (* Can this happen? *)
    end
  | _ -> failwith "TODO: handle_disembargo"

let listen t =
  let rec loop () =
    Endpoint.recv t.endpoint >>= function
    | Error e -> Lwt.return e
    | Ok msg ->
      begin
        let open Reader in
        let msg = Reader.Message.of_message msg in
        match Message.get msg with
        | Message.Call x       -> handle_call t x
        | Message.Bootstrap x  -> handle_bootstrap t x
        | Message.Return x     -> handle_return t x
        | Message.Finish x     -> handle_finish t x
        | Message.Disembargo x -> handle_disembargo t x
        | _ -> failwith "TODO: listen"
      end;
      loop ()
  in
  loop ()

let set_target b target =
  let open Builder in
  match target with
  | `ReceiverAnswer (id, i) ->
    let builder = MessageTarget.promised_answer_init b in
    write_promised_answer builder (id, i)
  | `ReceiverHosted id ->
    MessageTarget.imported_cap_set b (ImportId.uint32 id)

let serialise ~tags : Proto.Out.t -> _ =
  let open Builder in
  function
  | `Bootstrap qid ->
    let b = Message.init_root () in
    let boot = Message.bootstrap_init b in
    Bootstrap.question_id_set boot (QuestionId.uint32 qid);
    Log.info (fun f ->
        let tags = Logs.Tag.add Capnp_rpc.Debug.qid_tag (QuestionId.uint32 qid) tags in
        f ~tags "Requesting bootstrap service"
      );
    Message.to_message b
  | `Call (qid, target, request, descs) ->
    let c = Rpc.writable_req request in
    Call.question_id_set c (QuestionId.uint32 qid);
    set_target (Call.target_init c) target;
    let p = Call.params_get c in
    write_caps_array descs p;
    Call.to_message c
  | `Finish (qid, release_result_caps) ->
    let b = Message.init_root () in
    let fin = Message.finish_init b in
    Finish.question_id_set fin (QuestionId.uint32 qid);
    Finish.release_result_caps_set fin release_result_caps;
    Message.to_message b
  | `Release (id, count) ->
    let m = Message.init_root () in
    let rel = Message.release_init m in
    Release.id_set rel (ImportId.uint32 id);
    Release.reference_count_set_int_exn rel count;
    Message.to_message m
  | `Disembargo_request disembargo_request ->
    let m = Message.init_root () in
    let dis = Message.disembargo_init m in
    let ctx = Disembargo.context_init dis in
    begin match disembargo_request with
      | `Loopback ((qid, path), embargo_id) ->
        set_target (Disembargo.target_init dis) (`ReceiverAnswer (qid, path));
        Disembargo.Context.sender_loopback_set ctx (Protocol.EmbargoId.uint32 embargo_id)
    end;
    Message.to_message m
  | `Disembargo_reply (target, embargo_id) ->
    let m = Message.init_root () in
    let dis = Message.disembargo_init m in
    let ctx = Disembargo.context_init dis in
    set_target (Disembargo.target_init dis) target;
    Disembargo.Context.receiver_loopback_set ctx (Protocol.EmbargoId.uint32 embargo_id);
    Message.to_message m
  | `Return (aid, return) ->
    let ret =
      match return with
        | `Results (msg, descs) ->
          (* [msg] has payload filled in, but nothing else. *)
          let ret = Rpc.writable_resp msg in
          write_caps_array descs (results_of_return ret);
          ret
        | `Exception msg ->
          let m = Message.init_root () in
          let ret = Message.return_init m in
          let ex = Return.exception_init ret in
          Exception.type_set ex Exception.Type.Failed;    (* todo: other types? *)
          Exception.reason_set ex msg;
          ret
        | `Cancelled ->
          let m = Message.init_root () in
          let ret = Message.return_init m in
          Return.canceled_set ret;
          ret
        | _ -> failwith "TODO: other return type"
    in
    Return.answer_id_set ret (AnswerId.uint32 aid);
    Return.to_message ret

let queue_send ~tags endpoint x =
  let message = serialise ~tags x in
  async_tagged "Transmit message"
    (fun () -> Endpoint.send endpoint message)

let of_endpoint ?offer ?(tags=Logs.Tag.empty) ~switch endpoint =
  let conn = Conn.create ?bootstrap:offer ~tags ~queue_send:(queue_send ~tags endpoint) in
  let t = {
    conn;
    endpoint;
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
