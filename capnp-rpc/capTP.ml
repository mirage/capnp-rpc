module Log = Debug.Log

let failf msg = Fmt.kstrf failwith msg

module EmbargoId = Message_types.EmbargoId

module Make (EP : Message_types.ENDPOINT) = struct
  module Core_types = EP.Core_types
  module Wire = Core_types.Wire
  module P = Protocol.Make(EP)
  module Cap_proxy = Cap_proxy.Make(Core_types)
  module Struct_proxy = Struct_proxy.Make(Core_types)
  module Local_struct_promise = Local_struct_promise.Make(Core_types)

  type t = {
    queue_send : (EP.Out.t -> unit);
    p : P.t;
    ours : (Core_types.cap, P.message_target_cap) Hashtbl.t;              (* TODO: use weak table *)
    tags : Logs.Tag.set;
    embargoes : (EmbargoId.t, Cap_proxy.embargo_cap) Hashtbl.t;
    bootstrap : Core_types.cap option;
  }

  let create ?bootstrap ~tags ~queue_send =
    {
      queue_send;
      p = P.create ~tags ();
      ours = Hashtbl.create 10;
      tags;
      embargoes = Hashtbl.create 10;
      bootstrap;
    }

  let tags ?qid ?aid t =
    match qid, aid with
    | None, None -> t.tags
    | Some qid, None -> Logs.Tag.add Debug.qid_tag (EP.Table.QuestionId.uint32 qid) t.tags
    | None, Some aid -> Logs.Tag.add Debug.qid_tag (EP.Table.AnswerId.uint32 aid) t.tags
    | Some _, Some _ -> assert false

  let stats t = P.stats t.p

  let register t x y =
    match Hashtbl.find t.ours x with
    | exception Not_found -> Hashtbl.add t.ours x y
    | existing -> assert (y = existing)

  let unwrap t x =
    try Some (Hashtbl.find t.ours x)
    with Not_found -> None

  module Self_proxy = struct
    let to_cap_desc t (cap : Core_types.cap) =
      let cap = cap#shortest in
      match unwrap t cap with
      | None -> `Local cap
      | Some x -> (x :> [`Local of Core_types.cap | P.message_target_cap])

    type target = (P.question * unit Lazy.t) option  (* question, finish *)

    (* We've just converted [caps] to [con_caps] and transmitted them.
       [dec_ref] each [cap], unless we need to keep it around so the peer can refer
       to it later. *)
    let release_remote_caps caps con_caps =
      con_caps |> RO_array.iteri (fun i -> function
          | `ReceiverHosted _
          | `ReceiverAnswer _ -> (RO_array.get caps i)#dec_ref
          | `Local _ -> ()
        )

    let pp_promise f = function
      | Some (q, _) -> P.pp_question f q
      | None -> Fmt.string f "(not initialised)"

    let rec call t target msg caps =
      let result = make_remote_promise t in
      let con_caps = RO_array.map (to_cap_desc t) caps in
      let question, qid, message_target, descs = P.Send.call t.p (result :> Core_types.struct_resolver) target con_caps in
      Log.info (fun f -> f ~tags:(tags ~qid t) "Sending: (%a).call %a"
                   P.pp_cap target
                   Core_types.Request_payload.pp (msg, caps));
      result#set_question question;
      t.queue_send (`Call (qid, message_target, msg, descs));
      release_remote_caps caps con_caps;
      (result :> Core_types.struct_ref)

    (* A cap that sends to a promised answer's cap at other *)
    and make_remote_promise t =
      object (self : #Core_types.struct_resolver)
        inherit [target] Struct_proxy.t None as super

        method do_pipeline question i msg caps =
          match question with
          | Some (target_q, _) ->
            let target = `ReceiverAnswer (target_q, i) in
            call t target msg caps
          | None -> failwith "Not initialised!"

        method on_resolve q _ =
          match q with
          | Some (_target_q, finish) -> Lazy.force finish
          | None -> failwith "Not initialised!"

        method! pp f =
          Fmt.pf f "remote-promise -> %a" (Struct_proxy.pp_state ~pp_promise) state

        method set_question q =
          let finish = lazy (
            let qid = P.Send.finish t.p q in
            Log.info (fun f -> f ~tags:(tags ~qid t) "Send finish %t" self#pp);
            t.queue_send (`Finish (qid, false));
          ) in
          self#update_target (Some (q, finish))

        method! cap path =
          let field = super#cap path in
          begin match state with
            | Unresolved u ->
              begin match u.target with
                | None -> failwith "Not intialised!"
                | Some (target_q, _) ->
                  register t field (`ReceiverAnswer (target_q, path));        (* TODO: unregister *)
              end
            | _ -> ()
          end;
          field

        method do_finish = function
          | Some (_, finish) -> Lazy.force finish
          | None -> failwith "Not initialised!"
      end

    (* Turn a connection-scoped cap reference received from Other into a general-purpose
       cap for users. If the resulting cap is remote, our wrapper forwards it to Other.
       This will add a ref count to the cap if it already exists, or create a new
       one with [ref_count = 1]. *)
    let from_cap_desc t (desc:P.recv_descr) : Core_types.cap =
      match desc with
      | `Local c -> c#inc_ref; c
      | `ReceiverHosted import as message_target ->
        P.import_proxy import
          ~inc_ref:(fun c -> c#inc_ref)
          ~create:(fun () ->
              let cap =
                object (self : #Core_types.cap)
                  inherit Core_types.ref_counted

                  method call msg caps = call t message_target msg caps
                  method pp f = Fmt.pf f "far-ref(rc=%d) -> %a" ref_count P.pp_cap message_target
                  method private release =
                    Log.info (fun f -> f ~tags:t.tags "Sending release %t" self#pp);
                    let id, count = P.Send.release t.p import in
                    t.queue_send (`Release (id, count))

                  method shortest = self
                  method blocker = None   (* Can't detect cycles over the network *)
                end
              in
              register t cap message_target;
              cap
            )
      | `None -> Core_types.null
      | `ReceiverAnswer _ -> failwith "TODO: from_cap_desc ReceiverAnswer"
      | `ThirdPartyHosted _ -> failwith "TODO: from_cap_desc ThirdPartyHosted"
      | `LocalPromise (p, i) -> p#cap i

    let reply_to_disembargo t target embargo_id =
      let target = P.Send.disembargo_reply t.p target in
      Log.info (fun f -> f ~tags:t.tags "Sending disembargo response to %a" EP.Out.pp_desc target);
      t.queue_send (`Disembargo_reply (target, embargo_id))

    let disembargo t request =
      Log.info (fun f -> f ~tags:t.tags "Sending disembargo %a" EP.Out.pp_disembargo_request request);
      t.queue_send (`Disembargo_request request);
  end

  let bootstrap t =
    let result = Self_proxy.make_remote_promise t in
    let question, qid = P.Send.bootstrap t.p (result :> Core_types.struct_resolver) in
    result#set_question question;
    Log.info (fun f -> f ~tags:(tags ~qid t) "Sending: bootstrap");
    t.queue_send (`Bootstrap qid);
    let service = result#cap Wire.Path.root in
    result#finish;
    service

  let return_results t answer =
    let aid, ret =
      let answer_promise = P.answer_promise answer in
      match answer_promise#response with
      | None -> assert false
      | Some (Ok (msg, caps)) ->
        RO_array.iter (fun c -> c#inc_ref) caps;        (* Copy everything stored in [answer]. *)
        let con_caps = RO_array.map (Self_proxy.to_cap_desc t) caps in
        let aid, ret = P.Send.return_results t.p answer msg con_caps in
        Log.info (fun f -> f ~tags:(tags ~aid t) "Returning results: %a"
                     Core_types.Response_payload.pp (msg, caps));
        Self_proxy.release_remote_caps caps con_caps;
        aid, ret
      | Some (Error (`Exception msg)) ->
        let aid, ret = P.Send.return_error t.p answer msg in
        Log.info (fun f -> f ~tags:(tags ~aid t) "Returning error: %s" msg);
        aid, ret
      | Some (Error `Cancelled) ->
        let aid, ret = P.Send.return_cancelled t.p answer in
        Log.info (fun f -> f ~tags:(tags ~aid t) "Returning cancelled");
        aid, ret
    in
    t.queue_send (`Return (aid, ret))

  let reply_to_call t = function
    | `Bootstrap answer ->
      let promise = P.answer_promise answer in
      begin match t.bootstrap with
        | Some service ->
          service#inc_ref;
          promise#resolve (Ok (Wire.Response.bootstrap, RO_array.of_list [service]));
        | None ->
          promise#resolve (Error (`Exception "No bootstrap service available"));
      end;
      return_results t answer
    | `Call (answer, target, msg, caps) ->
      Log.info (fun f -> f ~tags:t.tags "Handling call: (%t).call %a" target#pp Core_types.Request_payload.pp (msg, caps));
      let resp = target#call msg caps in  (* Takes ownership of [caps]. *)
      target#dec_ref;
      (P.answer_promise answer)#connect resp;
      resp#when_resolved (fun _ -> return_results t answer)

  let handle_msg t = function
    | `Bootstrap qid ->
       let promise = Local_struct_promise.make () in
       let answer = P.Input.bootstrap t.p qid ~answer:promise in
       reply_to_call t (`Bootstrap answer)
    | `Call (aid, message_target, msg, descs) ->
      Log.info (fun f -> f ~tags:(tags ~aid t) "Received call to %a" EP.In.pp_desc message_target);
      let promise = Local_struct_promise.make () in
      let answer, target, caps = P.Input.call t.p aid message_target descs ~allowThirdPartyTailCall:false `Caller ~answer:promise in
      let target = Self_proxy.from_cap_desc t target in
      let caps = RO_array.map (Self_proxy.from_cap_desc t) caps in
      reply_to_call t (`Call (answer, target, msg, caps))
    | `Return (qid, ret) ->
       begin match ret with
       | `Results (msg, descs) ->
         let result, caps = P.Input.return_results t.p qid msg descs ~releaseParamCaps:false in
         let from_cap_desc = function
           | `LocalEmbargo (c, disembargo_request) ->
             c#inc_ref;
             Log.info (fun f -> f ~tags:t.tags "Embargo %t until %a is delivered"
                          c#pp
                          EP.Out.pp_disembargo_request disembargo_request
                      );
             (* We previously pipelined messages to [qid, index], which now turns out to be
                local service [c]. We need to send a disembargo to clear the pipeline before
                using [c]. *)
             let embargo = Cap_proxy.embargo c in
             let `Loopback (_target, embargo_id) = disembargo_request in
             Hashtbl.add t.embargoes embargo_id embargo;
             Self_proxy.disembargo t disembargo_request;
             (embargo :> Core_types.cap)
           | #P.recv_descr as x -> Self_proxy.from_cap_desc t x
         in
         let caps = RO_array.map from_cap_desc caps in
         Log.info (fun f -> f ~tags:(tags ~qid t) "Got results: %a"
                      Core_types.Response_payload.pp (msg, caps)
                  );
         result#resolve (Ok (msg, caps))
       | `Exception msg ->
         let result = P.Input.return_exception t.p qid ~releaseParamCaps:false in
         Log.info (fun f -> f ~tags:(tags ~qid t) "Got exception: %s" msg);
         result#resolve (Error (`Exception msg))
       | `Cancelled ->
         let result = P.Input.return_cancelled t.p qid ~releaseParamCaps:false in
         Log.info (fun f -> f ~tags:(tags ~qid t) "Got cancelled");
         result#resolve (Error `Cancelled)
       | _ -> failwith "TODO: other return"
       end
    | `Finish (aid, releaseResultCaps) ->
      let answer = P.Input.finish t.p aid ~releaseResultCaps in
      Log.info (fun f -> f ~tags:(tags ~aid t) "Received finish for %t" answer#pp);
      answer#finish
    | `Release (id, referenceCount) ->
      P.Input.release t.p id ~referenceCount
    | `Disembargo_request request ->
      begin
        Log.info (fun f -> f ~tags:t.tags "Received disembargo %a" EP.In.pp_disembargo_request request);
        match P.Input.disembargo_request t.p request with
        | `ReturnToSender ((answer_promise, path), id) ->
          match answer_promise#response with
          | None -> failwith "Got disembargo for unresolved promise!"
          | Some (Error _) -> failwith "Got disembargo for exception!"
          | Some (Ok payload) ->
            let cap = Core_types.Response_payload.field payload path in
            match unwrap t cap with
            | Some (`ReceiverHosted _ | `ReceiverAnswer _ as target) -> Self_proxy.reply_to_disembargo t target id
            | None -> failwith "Protocol error: disembargo for invalid target"
      end
    | `Disembargo_reply (target, embargo_id) ->
      P.Input.disembargo_reply t.p target embargo_id;
      let embargo =
        try Hashtbl.find t.embargoes embargo_id
        with Not_found -> failf "Unexpected disembargo ID %a" EmbargoId.pp embargo_id
      in
      Hashtbl.remove t.embargoes embargo_id;
      Log.info (fun f -> f ~tags:t.tags "Received disembargo response %a -> %t"
                   EP.In.pp_desc target
                   embargo#pp);
      embargo#disembargo

  let pp_embargoes f xs =
    let pp_item f (id, proxy) =
      Fmt.pf f "%a: @[%t@]" EmbargoId.pp id proxy#pp
    in
    let add k v acc = (k, v) :: acc in
    let items = Hashtbl.fold add xs [] in
    let items = List.sort compare items in
    (Fmt.Dump.list pp_item) f items

  let dump f t =
    Fmt.pf f "@[<v 2>CapTP state:@,%a@,@[<2>Embargos:@,%a@]@]" P.dump t.p pp_embargoes t.embargoes
end
