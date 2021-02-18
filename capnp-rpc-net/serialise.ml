open Capnp_rpc_lwt.Private

module EmbargoId = Capnp_rpc.Message_types.EmbargoId
module Log = Capnp_rpc.Debug.Log
module Builder = Schema.Builder
module RO_array = Capnp_rpc.RO_array

module Make (EP : Capnp_core.ENDPOINT) = struct
  open EP.Table

  let results_of_return ret =
    let open Builder in
    match Return.get ret with
    | Return.Results r -> r
    | _ -> failwith "results_of_return: not results!"

  let write_promised_answer pa (qid, xforms) =
    let open Builder in
    PromisedAnswer.question_id_set pa (QuestionId.uint32 qid);
    let xforms_builder = PromisedAnswer.transform_init pa (List.length xforms) in
    xforms |> List.iteri (fun i (Xform.Field f) ->
        let b = Capnp.Array.get xforms_builder i in
        PromisedAnswer.Op.get_pointer_field_set_exn b f
      )

  let write_cap slot =
    let open Builder in function
      | `ReceiverHosted id -> CapDescriptor.receiver_hosted_set slot (ImportId.uint32 id)
      | `ReceiverAnswer x -> write_promised_answer (CapDescriptor.receiver_answer_init slot) x
      | `SenderHosted id -> CapDescriptor.sender_hosted_set slot (ExportId.uint32 id)
      | `SenderPromise id -> CapDescriptor.sender_promise_set slot (ExportId.uint32 id)
      | `ThirdPartyHosted _ -> failwith "TODO: write_caps_array"
      | `None -> CapDescriptor.none_set slot

  let write_caps_array caps payload =
    let open Builder in
    let builder = Payload.cap_table_init payload (RO_array.length caps) in
    caps |> RO_array.iteri (fun i -> write_cap (Capnp.Array.get builder i))

  let set_target b target =
    let open Builder in
    match target with
    | `ReceiverAnswer (id, i) ->
      let builder = MessageTarget.promised_answer_init b in
      write_promised_answer builder (id, i)
    | `ReceiverHosted id ->
      MessageTarget.imported_cap_set b (ImportId.uint32 id)

  let write_exn b ex =
    let open Builder.Exception in
    let ty =
      match ex.Capnp_rpc.Exception.ty with
      | `Failed        -> Type.Failed
      | `Overloaded    -> Type.Overloaded
      | `Disconnected  -> Type.Disconnected
      | `Unimplemented -> Type.Unimplemented
      | `Undefined x   -> Type.Undefined x
    in
    type_set b ty;
    reason_set b ex.Capnp_rpc.Exception.reason

  let message : EP.Out.t -> _ =
    let open Builder in
    function
    | `Abort ex ->
      let b = Message.init_root () in
      write_exn (Message.abort_init b) ex;
      Message.to_message b
    | `Bootstrap (qid, object_id) ->
      let b = Message.init_root ~message_size:100 () in
      let boot = Message.bootstrap_init b in
      Bootstrap.question_id_set boot (QuestionId.uint32 qid);
      if not (String.equal object_id "") then
        Schema.BuilderOps.write_string (Bootstrap.deprecated_object_id_get boot) object_id;
      Message.to_message b
    | `Call (qid, target, request, descs, results_to) ->
      let c = Msg.Request.writable request in
      Call.question_id_set c (QuestionId.uint32 qid);
      set_target (Call.target_init c) target;
      let p = Call.params_get c in
      write_caps_array descs p;
      let dest = Call.send_results_to_init c in
      begin match results_to with
        | `Caller -> Call.SendResultsTo.caller_set dest
        | `Yourself -> Call.SendResultsTo.yourself_set dest
        | `ThirdParty _ -> failwith "TODO: send_results_to ThirdParty"
      end;
      Call.to_message c
    | `Finish (qid, release_result_caps) ->
      let b = Message.init_root ~message_size:42 () in
      let fin = Message.finish_init b in
      Finish.question_id_set fin (QuestionId.uint32 qid);
      Finish.release_result_caps_set fin release_result_caps;
      Message.to_message b
    | `Release (id, count) ->
      let m = Message.init_root ~message_size:48 () in
      let rel = Message.release_init m in
      Release.id_set rel (ImportId.uint32 id);
      Release.reference_count_set_int_exn rel count;
      Message.to_message m
    | `Disembargo_request disembargo_request ->
      let m = Message.init_root ~message_size:200 () in
      let dis = Message.disembargo_init m in
      let ctx = Disembargo.context_init dis in
      begin match disembargo_request with
        | `Loopback (old_path, embargo_id) ->
          set_target (Disembargo.target_init dis) old_path;
          Disembargo.Context.sender_loopback_set ctx (EmbargoId.uint32 embargo_id)
      end;
      Message.to_message m
    | `Disembargo_reply (target, embargo_id) ->
      let m = Message.init_root ~message_size:200 () in
      let dis = Message.disembargo_init m in
      let ctx = Disembargo.context_init dis in
      set_target (Disembargo.target_init dis) target;
      Disembargo.Context.receiver_loopback_set ctx (EmbargoId.uint32 embargo_id);
      Message.to_message m
    | `Return (aid, return, release) ->
      let ret =
        match return with
        | `Results (msg, descs) ->
          (* [msg] has payload filled in, but nothing else. *)
          let ret = Msg.Response.writable msg in
          write_caps_array descs (results_of_return ret);
          ret
        | `Exception ex ->
          let m = Message.init_root () in
          let ret = Message.return_init m in
          write_exn (Return.exception_init ret) ex;
          ret
        | `Cancelled ->
          let m = Message.init_root ~message_size:200 () in
          let ret = Message.return_init m in
          Return.canceled_set ret;
          ret
        | `ResultsSentElsewhere ->
          let m = Message.init_root ~message_size:200 () in
          let ret = Message.return_init m in
          Return.results_sent_elsewhere_set ret;
          ret
        | `TakeFromOtherQuestion qid ->
          let m = Message.init_root ~message_size:200 () in
          let ret = Message.return_init m in
          Return.take_from_other_question_set ret (QuestionId.uint32 qid);
          ret
        | `AcceptFromThirdParty ->
          failwith "TODO: AcceptFromThirdParty"
      in
      Return.answer_id_set ret (AnswerId.uint32 aid);
      Return.release_param_caps_set ret release;
      Return.to_message ret
    | `Resolve (id, new_target) ->
      let m = Message.init_root () in
      let r = Message.resolve_init m in
      begin match new_target with
        | Ok cap -> write_cap (Resolve.cap_init r) cap
        | Error e -> write_exn (Resolve.exception_init r) e
      end;
      Resolve.promise_id_set r (ExportId.uint32 id);
      Message.to_message m
end
