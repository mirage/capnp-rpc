open Capnp_rpc_lwt

module EmbargoId = Capnp_rpc.Message_types.EmbargoId
module RO_array = Capnp_rpc.RO_array
module Reader = Private.Schema.Reader
module Log = Capnp_rpc.Debug.Log

(* A parser for the basic messages (excluding Unimplemented, which has a more complicated type). *)
module Make_basic
    (Core_types : Capnp_rpc.S.CORE_TYPES)
    (Network : S.NETWORK)
    (T : Capnp_rpc.Message_types.TABLE_TYPES) = struct
  module Message_types = Capnp_rpc.Message_types.Make(Core_types)(Network.Types)(T)
  open Message_types

  let parse_xform x =
    let open Reader.PromisedAnswer.Op in
    match get x with
    | Noop -> []
    | GetPointerField y -> [Private.Xform.Field y]
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
      let cap_id = ThirdPartyCapDescriptor.id_get tp in
      (* todo: for level 3, we should establish a direct connection rather than proxying
         through the vine *)
      `ThirdPartyHosted (Network.parse_third_party_cap_id cap_id, vine_id)
    | CapDescriptor.Undefined _ -> failwith "Unknown cap descriptor type"

  let parse_descs = RO_array.map parse_desc

  let parse_exn ex =
    let open Reader.Exception in
    let reason = reason_get ex in
    let ty =
      match type_get ex with
      | Failed        -> `Failed
      | Overloaded    -> `Overloaded
      | Disconnected  -> `Disconnected
      | Unimplemented -> `Unimplemented
      | Undefined x   -> `Undefined x
    in
    { Capnp_rpc.Exception.ty; reason }

  let parse_return return =
    let open Reader in
    let qid = Return.answer_id_get return |> QuestionId.of_uint32 in
    let release_param_caps = Return.release_param_caps_get return in
    let ret =
      match Return.get return with
      | Return.Results results ->
        let descs = parse_descs (Payload.cap_table_get_list results |> RO_array.of_list) in
        `Results (Private.Msg.Response.of_reader return, descs)
      | Return.Exception ex -> `Exception (parse_exn ex)
      | Return.Canceled -> `Cancelled
      | Return.ResultsSentElsewhere -> `ResultsSentElsewhere
      | Return.TakeFromOtherQuestion other -> `TakeFromOtherQuestion (AnswerId.of_uint32 other)
      | Return.AcceptFromThirdParty _ -> failwith "TODO: AcceptFromThirdParty"
      | Return.Undefined x -> failwith (Fmt.str "Unexpected return type received: %d" x)
    in
    `Return (qid, ret, release_param_caps)

  let parse_finish finish =
    let open Reader in
    let aid = Finish.question_id_get finish |> AnswerId.of_uint32 in
    let release = Finish.release_result_caps_get finish in
    `Finish (aid, release)

  let parse_target msg_target =
    let open Reader in
    match MessageTarget.get msg_target with
    | MessageTarget.Undefined _ -> failwith "Bad MessageTarget"
    | MessageTarget.ImportedCap id -> `ReceiverHosted (ExportId.of_uint32 id)
    | MessageTarget.PromisedAnswer x -> parse_promised_answer x

  (* We have received a question from our peer. *)
  let parse_call call =
    let open Reader in
    let aid = Call.question_id_get call |> AnswerId.of_uint32 in
    (* Resolve capabilities *)
    let p = Call.params_get call in
    let descs = parse_descs (Payload.cap_table_get_list p |> RO_array.of_list) in
    (* Get target *)
    let target = parse_target (Call.target_get call) in
    let msg = Private.Msg.Request.of_reader call in
    let results_to =
      let r = Call.send_results_to_get call in
      let open Call.SendResultsTo in
      match get r with
      | Caller -> `Caller
      | Yourself -> `Yourself
      | ThirdParty _ -> failwith "TODO: parse_call: ThirdParty"
      | Undefined x -> Capnp_rpc.Debug.failf "Unknown SendResultsTo type %d" x
    in
    `Call (aid, target, msg, descs, results_to)

  let parse_bootstrap boot =
    let open Reader in
    let qid = Bootstrap.question_id_get boot |> AnswerId.of_uint32 in
    let object_id = Bootstrap.deprecated_object_id_get boot |> Private.Schema.ReaderOps.string_of_pointer in
    `Bootstrap (qid, object_id)

  let parse_disembargo x =
    let open Reader in
    let target = parse_target (Disembargo.target_get x) in
    let ctx = Disembargo.context_get x in
    match Disembargo.Context.get ctx with
    | Disembargo.Context.SenderLoopback embargo_id -> `Disembargo_request (`Loopback (target, EmbargoId.of_uint32 embargo_id))
    | Disembargo.Context.ReceiverLoopback embargo_id -> `Disembargo_reply (target, EmbargoId.of_uint32 embargo_id)
    | Disembargo.Context.Accept
    | Disembargo.Context.Provide _ -> failwith "TODO: handle_disembargo: 3rd-party"
    | Disembargo.Context.Undefined x -> Capnp_rpc.Debug.failf "Unknown Disembargo type %d" x

  let parse_resolve x =
    let open Reader in
    let new_target =
      match Resolve.get x with
      | Resolve.Cap d -> Ok (parse_desc d)
      | Resolve.Exception e -> Error (parse_exn e)
      | Resolve.Undefined x -> Capnp_rpc.Debug.failf "Resolved to Undefined(%d)!" x
    in
    let import_id = Resolve.promise_id_get x |> ImportId.of_uint32 in
    `Resolve (import_id, new_target)

  let parse_release x =
    let open Reader in
    let export_id = Release.id_get x |> ExportId.of_uint32 in
    let ref_count = Release.reference_count_get x |> Stdint.Uint32.to_int in
    `Release (export_id, ref_count)

  (* Parse a message received from our peer. Returns [`Not_implemented`] if we couldn't understand it. *)
  let parse_msg msg =
    let open Reader.Message in
    match get msg with
    | Call x           -> parse_call x
    | Bootstrap x      -> parse_bootstrap x
    | Return x         -> parse_return x
    | Finish x         -> parse_finish x
    | Disembargo x     -> parse_disembargo x
    | Resolve x        -> parse_resolve x
    | Release x        -> parse_release x
    | Abort x          -> `Abort (parse_exn x)
    | Provide _
    | Accept _
    | Join _           -> `Not_implemented        (* TODO *)
    | ObsoleteSave _
    | ObsoleteDelete _ -> `Not_implemented
    | Undefined x      ->
      Log.warn (fun f -> f "Received Undefined message (%d)!" x);
      `Not_implemented
    | Unimplemented x  -> `Unimplemented x
end

module Make
    (EP : Private.Capnp_core.ENDPOINT)
    (Network : S.NETWORK with module Types = EP.Network_types)
= struct
  module Parse_in = Make_basic(EP.Core_types)(Network)(EP.Table)
  module Parse_out = Make_basic(EP.Core_types)(Network)(Capnp_rpc.Message_types.Flip(EP.Table))

  let message msg =
    match Parse_in.parse_msg msg with
    | #EP.In.t as msg -> msg
    | `Not_implemented -> `Not_implemented        (* We don't understand [msg] *)
    | `Unimplemented x ->                         (* The remote peer didn't understand [x] *)
      match Parse_out.parse_msg x with
      | #EP.Out.t as msg -> `Unimplemented msg
      | `Not_implemented -> failwith "Can't read copy of our own message in Unimplemented reply!"
      | `Unimplemented _ -> failwith "Peer doesn't implement support for unimplemented message!"
end
