(** This module defines the information in the messages that goes over the wire.
    These messages are turned into actual byte streams elsewhere. *)

module EmbargoId = Id.Make ( )

module type TABLE_TYPES = sig
  (** For the unit tests it is convenient to pass in the types of table indexes.
      This allows the tests to make both ends of a connection, with the types
      matched up. *)

  module QuestionId : Id.S
  module AnswerId : Id.S
  module ImportId : Id.S
  module ExportId : Id.S
end

module Flip (T : TABLE_TYPES) = struct
  (** [Flip T] is the types for the other end of [T]'s connection. *)

  module QuestionId = T.AnswerId
  module AnswerId = T.QuestionId
  module ImportId = T.ExportId
  module ExportId = T.ImportId
end

module Make (Core_types : S.CORE_TYPES) (Network : S.NETWORK_TYPES) (T : TABLE_TYPES) = struct
  (** This module defines the information in the messages that goes over the wire in one direction.
      The types are from the point of view of the sender, as in the Cap'n Proto RPC specification. *)

  open Core_types
  open Core_types.Wire

  include T

  type third_party_desc = Network.third_party_cap_id * ExportId.t

  type message_target = [
    | `ReceiverHosted of ImportId.t
    | `ReceiverAnswer of QuestionId.t * Wire.Path.t
  ]

  type desc = [
    message_target
    | `None
    | `SenderHosted of ExportId.t
    | `SenderPromise of ExportId.t
    | `ThirdPartyHosted of third_party_desc
  ]

  let pp_desc f = function
    | `None                  -> Fmt.pf f "None"
    | `Local local           -> Fmt.pf f "Local:%t" local#pp
    | `ReceiverAnswer (x, p) -> Fmt.pf f "ReceiverAnswer:%a:%a" QuestionId.pp x Path.pp p
    | `ReceiverHosted x      -> Fmt.pf f "ReceiverHosted:%a" ImportId.pp x
    | `ThirdPartyHosted _    -> Fmt.pf f "ThirdPartyHosted"
    | `SenderHosted x        -> Fmt.pf f "SenderHosted:%a" ExportId.pp x
    | `SenderPromise x       -> Fmt.pf f "SenderPromise:%a" ExportId.pp x

  type send_results_to = [
    | `Caller
    | `Yourself
    | `ThirdParty of Network.recipient_id
  ]

  type return = [
    | Error.t
    | `Results of Response.t * desc RO_array.t
    | `ResultsSentElsewhere
    | `TakeFromOtherQuestion of QuestionId.t
    | `AcceptFromThirdParty
  ]

  type disembargo_request = [
    | `Loopback of message_target * EmbargoId.t
  ]

  let pp_return f = function
    | `Results (_, descrs) -> Fmt.pf f "Results:%a" (RO_array.pp pp_desc) descrs
    | `Exception ex -> Fmt.pf f "Exception:%a" Exception.pp ex
    | `Cancelled -> Fmt.pf f "Cancelled"
    | `ResultsSentElsewhere -> Fmt.pf f "ResultsSentElsewhere"
    | `TakeFromOtherQuestion qid -> Fmt.pf f "TakeFromSenderQuestion(%a)" QuestionId.pp qid
    | `AcceptFromThirdParty -> Fmt.pf f "AcceptFromThirdParty"

  let pp_disembargo_request : disembargo_request Fmt.t = fun f -> function
    | `Loopback (old_path, id) -> Fmt.pf f "senderLoopback request for %a (embargo_id = %a)"
                                    pp_desc old_path
                                    EmbargoId.pp id

  type t = [
    | `Abort of Exception.t
    | `Bootstrap of QuestionId.t * string
    | `Call of QuestionId.t * message_target * Request.t * desc RO_array.t * send_results_to
    | `Finish of (QuestionId.t * bool)      (* bool is release-caps *)
    | `Release of ImportId.t * int
    | `Disembargo_request of disembargo_request
    | `Disembargo_reply of message_target * EmbargoId.t
    | `Return of (AnswerId.t * return * bool)   (* bool is release-caps *)
    | `Resolve of (ExportId.t * (desc, Exception.t) result)
  ]
  (** A message sent over the network. *)

  let with_qid_tag tags : t -> _ = function
    | `Finish (qid, _)
    | `Bootstrap (qid, _)
    | `Call (qid, _, _, _, _)
    | `Disembargo_request (`Loopback (`ReceiverAnswer (qid, _), _))
    | `Disembargo_reply (`ReceiverAnswer (qid, _), _) ->
      Logs.Tag.add Debug.qid_tag (QuestionId.uint32 qid) tags
    | `Return (aid, _, _) ->
      Logs.Tag.add Debug.qid_tag (AnswerId.uint32 aid) tags
    | `Abort _
    | `Disembargo_reply _
    | `Disembargo_request _
    | `Release _
    | `Resolve _ ->
      tags

  let pp_results_to f = function
    | `Caller -> ()
    | `Yourself -> Fmt.pf f " (results to yourself)"
    | `ThirdParty _ -> Fmt.pf f " (results to third party)"

  (** Describe message from the point of view of the receiver. *)
  let pp_recv pp_msg : t Fmt.t = fun f -> function
    | `Abort ex -> Fmt.pf f "Abort(%a)" Exception.pp ex
    | `Bootstrap _ -> Fmt.pf f "Bootstrap"
    | `Call (_, target, msg, caps, results_to) -> Fmt.pf f "Call %a.%a with %a%a"
                                        pp_desc target
                                        pp_msg msg
                                        (RO_array.pp pp_desc) caps
                                        pp_results_to results_to
    | `Finish (_, release) -> Fmt.pf f "Finish (release_result_caps=%b)" release
    | `Release (id, count) -> Fmt.pf f "Release export %a (count=%d)" ImportId.pp id count
    | `Disembargo_request disembargo_request -> pp_disembargo_request f disembargo_request
    | `Disembargo_reply (x, id) -> Fmt.pf f "Disembargo reply for %a (embargo ID = %a)" pp_desc x EmbargoId.pp id
    | `Return (_, return, release) -> Fmt.pf f "Return %a (release_param_caps = %b)" pp_return return release
    | `Resolve (id, Ok desc) -> Fmt.pf f "Resolve export %a -> %a" ExportId.pp id pp_desc desc
    | `Resolve (id, Error e) -> Fmt.pf f "Resolve export %a -> %a" ExportId.pp id Exception.pp e
end

module type ENDPOINT = sig
  module Core_types : S.CORE_TYPES
  module Network_types : S.NETWORK_TYPES
  module Table : TABLE_TYPES

  module Out : module type of Make(Core_types)(Network_types)(Table)
  (** The type of messages sent by this endpoint. *)

  module In : module type of Make(Core_types)(Network_types)(Flip(Table))
  (** The type of messages received by this endpoint. *)
end

module Table_types ( ) = struct
  module QuestionId = Id.Make ( )
  module AnswerId = Id.Make ( )
  module ImportId = Id.Make ( )
  module ExportId = Id.Make ( )
end

module Endpoint (Core_types : S.CORE_TYPES) (Network_types : S.NETWORK_TYPES) (Table : TABLE_TYPES) = struct
  module Core_types = Core_types
  module Network_types = Network_types
  module Table = Table

  module Out = Make(Core_types)(Network_types)(Table)
  module In = Make(Core_types)(Network_types)(Flip(Table))
end
