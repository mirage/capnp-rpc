(* This module defines the information in the messages that goes over the wire.
   These messages are turned into actual byte streams elsewhere.
*)

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
  (* [Flip T] is the types for the other end of [T]'s connection. *)

  module QuestionId = T.AnswerId
  module AnswerId = T.QuestionId
  module ImportId = T.ExportId
  module ExportId = T.ImportId
end

module Make (Network : S.NETWORK_TYPES) (T : TABLE_TYPES) = struct
  (* This module defines the information in the messages that goes over the wire in one direction.
     The types are from the point of view of the sender, as in the Cap'n Proto RPC specification. *)

  open Network
  open Network.Wire

  include T

  type third_party_desc = third_party_cap_id * ExportId.t

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

  type sendResultsTo = [
    | `Caller
    | `Yourself
    | `ThirdParty of recipient_id
  ]

  type return = [
    | Error.t
    | `Results of Response.t * desc RO_array.t
    | `ResultsSentElsewhere
    | `TakeFromOtherQuestion
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
    | `TakeFromOtherQuestion -> Fmt.pf f "TakeFromOtherQuestion"
    | `AcceptFromThirdParty -> Fmt.pf f "AcceptFromThirdParty"

  let pp_disembargo_request : disembargo_request Fmt.t = fun f -> function
    | `Loopback (old_path, id) -> Fmt.pf f "senderLoopback for %a (embargo_id = %a)"
                                    pp_desc old_path
                                    EmbargoId.pp id

  type t = [
    | `Bootstrap of QuestionId.t
    | `Call of QuestionId.t * message_target * Request.t * desc RO_array.t
    | `Finish of (QuestionId.t * bool)      (* bool is release-caps *)
    | `Release of ImportId.t * int
    | `Disembargo_request of disembargo_request
    | `Disembargo_reply of message_target * EmbargoId.t
    | `Return of (AnswerId.t * return)
    | `Resolve of (ExportId.t * (desc, Exception.t) result)
  ]
  (** A message sent over the network. *)
end

module type ENDPOINT = sig
  module Core_types : S.NETWORK_TYPES
  module Table : TABLE_TYPES

  module Out : module type of Make(Core_types)(Table)
  (** The type of messages sent by this endpoint. *)

  module In : module type of Make(Core_types)(Flip(Table))
  (** The type of messages received by this endpoint. *)
end

module Endpoint (Core_types : S.NETWORK_TYPES) ( ) = struct
  module Core_types = Core_types

  module Table = struct
    module QuestionId = Id.Make ( )
    module AnswerId = Id.Make ( )
    module ImportId = Id.Make ( )
    module ExportId = Id.Make ( )
  end

  module Out = Make(Core_types)(Table)
  module In = Make(Core_types)(Flip(Table))
end
