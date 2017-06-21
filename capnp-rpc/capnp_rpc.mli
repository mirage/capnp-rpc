module S = S
module RO_array = RO_array
module Stats = Stats
module Id = Id
module Debug = Debug
module Error = Error
module EmbargoId = Protocol.EmbargoId
module Message_types = Message_types
module Core_types (W : S.WIRE) : S.CORE_TYPES with module Wire = W
module Local_struct_promise = Local_struct_promise
module Cap_proxy = Cap_proxy

module CapTP : sig
  module Make (EP : Message_types.ENDPOINT) : sig
    type t

    module P : module type of Protocol.Make(EP)

    val tags : ?qid:EP.Out.QuestionId.t -> ?aid:EP.Out.AnswerId.t -> t -> Logs.Tag.set

    val bootstrap : t -> EP.Core_types.cap

    val handle_msg : t -> EP.In.t -> unit

    val stats : t -> Stats.t

    val create : ?bootstrap:EP.Core_types.cap -> tags:Logs.Tag.set -> queue_send:(EP.Out.t -> unit) -> t

    val dump : t Fmt.t
  end
end
