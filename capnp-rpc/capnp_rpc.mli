module S = S
module RO_array = RO_array
module Stats = Stats
module Id = Id
module Debug = Debug
module Error = Error

module Make (C : S.CONCRETE) (N : S.NETWORK_TYPES) : sig
  module Core_types : module type of Core_types.Make(C)
  module Protocol : module type of Protocol.Make(Core_types)(N)
  module Local_struct_promise : module type of Local_struct_promise.Make(Core_types)
  module Cap_proxy : module type of Cap_proxy.Make(Core_types)
  module CapTP : sig
    module Make (P : Protocol.S) : sig
      type t

      val tags : ?qid:P.T.QuestionId.t -> ?aid:P.T.AnswerId.t -> t -> Logs.Tag.set

      val bootstrap : t -> Core_types.cap

      val handle_msg : t -> P.In.t -> unit

      val stats : t -> Stats.t

      val create : ?bootstrap:Core_types.cap -> tags:Logs.Tag.set -> queue_send:(P.Out.t -> unit) -> t

      val dump : t Fmt.t
    end
  end
end
