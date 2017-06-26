open Capnp_direct.Core_types

module type ENDPOINT = sig
  type t

  module EP : Capnp_direct.ENDPOINT

  val dump : t Fmt.t

  val create : ?bootstrap:#cap -> tags:Logs.Tag.set -> EP.Out.t Queue.t -> EP.In.t Queue.t -> t

  val handle_msg : ?expect:string -> t -> unit

  val maybe_handle_msg : t -> unit

  val step : t -> bool

  val bootstrap : t -> cap

  val stats : t -> Capnp_rpc.Stats.t
end

module Endpoint (EP : Capnp_direct.ENDPOINT) : ENDPOINT

module Pair ( ) : sig
  module C : ENDPOINT
  module S : ENDPOINT

  val create : client_tags:Logs.Tag.set -> server_tags:Logs.Tag.set -> #cap -> C.t * S.t

  val flush : C.t -> S.t -> unit

  val dump : C.t -> S.t -> unit

  val check_finished : C.t -> S.t -> unit
end

