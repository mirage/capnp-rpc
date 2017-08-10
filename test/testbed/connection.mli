open Capnp_direct.Core_types
open Capnp_direct.String_content

val summary_of_msg :
  [< `Bootstrap of _
  | `Abort of _
  | `Call of _ * _ * Request.t * _ * _
  | `Disembargo_reply of _
  | `Disembargo_request of _
  | `Finish of _
  | `Release of _
  | `Resolve of _
  | `Return of
       _ *
       [< `AcceptFromThirdParty
       | `Cancelled
       | `Exception of Capnp_rpc.Exception.t
       | `Results of Response.t * _
       | `ResultsSentElsewhere
       | `TakeFromOtherQuestion of _] *
       _
  | `Unimplemented of _ ] ->
  string

module type ENDPOINT = sig
  type t

  module EP : Capnp_direct.ENDPOINT

  val dump : t Fmt.t

  val create : ?bootstrap:#cap -> tags:Logs.Tag.set ->
    [EP.Out.t | `Unimplemented of EP.In.t] Queue.t ->
    [EP.In.t | `Unimplemented of EP.Out.t] Queue.t ->
    t

  val handle_msg : ?expect:string -> t -> unit

  val maybe_handle_msg : t -> unit

  val step : t -> bool

  val bootstrap : t -> cap

  val stats : t -> Capnp_rpc.Stats.t

  val check_invariants : t -> unit

  val check_finished : t -> name:string -> unit

  val disconnect : t -> Capnp_rpc.Exception.t -> unit
end

module Endpoint (EP : Capnp_direct.ENDPOINT) : ENDPOINT

module Pair ( ) : sig
  module C : ENDPOINT
  module S : ENDPOINT

  val create : client_tags:Logs.Tag.set -> server_tags:Logs.Tag.set -> ?client_bs:#cap -> #cap -> C.t * S.t

  val flush : C.t -> S.t -> unit

  val dump : C.t -> S.t -> unit

  val check_finished : C.t -> S.t -> unit
end

