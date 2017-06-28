(** Provides the RPC layer on top of [Endpoint]. *)

open Capnp_core.Core_types

type t

val of_endpoint : ?offer:cap -> ?tags:Logs.Tag.set -> switch:Lwt_switch.t -> Endpoint.t -> t
(** [of_endpoint ?offer ~switch endpoint] is fresh CapTP state for communicating with [endpoint].
    If [offer] is given, the peer can use the "Bootstrap" message to get access to it.
    If the connection fails then [switch] will be turned off. *)

val bootstrap : t -> cap
(** [bootstrap t] is the peer's public bootstrap object, if any. *)

val dump : t Fmt.t
(** [dump] dumps the state of the connection, for debugging. *)
