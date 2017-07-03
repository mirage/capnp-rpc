(** Provides the RPC layer on top of [Endpoint]. *)

open Capnp_core.Core_types

type t
(** A Cap'n Proto RPC protocol handler. *)

val connect : ?offer:cap -> ?tags:Logs.Tag.set -> switch:Lwt_switch.t -> Endpoint.t -> t
(** [connect ?offer ~switch endpoint] is fresh CapTP protocol handler that sends and
    receives messages using [endpoint].
    If [offer] is given, the peer can use the "Bootstrap" message to get access to it.
    If the connection fails then [switch] will be turned off, and turning off the switch
    will release all resources used by the connection. *)

val bootstrap : t -> cap
(** [bootstrap t] is the peer's public bootstrap object, if any. *)

val dump : t Fmt.t
(** [dump] dumps the state of the connection, for debugging. *)
