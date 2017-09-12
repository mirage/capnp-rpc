(** Provides the RPC layer on top of some network. *)

open Capnp_core.Core_types

module Make (N : S.NETWORK) : sig
  type t
  (** A Cap'n Proto RPC protocol handler. *)

  val connect : restore:Restorer.t -> ?tags:Logs.Tag.set -> Endpoint.t -> t
  (** [connect ~restore ~switch endpoint] is fresh CapTP protocol handler that sends and
      receives messages using [endpoint].
      [restore] is used to respond to "Bootstrap" messages.
      If the connection fails then [endpoint] will be disconnected. *)

  val bootstrap : t -> string -> cap
  (** [bootstrap t object_id] is the peer's bootstrap object [object_id], if any.
      Use [object_id = ""] for the main, public object. *)

  val disconnect : t -> Capnp_rpc.Exception.t -> unit Lwt.t
  (** [disconnect t reason] releases all resources used by the connection. *)

  val disconnecting : t -> bool
  (** [disconnecting t] the connection is shutting down (or has shut down). *)

  val dump : t Fmt.t
  (** [dump] dumps the state of the connection, for debugging. *)
end
