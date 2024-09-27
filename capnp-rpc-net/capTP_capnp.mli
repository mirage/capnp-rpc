(** Provides the RPC layer on top of some network. *)

module Make (N : S.NETWORK) : sig
  type t
  (** A Cap'n Proto RPC protocol handler. *)

  val connect : sw:Eio.Switch.t -> restore:Restorer.t -> ?tags:Logs.Tag.set -> Endpoint.t -> t
  (** [connect ~sw ~restore ~switch endpoint] is fresh CapTP protocol handler that sends and
      receives messages using [endpoint].
      [restore] is used to respond to "Bootstrap" messages.
      If the connection fails then [endpoint] will be disconnected.
      You must call {!listen} to run the loop handling messages.
      @param sw Used to run methods and to run the transmit thread. *)

  val listen : t -> unit
  (** [listen t] reads and handles incoming messages until the connection is finished. *)

  val bootstrap : t -> string -> 'a Capnp_rpc_lwt.Capability.t
  (** [bootstrap t object_id] is the peer's bootstrap object [object_id], if any.
      Use [object_id = ""] for the main, public object. *)

  val disconnect : t -> Capnp_rpc.Exception.t -> unit
  (** [disconnect t reason] releases all resources used by the connection. *)

  val disconnecting : t -> bool
  (** [disconnecting t] the connection is shutting down (or has shut down). *)

  val dump : t Fmt.t
  (** [dump] dumps the state of the connection, for debugging. *)
end
