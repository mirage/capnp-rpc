(** The abstract Cap'n Proto RPC network protocol.
    @see <https://capnproto.org/rpc.html> *)

module Make (EP : Message_types.ENDPOINT) : sig
  type t
  (** A [t] is a connection to a remote vat. *)

  type restorer = ((EP.Core_types.cap, Exception.t) result -> unit) -> string -> unit
  (** A [restorer] is a function [f] for restoring saved capabilities.
      [f k object_id] must eventually call [k result] exactly once to respond
      to the client's bootstrap message with [result]. [k] takes ownership of the
      capability. *)

  val create : ?restore:restorer -> tags:Logs.Tag.set ->
    fork:((unit -> unit) -> unit) ->
    queue_send:([> EP.Out.t] -> unit) -> t
  (** [create ~restore ~tags ~queue_send] is a handler for a connection to a remote peer.
      Messages will be sent to the peer by calling [queue_send] (which MUST deliver them in order).
      If the remote peer asks for a bootstrap object, [restore] will be used to get it.
      Log messages will be tagged with [tags].
      @param fork is used when dispatching a local method handler. *)

  val bootstrap : t -> string -> EP.Core_types.cap
  (** [bootstrap t object_id] returns a reference to the remote peer's bootstrap object, if any.
      [object_id] is the "deprecatedObjectId", which is, however, still used.
      This call does not block; the result is a promise for the object, on which further
      messages may be pipelined. *)

  val handle_msg : t -> [< EP.In.t | `Unimplemented of EP.Out.t] -> unit
  (** [handle_msg t] feeds one message received from the remote peer into [t].
      It will call [queue_send] as necessary to handle the call.
      Messages MUST be fed to [handle_msg] in the order in which they arrive from the peer. *)

  val disconnect : t -> Exception.t -> unit
  (** [disconnect t reason] breaks all references with [reason] and releases the bootstrap object.
      Does nothing if already disconnected. *)

  (** {2 Debugging and diagnostics} *)

  val tags : t -> Logs.Tag.set
  (** [tags t] is a set of logging tags suitable for logging a message about this connection. *)

  val stats : t -> Stats.t
  (** [stats t] returns statistics about the state of the connection. *)

  val dump : t Fmt.t
  (** [dump] formats a dump of the current state of the connection. *)

  val check : t -> unit
  (** [check t] performs some sanity checks on the state of the tables and raises an exception
      if it finds a problem. *)
end
