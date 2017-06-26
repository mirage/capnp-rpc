module Make (EP : Message_types.ENDPOINT) : sig
  type t
  (** A [t] is a connection to a remote vat. *)

  val create : ?bootstrap:#EP.Core_types.cap -> tags:Logs.Tag.set -> queue_send:(EP.Out.t -> unit) -> t
  (** [create ~bootstrap ~tags ~queue_send] is a handler for a connection to a remote peer.
      Messages will be sent to the peer by calling [queue_send] (which MUST deliver them in order).
      If the remote peer asks for the bootstrap object, it will be given a reference to [bootstrap].
      Log messages will be tagged with [tags]. *)

  val bootstrap : t -> EP.Core_types.cap
  (** [bootstrap t] returns a reference to the remote peer's bootstrap object, if any.
      This call does not block; the result is a promise for the object, on which further
      messages may be pipelined. *)

  val handle_msg : t -> EP.In.t -> unit
  (** [handle_msg t] feeds one message received from the remote peer into [t].
      It will call [queue_send] as necessary to handle the call.
      Messages MUST be fed to [handle_msg] in the order in which they arrive from the peer. *)

  val disconnect : t -> Exception.t -> unit
  (** [disconnect t reason] breaks all references with [reason] and releases the bootstrap object. *)

  (** {2 Debugging and diagnostics} *)

  val tags : ?qid:EP.Out.QuestionId.t -> ?aid:EP.Out.AnswerId.t -> t -> Logs.Tag.set
  (** [tags t] is a set of logging tags suitable for logging a message about this connection.
      [qid] or [aid] (but not both) may be given to add a tag with the question ID. *)

  val stats : t -> Stats.t
  (** [stats t] returns statistics about the state of the connection. *)

  val dump : t Fmt.t
  (** [dump] formats a dump of the current state of the connection. *)

  val check : t -> unit
  (** [check t] performs some sanity checks on the state of the tables and raises an exception
      if it finds a problem. *)
end
