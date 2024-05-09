(** Send and receive capnp messages over a byte-stream. *)

val src : Logs.src
(** Control the log level. *)

type t
(** A wrapper for a byte-stream (flow). *)

val send : t -> 'a Capnp.BytesMessage.Message.t -> unit
(** [send t msg] enqueues [msg]. *)

val run_writer : tags:Logs.Tag.set -> t -> unit
(** [run_writer ~tags t] runs a loop that transmits batches of messages from [t].
    It returns when the flow is closed. *)

val recv : tags:Logs.Tag.set -> t -> (Capnp.Message.ro Capnp.BytesMessage.Message.t, [> `Closed]) result
(** [recv ~tags t] reads the next message from the remote peer.
    It returns [Error `Closed] if the connection to the peer is lost. *)

val of_flow : peer_id:Auth.Digest.t -> _ Eio.Flow.two_way -> t
(** [of_flow ~peer_id flow] sends and receives on [flow]. *)

val peer_id : t -> Auth.Digest.t
(** [peer_id t] is the fingerprint of the peer's public key,
    or [Auth.Digest.insecure] if TLS isn't being used. *)
                     
val flush : t -> unit
(** [flush t] is useful to try to send any buffered data before disconnecting.
    Otherwise, the final abort message is likely to get lost. *)

val disconnect : t -> unit
(** [disconnect t] shuts down the underlying flow. *)
