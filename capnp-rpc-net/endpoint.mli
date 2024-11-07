(** Send and receive capnp messages over a byte-stream. *)

val src : Logs.src
(** Control the log level. *)

type t
(** A wrapper for a byte-stream (flow). *)

val send : t -> 'a Capnp.BytesMessage.Message.t -> (unit, [`Closed | `Msg of string]) result
(** [send t msg] transmits [msg]. *)

val recv : t -> (Capnp.Message.ro Capnp.BytesMessage.Message.t, [> `Closed]) result
(** [recv t] reads the next message from the remote peer.
    It returns [Error `Closed] if the connection to the peer is lost. *)

val of_flow : peer_id:Auth.Digest.t -> _ Eio.Flow.two_way -> t
(** [of_flow ~peer_id flow] sends and receives on [flow]. *)

val peer_id : t -> Auth.Digest.t
(** [peer_id t] is the fingerprint of the peer's public key,
    or [Auth.Digest.insecure] if TLS isn't being used. *)

val disconnect : t -> unit
(** [disconnect t] shuts down the underlying flow. *)
