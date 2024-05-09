(** Send and receive capnp messages over a byte-stream. *)

open Eio.Std

val src : Logs.src
(** Control the log level. *)

type t
(** A wrapper for a byte-stream (flow). *)

val send : t -> 'a Capnp.BytesMessage.Message.t -> unit
(** [send t msg] enqueues [msg]. *)

val recv : t -> (Capnp.Message.ro Capnp.BytesMessage.Message.t, [> `Closed]) result
(** [recv t] reads the next message from the remote peer.
    It returns [Error `Closed] if the connection to the peer is lost. *)

val of_flow : sw:Switch.t -> peer_id:Auth.Digest.t -> _ Eio.Flow.two_way -> t
(** [of_flow ~sw ~peer_id flow] sends and receives on [flow].

    [sw] is used to run a fiber writing messages in batches. *)

val peer_id : t -> Auth.Digest.t
(** [peer_id t] is the fingerprint of the peer's public key,
    or [Auth.Digest.insecure] if TLS isn't being used. *)

val disconnect : t -> unit
(** [disconnect t] shuts down the underlying flow. *)
