(** Send and receive capnp messages over a byte-stream. *)

type t
(** A wrapper for a byte-stream (flow). *)

val send : t -> 'a Capnp.BytesMessage.Message.t -> (unit, [`Closed | `Msg of string]) result Lwt.t
(** [send t msg] transmits [msg]. *)

val recv : t -> (Capnp.Message.ro Capnp.BytesMessage.Message.t, [> `Closed]) result Lwt.t
(** [recv t] reads the next message from the remote peer.
    It returns [Error `Closed] if the connection to the peer is lost
    (this will also happen if the switch is turned off). *)

val of_flow : switch:Lwt_switch.t -> peer_id:Auth.Digest.t ->
  (module Mirage_flow.S with type flow = 'flow) -> 'flow -> t
(** [of_flow ~switch ~peer_id (module F) flow] sends and receives on [flow].
    The caller should arrange for [flow] to be closed when the switch is turned off.
    If the flow is closed, the switch will be turned off.
    If the flow returns an error when the switch is off, the endpoint will return [`Closed]
    instead of the underlying error. *)

val peer_id : t -> Auth.Digest.t
(** [peer_id t] is the fingerprint of the peer's public key,
    or [Auth.Digest.insecure] TLS isn't being used. *)

val disconnect : t -> unit Lwt.t
(** [disconnect t] turns off [t]'s switch. *)

val pp_error : [< `Closed | `Msg of string] Fmt.t
