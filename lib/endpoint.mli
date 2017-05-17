(* Send and receive capnp messages over a socket. *)

type t

val of_socket : switch:Lwt_switch.t -> Unix.file_descr -> t
(** [of_socket ~switch fd] sends and receives on [fd].
    When [switch] is turned off, [fd] is closed. *)

val send : t -> 'a Capnp.BytesMessage.Message.t -> unit Lwt.t
(** [send t msg] transmits [msg] atomically. *)

val recv : t -> (Capnp.Message.ro Capnp.BytesMessage.Message.t, [`Closed]) result Lwt.t
(** [recv t] reads the next message from the remote peer.
    It returns [Error `Closed] if the connection to the peer is lost
    (this will also happen if the switch is turned off). *)
