(** Wraps a Unix [file_descr] to provide the Mirage flow API. *)

include Mirage_flow_lwt.S

val connect : ?switch:Lwt_switch.t -> Lwt_unix.file_descr -> flow

val socketpair : ?switch:Lwt_switch.t -> unit -> flow * flow
