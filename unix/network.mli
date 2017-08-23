(** A network using TCP and Unix-domain sockets. *)

include Capnp_rpc_lwt.S.NETWORK with
  type Address.t = [
    | `Unix of string
    | `TCP of string * int
  ]
