(** A network using Unix-domain sockets. *)

include Capnp_rpc_lwt.S.NETWORK with
  type Address.t = [
    | `Unix of string
  ]
