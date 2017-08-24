(** A network using TCP and Unix-domain sockets. *)

module Socket_address : sig
  type t = [
    | `Unix of string
    | `TCP of string * int
  ]

  val pp : t Fmt.t

  val equal : t -> t -> bool
end

include Capnp_rpc_lwt.S.NETWORK with
  type Address.t = Socket_address.t * Capnp_rpc_lwt.Auth.Digest.t

val accept_connection :
  switch:Lwt_switch.t ->
  secret_key:Capnp_rpc_lwt.Auth.Secret_key.t option ->
  Unix_flow.flow ->
  (Capnp_rpc_lwt.Endpoint.t, [> `Msg of string]) result Lwt.t
(** [accept_connection ~switch ~secret_key flow] is a new endpoint for [flow].
    If [secret_key] is not [None], it is used to perform a TLS server-side handshake.
    Otherwise, the connection is not encrypted. *)
