(** A capnp network build from a Mirage network stack. *)

module Location : sig
  type t = [
    | `TCP of string * int
  ]

  val pp : t Fmt.t

  val equal : t -> t -> bool

  val tcp : host:string -> port:int -> t
  (** [tcp ~host port] is [`TCP (host, port)]. *)
end

module Make (Stack : Mirage_stack_lwt.V4) (Dns : Dns_resolver_mirage.S) : sig

  type t = {
    stack : Stack.t;
    dns : Dns.t;
  }

  include Capnp_rpc_net.S.NETWORK with
    type t := t and
    type Address.t = Location.t * Capnp_rpc_net.Auth.Digest.t

  val accept_connection :
    switch:Lwt_switch.t ->
    secret_key:Capnp_rpc_net.Auth.Secret_key.t option ->
    Stack.TCPV4.flow ->
    (Capnp_rpc_net.Endpoint.t, [> `Msg of string]) result Lwt.t
  (** [accept_connection ~switch ~secret_key flow] is a new endpoint for [flow].
      If [secret_key] is not [None], it is used to perform a TLS server-side handshake.
      Otherwise, the connection is not encrypted. *)
end
