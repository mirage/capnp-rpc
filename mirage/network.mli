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

module Make (R : Mirage_random.S) (T : Mirage_time.S) (C : Mirage_clock.MCLOCK) (Stack : Mirage_stack.V4V6) : sig

  module Dns : module type of Dns_client_mirage.Make(R)(T)(C)(Stack)

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
    Stack.TCP.flow ->
    (Capnp_rpc_net.Endpoint.t, [> `Msg of string]) result Lwt.t
  (** [accept_connection ~switch ~secret_key flow] is a new endpoint for [flow].
      If [secret_key] is not [None], it is used to perform a TLS server-side handshake.
      Otherwise, the connection is not encrypted. *)
end
