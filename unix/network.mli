(** A network using TCP and Unix-domain sockets. *)

module Location : sig
  type t = [
    | `Unix of string
    | `TCP of string * int
  ]

  val validate_public : t -> unit
  (** Raises an exception if [t] is not a valid public address (e.g. the Unix path is relative) *)

  val pp : t Fmt.t

  val equal : t -> t -> bool

  val unix : string -> t
  (** [unix path] is a Unix-domain socket address. [path] is made absolute if it isn't already. *)

  val tcp : host:string -> port:int -> t
  (** [tcp ~host port] is [`TCP (host, port)]. *)

  val of_string : string -> (t, [> `Msg of string]) result

  val cmdliner_conv : t Cmdliner.Arg.conv
end

include Capnp_rpc_lwt.S.NETWORK with
  type t = unit and
  type Address.t = Location.t * Capnp_rpc_lwt.Auth.Digest.t

val accept_connection :
  switch:Lwt_switch.t ->
  secret_key:Capnp_rpc_lwt.Auth.Secret_key.t option ->
  Unix_flow.flow ->
  (Capnp_rpc_lwt.Endpoint.t, [> `Msg of string]) result Lwt.t
(** [accept_connection ~switch ~secret_key flow] is a new endpoint for [flow].
    If [secret_key] is not [None], it is used to perform a TLS server-side handshake.
    Otherwise, the connection is not encrypted. *)
