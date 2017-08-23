(** Helpers for using {!Capnp_rpc_lwt} on traditional operating systems. *)

open Capnp_rpc_lwt

include Capnp_rpc_lwt.S.VAT_NETWORK
  with type Network.Address.t = [
      | `Unix of string
    ]

module Vat_config : sig
  type t = {
    listen_address : Network.Address.t;
    public_address : Network.Address.t;
  }

  val v :
    ?public_address:Network.Address.t ->
    Network.Address.t -> t
  (** [v listen_address] is the configuration for a server vat that listens on address [listen_address].
      The vat will suggest that others connect to it at [public_address] (or [listen_address] if
      no public address is given. *)
 
  val cmd : t Cmdliner.Term.t
  (** [cmd] evalutes to a configuration populated from the command-line options. *)
end

module Connect_address : sig
  type t = [
    | `Unix of string
  ]

  val conv : t Cmdliner.Arg.conv
  (** A cmdliner argument converter for [t]. *)

  val pp : t Fmt.t
end

val endpoint_of_socket : switch:Lwt_switch.t -> Lwt_unix.file_descr -> Capnp_rpc_lwt.Endpoint.t
(** [endpoint_of_socket ~switch fd] is an endpoint that sends and receives on [fd].
    When [switch] is turned off, [fd] is closed. *)

val serve : ?backlog:int -> ?offer:'a Capability.t -> Vat_config.t -> 'b Lwt.t
(** [serve ~offer address] listens for new connections on [address] and handles them.
    Clients can get access to the bootstrap object [offer].
    [backlog] is passed to [Unix.listen]. *)

val connect : ?switch:Lwt_switch.t -> ?offer:'a Capability.t -> Connect_address.t -> 'b Capability.t
(** [connect addr] connects to the server at [addr] and returns its bootstrap object.
    If [offer] is given, the client will also offer this service to the remote vat.
    If [switch] is given then turning it off will disconnect,
    and disconnecting will turn off the switch. *)
