(** Helpers for using {!Capnp_rpc_lwt} on traditional operating systems. *)

open Capnp_rpc_lwt

module Unix_flow = Unix_flow

include Capnp_rpc_lwt.S.VAT_NETWORK with
  type flow = Unix_flow.flow and
  type 'a capability = 'a Capability.t and
  module Network = Network

module Vat_config : sig
  type t = {
    backlog : int;
    secret_key : Auth.Secret_key.t option;
    listen_address : Network.Socket_address.t;
    public_address : Network.Socket_address.t;
  }

  val v :
    ?backlog:int ->
    ?public_address:Network.Socket_address.t ->
    secret_key:Auth.Secret_key.t option ->
    Network.Socket_address.t -> t
  (** [v ~secret_key listen_address] is the configuration for a server vat that
      listens on address [listen_address] and proves its identity to clients using [secret_key].
      [backlog] is passed to [Unix.listen].
      If [secret_key] is [None] then no authentication or encryption is performed.
      The vat will suggest that others connect to it at [public_address] (or [listen_address] if
      no public address is given). *)

  val pp : t Fmt.t

  val equal : t -> t -> bool

  val cmd : t Cmdliner.Term.t
  (** [cmd] evalutes to a configuration populated from the command-line options. *)

  val secret_key : Auth.Secret_key.t option Cmdliner.Term.t
  (** [secret_key] evaluates to a secret key specified on the command line, or to [None] if the
      user requested not to use any crypto.
      This is normally not needed - {!cmd} includes this automatically. *)
end

val sturdy_ref : unit -> 'a Sturdy_ref.t Cmdliner.Arg.conv
(** A cmdliner argument converter for parsing "capnp://" URIs. *)

val serve :
  ?offer:'a Capability.t ->
  Vat_config.t ->
  Vat.t Lwt.t
(** [serve ~offer vat_config] is a new vat that is listening for new connections
    as specified by [vat_config]. After connecting to it, clients can get
    access to the bootstrap service [offer]. *)

val client_only_vat :
  ?switch:Lwt_switch.t ->
  ?offer:'a Capability.t ->
  unit -> Vat.t
(** [client_only_vat ()] is a new vat that does not listen for incoming connections. *)
