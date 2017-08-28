(** Helpers for using {!Capnp_rpc_lwt} on traditional operating systems. *)

open Capnp_rpc_lwt

module Unix_flow = Unix_flow

include Capnp_rpc_lwt.S.VAT_NETWORK with
  type flow = Unix_flow.flow and
  type 'a capability = 'a Capability.t and
  type restorer = Restorer.t and
  type Sturdy_ref.service_id = Restorer.Id.t and
  module Network = Network

module Vat_config : sig
  type t

  val create :
    ?backlog:int ->
    ?public_address:Network.Socket_address.t ->
    secret_key:[< `File of string | `PEM of string | `Ephemeral] ->
    ?serve_tls:bool ->
    Network.Socket_address.t -> t
  (** [create ~secret_key listen_address] is the configuration for a server vat that
      listens on address [listen_address].
      [secret_key] may be one of:
      - [`File path]: a PEM-encoded RSA private key is read from [path]. If [path] doesn't yet
        exist, a new key is created and stored there.
      - [`PEM data]: the given PEM-encoded data is used as the key.
      - [`Ephemeral]: a new key is generated (if needed) and not saved anywhere.
      If [serve_tls] is [false] then the vat accepts unencrypted incoming connections.
      If [true] (the default), the vat performs a server TLS handshake, using
      [secret_key] to prove its identity to clients.
      [backlog] is passed to [Unix.listen].
      The vat will suggest that others connect to it at [public_address] (or [listen_address] if
      no public address is given). *)

  val secret_key : t -> Auth.Secret_key.t
  (** [secret_key t] returns the vat's secret yet, generating it if this is the first time
      it has been used. *)

  val hashed_secret : t -> string
  (** [hashed_secret t] is the SHA256 digest of the secret key file.
      This is useful as an input to {!Restorer.Id.derived}. *)

  val derived_id : ?name:string -> t -> Restorer.Id.t
  (** [derived_id t] is a secret service ID derived from the vat's secret key
      (using {!Restorer.Id.derived}). It won't change (unless the vat's key
      changes). [name] defaults to ["main"]. *)

  val pp : t Fmt.t
  (** This is probably only useful for the unit-tests. *)

  val equal : t -> t -> bool
  (** This is probably only useful for the unit-tests. *)

  val cmd : t Cmdliner.Term.t
  (** [cmd] evalutes to a configuration populated from the command-line options. *)
end

val sturdy_ref : unit -> 'a Sturdy_ref.t Cmdliner.Arg.conv
(** A cmdliner argument converter for parsing "capnp://" URIs. *)

val serve :
  ?restore:Restorer.t ->
  Vat_config.t ->
  Vat.t Lwt.t
(** [serve ~restore vat_config] is a new vat that is listening for new connections
    as specified by [vat_config]. After connecting to it, clients can get access
    to services using [restore]. *)

val client_only_vat :
  ?switch:Lwt_switch.t ->
  ?restore:Restorer.t ->
  unit -> Vat.t
(** [client_only_vat ()] is a new vat that does not listen for incoming connections. *)
