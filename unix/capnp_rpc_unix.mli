(** Helpers for using {!Capnp_rpc_lwt} on traditional operating systems. *)

open Capnp_rpc_lwt
open Capnp_rpc_net

module Unix_flow = Unix_flow

include Capnp_rpc_net.VAT_NETWORK with
  type flow = Unix_flow.flow and
  module Network = Network

module Vat_config : sig
  type t

  val create :
    ?backlog:int ->
    ?public_address:Network.Location.t ->
    secret_key:[< `File of string | `PEM of string | `Ephemeral] ->
    ?serve_tls:bool ->
    Network.Location.t -> t
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

  val derived_id : t -> string -> Restorer.Id.t
  (** [derived_id t name] is a secret service ID derived from name and the
      vat's secret key (using {!Restorer.Id.derived}). It won't change
      (unless the vat's key changes). *)

  val sturdy_uri : t -> Restorer.Id.t -> Uri.t
  (** [sturdy_uri t id] is a sturdy URI for [id] at the vat that would be
      created by [t]. *)

  val pp : t Fmt.t
  (** This is probably only useful for the unit-tests. *)

  val equal : t -> t -> bool
  (** This is probably only useful for the unit-tests. *)

  val cmd : t Cmdliner.Term.t
  (** [cmd] evalutes to a configuration populated from the command-line options. *)
end

module File_store : sig
  (** An on-disk store for saved services. *)

  type 'a t
  (** A store of values of type ['a]. *)

  val create : string -> 'a t
  (** [create dir] is a store for Cap'n Proto structs.
      Items are stored inside [dir]. *)

  val save : 'a t -> digest:string -> 'a StructStorage.reader_t -> unit
  (** [save t ~digest data] saves [data] to disk in a file named [base64_encode digest]. *)

  val load : 'a t -> digest:string -> 'a StructStorage.reader_t option
  (** [load t ~digest] is the data passed to [save t ~digest],
      or [None] if the digest is not known. *)

  val remove : 'a t -> digest:string -> unit
  (** [remove t ~digest] removes the stored data for [digest]. *)
end

module Cap_file : sig
  val load : Vat.t -> string -> (_ Sturdy_ref.t, [> `Msg of string]) result
  (** [load vat path] loads the contents of [path] as a capability URI. *)

  val save_sturdy : Vat.t -> _ Sturdy_ref.t -> string -> (unit, [> `Msg of string]) result
  (** [save_sturdy vat sr path] saves [sr] to [path], with a mode of [0o600]. *)

  val save_service : Vat.t -> Capnp_rpc_net.Restorer.Id.t -> string ->
    (unit, [> `Msg of string]) result
  (** [save_service vat id path] saves [vat/id] to [path], with a mode of [0o600]. *)
end

val sturdy_uri : Uri.t Cmdliner.Arg.conv
(** A cmdliner argument converter for a "capnp://" URI (or the path of a file containing such a URI). *)

val serve :
  ?switch:Lwt_switch.t ->
  ?tags:Logs.Tag.set ->
  ?restore:Restorer.t ->
  Vat_config.t ->
  Vat.t Lwt.t
(** [serve ~restore vat_config] is a new vat that is listening for new connections
    as specified by [vat_config]. After connecting to it, clients can get access
    to services using [restore]. *)

val client_only_vat :
  ?switch:Lwt_switch.t ->
  ?tags:Logs.Tag.set ->
  ?restore:Restorer.t ->
  unit -> Vat.t
(** [client_only_vat ()] is a new vat that does not listen for incoming connections. *)

val manpage_capnp_options : string
(** [manpage_capnp_options] is the title of the section of the man-page containing the Cap'n Proto options.
    This can be used to control where these options appear in the page (e.g. to put them below the other options). *)
