(** Helpers for using {!Capnp_rpc_lwt} with MirageOS. *)

open Capnp_rpc_net

module Location = Network.Location

module Make (R : Mirage_random.S) (T : Mirage_time.S) (M : Mirage_clock.MCLOCK) (P : Mirage_clock.PCLOCK) (Stack : Tcpip.Stack.V4V6) : sig
  include Capnp_rpc_net.VAT_NETWORK with
    type flow = Stack.TCP.flow and
    module Network = Network.Make(R)(T)(M)(P)(Stack)

  module Vat_config : sig
    module Listen_address : sig
      type t = [`TCP of int]
      val pp : t Fmt.t
    end

    type t

    val create :
      public_address:Location.t ->
      secret_key:[< `PEM of string | `Ephemeral] ->
      ?serve_tls:bool ->
      Listen_address.t -> t
    (** [create ~public_address ~secret_key listen_address] is the configuration for a server vat that
        listens on address [listen_address].
        [secret_key] may be one of:
        - [`PEM data]: the given PEM-encoded data is used as the key.
        - [`Ephemeral]: a new key is generated (if needed) and not saved anywhere.
        If [serve_tls] is [false] then the vat accepts unencrypted incoming connections.
        If [true] (the default), the vat performs a server TLS handshake, using
        [secret_key] to prove its identity to clients.
        The vat will suggest that others connect to it at [public_address]. *)

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
  end

  val network : dns:Network.Dns.t -> Stack.t -> Network.t

  val serve :
    ?switch:Lwt_switch.t ->
    ?tags:Logs.Tag.set ->
    ?restore:Restorer.t ->
    Network.t ->
    Vat_config.t ->
    Vat.t Lwt.t
  (** [serve ~restore net vat_config] is a new vat that is listening for new connections
      as specified by [vat_config]. After connecting to it, clients can get access
      to services using [restore]. *)

  val client_only_vat :
    ?switch:Lwt_switch.t ->
    ?tags:Logs.Tag.set ->
    ?restore:Restorer.t ->
    Network.t -> Vat.t
  (** [client_only_vat net] is a new vat that does not listen for incoming connections. *)
end
