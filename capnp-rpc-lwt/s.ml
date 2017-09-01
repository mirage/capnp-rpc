(** Module signatures. *)

module type NETWORK = sig
  module Types : Capnp_rpc.S.NETWORK_TYPES

  module Address : sig
    type t
    (** A network address at which a vat can be reached. *)

    val parse_uri : Uri.t -> ((t * string), [> `Msg of string]) result
    (** [parse_uri uri] extracts from a URI the network address and service ID. *)

    val to_uri : t -> string -> Uri.t
    (** [to_uri t service_id] is a URI that can be parsed back into [(t, service_id)] by [parse_uri]. *)

    val equal : t -> t -> bool

    val pp : t Fmt.t
  end

  val connect :
    secret_key:Auth.Secret_key.t Lazy.t ->
    Address.t ->
    (Endpoint.t, [> `Msg of string]) result Lwt.t

  val parse_third_party_cap_id : Schema.Reader.pointer_t -> Types.third_party_cap_id
end

module type VAT_NETWORK = sig
  (** Stretching capability references across a network link.
      Note: see {!module:Capnp_rpc_unix} for a higher-level wrapper for this API. *)

  type 'a capability
  (** An ['a capability] is a capability reference to a service of type ['a]. *)

  type flow
  (** A bi-directional byte-stream. *)

  type restorer
  (** A function for restoring persistent capabilities from sturdy ref service IDs. *)

  module Network : NETWORK

  module Sturdy_ref : sig
    (** An off-line reference to a capability.

        A sturdy ref contains all the information necessary to get a live reference to the capability:

        - The network address of the hosting vat (e.g. TCP host and port)
        - A way to authenticate the hosting vat (e.g. a fingerprint of the vat's public key)
        - A way to identify the target service within the vat and prove access to it (e.g. a Swiss number)

        Note that a sturdy ref often contains secrets.
      *)

    type service_id
    (** Identifies a service within the hosting vat.
        This is typically a long, unguessable string. *)

    type +'a t
    (** A persistent capability reference that can be restored after contact to the target vat is interrupted. *)

    val v : address:Network.Address.t -> service:service_id -> 'a t
    (** Create a new sturdy ref. *)

    val address : 'a t -> Network.Address.t
    (** Where and how to connect to access this service's vat. *)

    val service : 'a t -> service_id
    (** Which service within the vat to select. *)

    val cast : _ t -> _ t
    (** Treat this as a reference to service with a different type. *)

    val of_uri : Uri.t -> ('a t, [> `Msg of string]) result
    (** [of_uri x] constructs a sturdy ref from a URI of the form "capnp://AUTH@...".
        AUTH is HASH-TYPE@DIGEST for connections using TLS, or "insecure" for connections without.
        The rest of the URI is parsed using [Network.Address.parse_uri]. *)

    val to_uri_with_secrets : 'a t -> Uri.t
    (** [to_uri_with_secrets t] is a "capnp://" URI which can be converted back to [t] using [of_uri]. *)

    val pp_with_secrets : 'a t Fmt.t
    (** [pp_with_secrets] formats a sturdy ref as a URI, including any secret tokens. *)

    val pp_address : 'a t Fmt.t
    (** [pp_address] formats just the (public) address part of a [t]. *)

    val equal : 'a t -> 'a t -> bool
    (** [equal a b] is [true] iff [a] and [b] have the same address, server fingerprint and service. *)
  end

  module CapTP : sig
    (** Sharing capabilities over a network link. *)

    type t
    (** A CapTP connection to a remote peer. *)

    val connect : restore:restorer -> ?tags:Logs.Tag.set -> switch:Lwt_switch.t -> Endpoint.t -> t
    (** [connect ~restore ~switch endpoint] is fresh CapTP protocol handler that sends and
        receives messages using [endpoint].
        [restore] is used to respond to "Bootstrap" messages.
        If the connection fails then [switch] will be turned off, and turning off the switch
        will release all resources used by the connection. *)

    val bootstrap : t -> Sturdy_ref.service_id -> 'a capability
    (** [bootstrap t object_id] is the peer's bootstrap object [object_id], if any.
        Use [object_id = ""] for the main, public object. *)

    val disconnect : t -> Capnp_rpc.Exception.t -> unit Lwt.t
    (** [disconnect reason] closes the connection, sending [reason] to the peer to explain why.
        Capabilities and questions at both ends will break, with [reason] as the problem. *)

    val dump : t Fmt.t
    (** [dump] dumps the state of the connection, for debugging. *)
  end

  module Vat : sig
    (** An actor in the CapTP network.
        A vat is a collection of objects that can call each other directly.
        A vat may be connected to other vats over CapTP network connections.
        Typically an application will create only a single vat.
        See the {!module:Capnp_rpc_unix} module for a higher-level API. *)

    type t
    (** A local vat. *)

    val create :
      ?switch:Lwt_switch.t ->
      ?restore:restorer ->
      ?address:Network.Address.t ->
      secret_key:Auth.Secret_key.t Lazy.t ->
      unit -> t
    (** [create ~switch ~restore ~address ~secret_key ()] is a new vat that
        uses [restore] to restore sturdy refs hosted at this vat to live
        capabilities for peers.
        The vat will suggest that other parties connect to it using [address].
        Turning off the switch will disconnect any active connections. *)

    val add_connection : t -> Endpoint.t -> CapTP.t
    (** [add_connection t endpoint] runs the CapTP protocol over [endpoint],
        which is a connection to another vat. *)

    val public_address : t -> Network.Address.t option
    (** [public_address t] is the address that peers should use when connecting
        to this vat to locate and authenticate it. *)

    val sturdy_ref : t -> Sturdy_ref.service_id -> 'a Sturdy_ref.t
    (** [sturdy_ref t object_id] is a sturdy ref for [object_id], hosted at this vat.
        Fails if this vat does not accept incoming connections. *)

    val connect : t -> 'a Sturdy_ref.t -> ('a capability, [> `Msg of string]) result Lwt.t
    (** [connect t sr] creates and returns a live reference to the off-line capability [sr]. *)

    val connect_exn : t -> 'a Sturdy_ref.t -> 'a capability Lwt.t
    (** [connect_exn] is a wrapper for [connect] that returns a failed Lwt thread on error. *)

    val dump : t Fmt.t
  end
end
