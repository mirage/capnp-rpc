(** Module signatures. *)

open Capnp_rpc_lwt

module type ADDRESS = sig
  type t
  (** A network address at which a vat can be reached. *)

  val parse_uri : Uri.t -> ((t * string), [> `Msg of string]) result
  (** [parse_uri uri] extracts from a URI the network address and service ID. *)

  val to_uri : t * string -> Uri.t
  (** [to_uri (t, service_id)] is a URI that can be parsed back into [(t, service_id)] by [parse_uri]. *)

  val equal : t -> t -> bool

  val digest : t -> Auth.Digest.t
  (** How to verify that the correct address has been reached. *)

  val pp : t Fmt.t
end

module type NETWORK = sig
  module Types : Capnp_rpc.S.NETWORK_TYPES

  module Address : ADDRESS

  type t

  val connect :
    t ->
    sw:Eio.Switch.t ->
    secret_key:Auth.Secret_key.t Lazy.t ->
    Address.t ->
    (Endpoint.t, [> `Msg of string]) result
  (** [connect t ~sw ~secret_key address] connects to [address], proves ownership of
      [secret_key] (if TLS is being used), and returns the resulting endpoint.
      Returns an error if no connection can be established or the target fails
      to authenticate itself. *)

  val parse_third_party_cap_id : Private.Schema.Reader.pointer_t -> Types.third_party_cap_id
end

module type VAT_NETWORK = sig
  (** Stretching capability references across a network link.
      Note: see {!module:Capnp_rpc_unix} for a higher-level wrapper for this API. *)

  type +'a capability
  (** An ['a capability] is a capability reference to a service of type ['a]. *)

  type restorer
  (** A function for restoring persistent capabilities from sturdy ref service IDs. *)

  type +'a sturdy_ref
  (** An off-line (persistent) capability. *)

  type service_id
  (** A (secret) token that identifies a persistent service within a vat and grants access to it. *)

  module Network : NETWORK

  (** Sharing capabilities over a network link. *)
  module CapTP : sig

    type t
    (** A CapTP connection to a remote peer. *)

    val connect : sw:Eio.Switch.t -> restore:restorer -> ?tags:Logs.Tag.set -> Endpoint.t -> t
    (** [connect ~sw ~restore ~switch endpoint] is fresh CapTP protocol handler that sends and
        receives messages using [endpoint].
        [restore] is used to respond to "Bootstrap" messages.
        If the connection fails then [endpoint] will be disconnected.
        You must call {!listen} to run the loop handling messages.
        @param sw Used to run methods and to run the transmit thread. *)

    val listen : t -> unit
    (** [listen t] reads and handles incoming messages until the connection is finished. *)

    val bootstrap : t -> service_id -> 'a capability
    (** [bootstrap t object_id] is the peer's bootstrap object [object_id], if any.
        Use [object_id = ""] for the main, public object. *)

    val disconnect : t -> Capnp_rpc.Exception.t -> unit
    (** [disconnect reason] closes the connection, sending [reason] to the peer to explain why.
        Capabilities and questions at both ends will break, with [reason] as the problem. *)

    val dump : t Fmt.t
    (** [dump] dumps the state of the connection, for debugging. *)
  end

  (** A Vat is an actor in the CapTP network, composed of
      objects that can call each other directly. *)
  module Vat : sig
    (** An actor in the CapTP network.
        A Vat may be connected to other Vats over CapTP network connections.
        Typically an application will create only a single Vat.
        See the {!module:Capnp_rpc_unix} module for a higher-level API. *)

    type t
    (** A local Vat. *)

    val create :
      ?tags:Logs.Tag.set ->
      ?restore:restorer ->
      ?address:Network.Address.t ->
      sw:Eio.Switch.t ->
      secret_key:Auth.Secret_key.t Lazy.t ->
      Network.t -> t
    (** [create ~sw ~restore ~address ~secret_key network] is a new Vat that
        uses [restore] to restore sturdy refs hosted at this vat to live
        capabilities for peers.
        The Vat will suggest that other parties connect to it using [address].
        Turning off the switch will disconnect any active connections. *)

    val add_connection : t -> mode:[`Accept|`Connect] -> Endpoint.t -> CapTP.t
    (** [add_connection t ~mode endpoint] runs the CapTP protocol over [endpoint],
        which is a connection to another vat.
        [mode] is used if two Vats connect to each other at the same time to
        decide which connection to drop. Use [`Connect] if [t] initiated the new
        connection. Note that [add_connection] may return an existing connection. *)

    val public_address : t -> Network.Address.t option
    (** [public_address t] is the address that peers should use when connecting
        to this Vat to locate and authenticate it. *)

    val sturdy_ref : t -> service_id -> 'a sturdy_ref
    (** [sturdy_ref t service_id] is a sturdy ref for [service_id], hosted at this Vat.
        Fails if this Vat does not accept incoming connections. *)

    val export : t -> 'a sturdy_ref -> Uri.t
    (** [export t sr] turns [sr] into a URI, which can be displayed and imported into another Vat. *)

    val sturdy_uri : t -> service_id -> Uri.t
    (** [sturdy_uri t id] is [sturdy_ref t id |> export t]. *)

    val import : t -> Uri.t -> ('a sturdy_ref, [> `Msg of string]) result
    (** [import t uri] parses [uri] as a "capnp://" URI. *)

    val import_exn : t -> Uri.t -> 'a sturdy_ref
    (** [import_exn] is a wrapper for [import] that raises an exception if it fails. *)

    val dump : t Fmt.t
  end
end
