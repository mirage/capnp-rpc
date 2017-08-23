module type NETWORK = sig
  module Types : Capnp_rpc.S.NETWORK_TYPES

  val parse_third_party_cap_id : Schema.Reader.pointer_t -> Types.third_party_cap_id
end

module type VAT_NETWORK = sig
  (** Stretching capability references across a network link.
      Note: see {!module:Capnp_rpc_unix} for a higher-level wrapper for this API. *)

  type 'a capability
  (** An ['a capability] is a capability reference to a service of type ['a]. *)

  module Network : NETWORK

  module CapTP : sig
    (** Sharing capabilities over a network link. *)

    type t
    (** A CapTP connection to a remote peer. *)

    val connect : ?offer:'a capability -> ?tags:Logs.Tag.set -> switch:Lwt_switch.t -> Endpoint.t -> t
    (** [connect ?offer ~switch endpoint] is fresh CapTP protocol handler that sends and
        receives messages using [endpoint].
        If [offer] is given, the peer can use the "Bootstrap" message to get access to it.
        If the connection fails then [switch] will be turned off, and turning off the switch
        will release all resources used by the connection. *)

    val bootstrap : t -> 'a capability
    (** [bootstrap t] is the peer's public bootstrap object, if any. *)

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

    val create : ?switch:Lwt_switch.t -> ?bootstrap:'a capability -> unit -> t
    (** [create ~switch ~bootstrap ()] is a new vat that offers [bootstrap] to its peers.
        The vat takes ownership of [bootstrap], and will release it when the switch is turned off.
        Turning off the switch will also disconnect any active connections. *)

    val connect : t -> Endpoint.t -> CapTP.t
    (** [connect t endpoint] runs the CapTP protocol over [endpoint], which is a
        connection to another vat. *)
  end
end
