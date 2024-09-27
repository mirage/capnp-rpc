(** This package adds networking support, including TLS. It contains code common
    to capnp-rpc-unix and capnp-rpc-mirage. Libraries should not need to link against
    this package (just use capnp-rpc-lwt instead), since they generally shouldn't
    care whether services are local or remote. *)

open Capnp_rpc_lwt

module S = S

module Endpoint = Endpoint
module Two_party_network = Two_party_network
module Auth = Auth
module Tls_wrapper = Tls_wrapper

module Restorer : sig
  module Id : sig
    type t
    (** The object ID passed in the Cap'n Proto Bootstrap message. *)

    val generate : unit -> t
    (** [generate ()] is a fresh unguessable service ID.
        Note: you must initialise `Nocrypto`'s entropy before calling this
        (you will get a runtime error if you forget). *)

    val derived : secret:string -> string -> t
    (** [derived ~secret name] is a service ID based on [secret] and [name].
        It is calculated as [SHA256.hmac secret name].
        [secret] could be the hash of a private key file, for example. *)

    val public : string -> t
    (** [public name] is the service ID [name].
        This may be useful for interoperability with non-secure clients that expect
        to use a plain-text service ID (e.g. "calculator"). It could also be
        useful if [name] is some unguessable token you have generated yourself. *)

    val digest : Auth.hash -> t -> string
    (** [digest h id] is the digest [h id].

        Since [id] is normally a secret token, we must be careful not to allow
        timing attacks (taking a slightly different amount of time to return an
        error depending on how much of the ID the caller guessed correctly).
        Taking a secure hash of the value first is one way to avoid this, since
        revealing the hash isn't helpful to the attacker. *)

    val to_string : t -> string
    (** [to_string t] is the raw bytes of [t]. *)

    val pp : t Fmt.t
    val equal : t -> t -> bool
  end

  (** {2 Resolutions} *)

  type resolution
  (** Internally, this is just [('a Capability.t, Capnp_rpc.Exception.t) result] but the
      types work out better having it abstract. *)

  val grant : 'a Capability.t -> resolution
  (** [grant x] is [Ok x]. *)

  val reject : Capnp_rpc.Exception.t -> resolution
  (** [reject x] is [Error x]. *)

  val unknown_service_id : resolution
  (** [unknown_service_id] is a standard rejection message. *)

  (** {2 Restorers} *)

  type t
  (** A restorer looks up live capabilities from service IDs. *)

  val none : t
  (** [none] is a restorer that rejects everything. *)

  val single : Id.t -> 'a Capability.t -> t
  (** [single id cap] is a restorer that responds to [id] with [cap] and
      rejects everything else. *)

  module type LOADER = sig
    type t
    (** A user-provided function to restore services from persistent storage. *)

    val hash : t -> Auth.hash
    (** [hash t] is the hash to apply to a [Restorer.Id.t] to get the storage key,
        which is passed to [load].
        You should use the [hash id] value to find the item. Note that [hash]
        is purely a local security measure - remote peers only see the ID. *)

    val make_sturdy : t -> Id.t -> Uri.t
    (** [make_sturdy t id] converts an ID to a full URI, by adding the
        hosting vat's address and fingerprint. *)

    val load : t -> 'a Sturdy_ref.t -> string -> resolution
    (** [load t sr digest] is called to restore the service with key [digest].
        [sr] is a sturdy ref that refers to the service, which the service
        might want to hand out to clients.
        Note that connecting to [sr] will block until the loader has returned.
        The result is cached until its ref-count reaches zero, so the table
        will never allow two live capabilities for a single [Id.t] at once. It
        will also not call [load] twice in parallel for the same digest. *)
  end

  module Table : sig
    type t
    (** A restorer that keeps a hashtable mapping IDs to capabilities in memory. *)

    val create : (Id.t -> Uri.t) -> t
    (** [create make_sturdy] is a new in-memory-only table.
        [make_sturdy id] converts an ID to a full URI, by adding the
        hosting vat's address and fingerprint. *)

    val of_loader : sw:Eio.Switch.t -> (module LOADER with type t = 'loader) -> 'loader -> t
    (** [of_loader ~sw (module Loader) l] is a new caching table that uses
        [Loader.load l sr (Loader.hash id)] to restore services that aren't in the cache.
        The load function runs in a new fiber in [sw]. *)

    val add : t -> Id.t -> 'a Capability.t -> unit
    (** [add t id cap] adds a mapping to [t].
        It takes ownership of [cap] (it will call [Capability.dec_ref cap] on [clear]). *)

    val sturdy_ref : t -> Id.t -> 'a Sturdy_ref.t
    (** [sturdy_ref t id] is a sturdy ref that can be used to restore service [id]. *)

    val remove : t -> Id.t -> unit
    (** [remove t id] removes [id] from [t].
        It decrements the capability's ref count if it was added manually with [add]. *)

    val clear : t -> unit
    (** [clear t] removes all entries from the table. *)
  end

  val of_table : Table.t -> t

  val restore : t -> Id.t -> ('a Capability.t, Capnp_rpc.Exception.t) result
  (** [restore t id] restores [id] using [t].
      You don't normally need to call this directly, as the Vat will do it automatically. *)
end

module type VAT_NETWORK = S.VAT_NETWORK with
  type 'a capability := 'a Capability.t and
  type restorer := Restorer.t and
  type service_id := Restorer.Id.t and
  type 'a sturdy_ref := 'a Sturdy_ref.t

module Networking (N : S.NETWORK) : VAT_NETWORK with
  module Network = N

module Capnp_address = Capnp_address
