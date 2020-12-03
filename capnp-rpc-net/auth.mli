open Asetmap

(** Vat-level authentication and encryption.

    Unless your network provides a secure mechanism for establishing connections
    to other vats, where you can be sure of the identity of the other party,
    you'll probably want to enable cryptographic security.

    Each vat (application instance) should generate a secret key when it is first
    deployed. For servers at least, this key must be saved to disk so that the
    server retains its identity over re-starts. Otherwise, clients will think it
    is an imposter and refuse to connect.

    Clients that do not accept incoming connections, nor create SturdyRefs, can
    get away with creating a new key each time. However, it might be quicker
    to save and reload the key anyway. *)

type hash = [`SHA256]
(** Supported hashes. *)

module Digest : sig
  type t
  (** The digest of a public key, used to recognise a vat.
      This appears in URIs as e.g. 'capnp://sha256:1234@host/'. *)

  val insecure : t
  (** A special value indicating no authentication should be performed. *)

  val from_uri : Uri.t -> (t, [> `Msg of string]) result
  (** [from_uri t] is the parsed digest information in [t]. *)

  val add_to_uri : t -> Uri.t -> Uri.t
  (** [add_to_uri t uri] is [uri] with the [user] and [password] fields set
      to the correct values for [t]. Note that although we use the "password" field,
      this is not secret. *)

  val authenticator : t -> X509.Authenticator.t option
  (** [authenticator t] is an authenticator that checks that the peer's public key
      matches [t]. Returns [None] if [t] is [insecure].
      Note: it currently also requires the DN field to be "capnp". *)

  val of_certificate : X509.Certificate.t -> t
  (** [of_certificate cert] is a digest of [cert]'s public key. *)

  val equal : t -> t -> bool

  val pp : t Fmt.t

  module Map : Map.S with type key = t
end

module Secret_key : sig
  type t
  (** A vat's [secret_key] allows it to prove its identity to other vats. *)

  val generate : unit -> t
  (** [generate ()] is a fresh secret key.
      You must call the relevant entropy initialization function
      (e.g. {!Mirage_crypto_rng_lwt.initialize}) before using this, or it
      will raise an error if you forget. *)

  val digest : ?hash:hash -> t -> Digest.t
  (** [digest ~hash t] is the digest of [t]'s public key, using [hash]. *)

  val of_pem_data : string -> t
  (** [of_pem_data data] parses [data] as a PEM-encoded private key. *)

  val to_pem_data : t -> string
  (** [to_pem_data t] returns [t] as a PEM-encoded private key. *)

  val tls_client_config : t -> authenticator:X509.Authenticator.t -> Tls.Config.client
  (** [tls_client_config t ~authenticator] is the TLS client configuration to
      use for a vat with secret key [t], attempting to connect to a vat that
      can be authenticated with [authenticator]. *)

  val tls_server_config : t -> Tls.Config.server
  (** [tls_server_config t] is the TLS server configuration to use for a vat with secret key [t]. *)

  val pp_fingerprint : hash -> t Fmt.t
  (** [pp_fingerprint hash] formats the hash of [t]'s public key. *)

  val equal : t -> t -> bool
end
