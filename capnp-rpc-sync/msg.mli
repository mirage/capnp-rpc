type request
type response
type 'a msg

module Path : sig
  type t = Xform.t list
  val compare : t -> t -> int
  val pp : t Fmt.t
  val root : t
end

module Request : sig
  include Capnp_rpc.S.WIRE_PAYLOAD with type path := Path.t and type t = request msg

  val writable : t -> Schema.Builder.Call.t
  (** We're about to transmit this message and we need to fill in the target and CapDescriptor table.
      If the message is read-only (e.g. we received it over the network and are forwarding it)
      then make a copy. The copy has no CapDescriptor table and no attachments. *)

  val readable : t -> Schema.Reader.Call.t

  val of_builder : Schema.Builder.Call.t -> t
  val of_reader : Schema.Reader.Call.t -> t
end

module Response : sig
  include Capnp_rpc.S.WIRE_PAYLOAD with type path := Path.t and type t = response msg

  val writable : t -> Schema.Builder.Return.t
  (** We're about to transmit this message and we need to fill in the CapDescriptor table.
      If the message is read-only (e.g. we received it over the network and are forwarding it)
      then make a copy. The copy has no CapDescriptor table and no attachments. *)

  val readable : t -> Schema.Reader.Payload.t

  val of_builder : Schema.Builder.Return.t -> t
  val of_reader : Schema.Reader.Return.t -> t

  val bootstrap : unit -> t
  (** [bootstrap ()] is a fresh bootstrap response. *)
end

val wrap_attachments : Capnp_rpc.S.attachments -> Capnp.MessageSig.attachments
val unwrap_attachments : Capnp.MessageSig.attachments -> Capnp_rpc.S.attachments
