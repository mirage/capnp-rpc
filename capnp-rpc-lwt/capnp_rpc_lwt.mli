open Capnp.RPC

module Message = Capnp.Message.BytesMessage

type 'a or_error = ('a, Capnp_rpc.Error.t) result

type 'a reader_t = 'a Message.StructStorage.reader_t

module StructRef : sig
  type 'a t
  (** An ['a t] is a reference to a response message (that may not have arrived yet)
      with content type ['a]. *)

  val inc_ref : 'a t -> unit
  (** [inc_ref t] increases the reference count on [t] by one. *)

  val dec_ref : 'a t -> unit
  (** [dec_ref t] reduces the reference count on [t] by one.
      When the count reaches zero, this result must never be used again.
      If the results have not yet arrived when the count reaches zero, we send
      a cancellation request (which may or may not succeed). As soon as the
      results are available, they are released. *)
end

module Capability : sig
  type 'a t
  (** An ['a t] is a capability reference to a service of type ['a]. *)

  type 'a capability_t = 'a t (* (alias because we have too many t's) *)

  module Request : sig
    type 'a t
    (** An ['a t] is a builder for the out-going request's payload. *)

    val create : (Capnp.Message.rw Message.Slice.t -> 'a) -> 'a t * 'a
    (** [create init] is a fresh request payload and contents builder.
        Use one of the generated [init_pointer] functions for [init]. *)

    val create_no_args : unit -> 'a t
    (** [create_no_args ()] is a payload with no content. *)

    val release : 'a t -> unit
    (** Clear the exported refs, dropping their ref-counts. This is called automatically
        when you send a message, but you might need it if you decide to abort. *)
  end

  val call : 't capability_t -> ('t, 'a, 'b) MethodID.t -> 'a Request.t -> 'b StructRef.t
  (** [call m req] invokes [m req] and returns a promise for the result.
      Messages may be sent to the capabilities that will be in the result
      before the result arrives - they will be pipelined to the service
      responsible for resolving the promise. The caller must call [StructRef.dec_ref]
      when finished with the result (consider using one of the [call_for_*] functions
      instead for a simpler interface). *)

  val call_and_wait : 't capability_t -> ('t, 'a, 'b reader_t) MethodID.t -> 'a Request.t ->
    ('b reader_t * (unit -> unit)) or_error Lwt.t
  (** [call_and_wait m req] invokes [m req] and waits for the response.
      This is simpler than using [call], but doesn't support pipelining
      (you can't use any capabilities in the response in another message until the
      response arrives).
      On success, it returns [Ok (response, release_response_caps)].
      Call [release_response_caps] when done with the results, to release any capabilities it might
      contain that you didn't use (remembering that future versions of the protocol might add
      new optional capabilities you don't know about yet).
      If you don't need any capabilities from the result, consider using [call_for_value] instead.
      Doing [Lwt.cancel] on the result will send a cancel message to the target
      for remote calls. *)

  val call_for_value : 't capability_t -> ('t, 'a, 'b reader_t) MethodID.t -> 'a Request.t -> 'b reader_t or_error Lwt.t
  (** [call_for_value m req] is similar to [call_and_wait], but automatically
      releases any capabilities in the response before returning. Use this if
      you aren't expecting any capabilities in the response. *)

  val call_for_value_exn : 't capability_t -> ('t, 'a, 'b reader_t) MethodID.t -> 'a Request.t -> 'b reader_t Lwt.t
  (** Wrapper for [call_for_value] that turns errors in Lwt failures. *)

  val call_for_unit : 't capability_t -> ('t, 'a, 'b reader_t) MethodID.t -> 'a Request.t -> unit or_error Lwt.t
  (** Wrapper for [call_for_value] that ignores the result. *)

  val call_for_unit_exn : 't capability_t -> ('t, 'a, 'b reader_t) MethodID.t -> 'a Request.t -> unit Lwt.t
  (** Wrapper for [call_for_unit] that raises an exception on error. *)

  val call_for_caps : 't capability_t -> ('t, 'a, 'b) MethodID.t -> 'a Request.t -> ('b StructRef.t -> 'c) -> 'c
  (** [call_for_caps] is a wrapper for [call] that passes the results to a
      callback and finishes them automatically when it returns.
      In the common case where you want a single cap "foo" from the result, use
      [call_for_caps target meth req R.foo_get_pipelined]. *)

  type 'a resolver
  (** An ['a resolver] can be used to resolve a promise for an ['a]. It can only be used once. *)

  val promise : unit -> 't capability_t * 'a resolver
  (** [promise () returns a fresh local promise and a resolver for it.
      Any calls made on the promise will be queued until it is resolved. *)

  val resolve_ok : 'a resolver -> 'a capability_t -> unit
  (** [resolve_ok r x] resolves [r]'s promise to [x]. [r] takes ownership of [x]
      (the caller must use [inc_ref] first if they want to continue using it). *)

  val resolve_exn : 'a resolver -> Capnp_rpc.Exception.t -> unit
  (** [resolve_exn r x] breaks [r]'s promise with exception [x]. *)

  val inc_ref : _ t -> unit

  val dec_ref : _ t -> unit

  val pp : 'a t Fmt.t
end

module Service : sig
  type ('a, 'b) method_t = 'a -> (unit -> unit) -> 'b StructRef.t
  (** An ('a, 'b) method_t is a method implementation that takes
      a reader for the parameters and
      a function to release the capabilities in the parameters,
      and returns a promise for the results. *)

  module Response : sig
    type 'b t
    (** An ['a t] is a builder for the out-going response's payload. *)

    val create : (Capnp.Message.rw Message.Slice.t -> 'a) -> 'a t * 'a
    (** [create init] is a fresh request payload and contents builder.
        Use one of the generated [init_pointer] functions for [init]. *)

    val create_empty : unit -> 'a t
    (** [empty ()] is an empty response. *)

    val release : 'a t -> unit
    (** Clear the exported refs, dropping their ref-counts. This is called automatically
        when you send a message, but you might need it if you decide to abort. *)
  end

  val return : 'a Response.t -> 'a StructRef.t
  (** [return r] wraps up a simple local result as a promise. *)

  val return_empty : unit -> 'a StructRef.t
  (** [return_empty ()] is a promise for a response with no payload. *)

  val return_lwt : (unit -> 'a Response.t or_error Lwt.t) -> 'a StructRef.t
  (** [return_lwt fn] is a local promise for the result of Lwt thread [fn ()].
      If [fn ()] fails, the error is logged and an "Internal error" returned to the caller.
      Note that this does not support pipelining. *)

  val fail : ?ty:Capnp_rpc.Exception.ty -> ('a, Format.formatter, unit, 'b StructRef.t) format4 -> 'a
  (** [fail msg] is an exception with reason [msg]. *)
end

module Untyped : sig
  (** This module is only for use by the code generated by the capnp-ocaml
      schema compiler. The generated code provides type-safe wrappers for
      everything here. *)

  type abstract_method_t

  type 'a reader_t = 'a Message.StructStorage.reader_t

  val abstract_method : ('a reader_t, 'b) Service.method_t -> abstract_method_t

  val struct_field : 'a StructRef.t -> int -> 'b StructRef.t

  val capability_field : 'a StructRef.t -> int -> 'b Capability.t

  class type generic_service = object
    method dispatch : interface_id:Uint64.t -> method_id:int -> abstract_method_t
    method release : unit
    method pp : Format.formatter -> unit
  end

  val local : #generic_service -> 'a Capability.t

  val get_cap : Capnp.MessageSig.attachments -> Uint32.t -> _ Capability.t
  val add_cap : Capnp.MessageSig.attachments -> _ Capability.t -> Uint32.t
  val clear_cap : Capnp.MessageSig.attachments -> Uint32.t -> unit

  val unknown_interface : interface_id:Uint64.t -> abstract_method_t
  val unknown_method : interface_id:Uint64.t -> method_id:int -> abstract_method_t
end

module CapTP : sig
  type t
  (** A CapTP connection to a remote peer. *)

  val connect : ?offer:'a Capability.t -> ?tags:Logs.Tag.set -> switch:Lwt_switch.t -> Endpoint.t -> t
  (** [connect ?offer ~switch endpoint] is fresh CapTP protocol handler that sends and
      receives messages using [endpoint].
      If [offer] is given, the peer can use the "Bootstrap" message to get access to it.
      If the connection fails then [switch] will be turned off, and turning off the switch
      will release all resources used by the connection. *)

  val bootstrap : t -> 'a Capability.t
  (** [bootstrap t] is the peer's public bootstrap object, if any. *)

  val disconnect : t -> Capnp_rpc.Exception.t -> unit Lwt.t
  (** [disconnect reason] closes the connection, sending [reason] to the peer to explain why.
      Capabilities and questions at both ends will break, with [reason] as the problem. *)

  val dump : t Fmt.t
  (** [dump] dumps the state of the connection, for debugging. *)
end

module Endpoint = Endpoint

module Vat : sig
  type t

  val create : ?switch:Lwt_switch.t -> ?bootstrap:'a Capability.t -> unit -> t
  (** [create ~switch ~bootstrap ()] is a new vat that offers [bootstrap] to its connections.
      The vat takes ownership of [bootstrap], and will release it when the switch is turned off.
      Turning off the switch will also disconnect any active connections. *)

  val connect : t -> Endpoint.t -> CapTP.t
  (** [connect t endpoint] runs the CapTP protocol over [endpoint]. *)
end
