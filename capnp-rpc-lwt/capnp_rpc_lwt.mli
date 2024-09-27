(** Cap'n Proto RPC using the Cap'n Proto serialisation and Eio for concurrency. *)

open Capnp.RPC

include (module type of Capnp.BytesMessage)

module StructRef : sig
  (** A promise for a response structure.
      You can use the generated [_get_pipelined] functions on a promise to get
      a promise for a capability inside the promise, and then pipeline messages
      to that promise without waiting for the response struct to arrive. *)

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
  (** A capability is a reference to an object, or to a promise for an object.
      You can invoke methods on a capability even while it is still only a
      promise. *)

  type +'a t
  (** An ['a t] is a capability reference to a service of type ['a]. *)

  val broken : Capnp_rpc.Exception.t -> 'a t
  (** [broken ex] is a broken capability, with problem [ex].
      Any attempt to call methods on it will fail with [ex]. *)

  val when_broken : (Capnp_rpc.Exception.t -> unit) -> 'a t -> unit
  (** [when_broken fn x] calls [fn problem] when [x] becomes broken.
      If [x] is already broken, [fn] is called immediately.
      If [x] can never become broken (e.g. it is a near ref), this does nothing.
      If [x]'s ref-count reaches zero without [fn] being called, it will never
      be called. *)

  val when_released : 'a t -> (unit -> unit) -> unit
  (** [when_released t fn] will call [fn ()] when [t]'s ref-count drops to zero.
      This is used for caches, to remove entries when they become invalid.
      For promises, [fn] will be transferred to the resolution if resolved.
      For broken caps, this method does nothing (exceptions are never released). *)

  val problem : 'a t -> Capnp_rpc.Exception.t option
  (** [problem t] is [Some ex] if [t] is broken, or [None] if it is still
      believed to be healthy. Once a capability is broken, it will never
      work again and any calls made on it will fail with exception [ex]. *)

  val await_settled : 'a t -> (unit, Capnp_rpc.Exception.t) result
  (** [await_settled t] resolves once [t] is a "settled" (non-promise) reference.
      If [t] is a near, far or broken reference, this returns immediately.
      If it is currently a local or remote promise, it waits until it isn't.
      [wait_until_settled] takes ownership of [t] until it returns (you must not
      [dec_ref] it before then).
      @return [Ok ()] on success, or [Error _] if [t] failed.
      @since 1.2 *)

  val await_settled_exn : 'a t -> unit
  (** Like [await_settled], but raises an exception on error.
      @since 1.2 *)

  val equal : 'a t -> 'a t -> (bool, [`Unsettled]) result
  (** [equal a b] indicates whether [a] and [b] designate the same settled service.
      Returns [Error `Unsettled] if [a] or [b] is still a promise (and they therefore
      may yet turn out to be equal when the promise resolves). *)

  module Request : sig
    type 'a t
    (** An ['a t] is a builder for the out-going request's payload. *)

    val create : ?message_size:int -> (Capnp.Message.rw Slice.t -> 'a) -> 'a t * 'a
    (** [create init] is a fresh request payload and contents builder.
        Use one of the generated [init_pointer] functions for [init].
        @param message_size An estimate of the size of the payload. If this is too small,
                            additional segments will be allocated automatically, but this
                            is less efficient than getting the size right to start with. *)

    val create_no_args : unit -> 'a t
    (** [create_no_args ()] is a payload with no content. *)

    val release : 'a t -> unit
    (** Clear the exported refs, dropping their ref-counts. This is called automatically
        when you send a message, but you might need it if you decide to abort. *)
  end

  val call : 't t -> ('t, 'a, 'b) MethodID.t -> 'a Request.t -> 'b StructRef.t
  (** [call target m req] invokes [target#m req] and returns a promise for the result.
      Messages may be sent to the capabilities that will be in the result
      before the result arrives - they will be pipelined to the service
      responsible for resolving the promise. The caller must call [StructRef.dec_ref]
      when finished with the result (consider using one of the [call_*] functions below
      instead for a simpler interface). *)

  val call_and_wait : 't t -> ('t, 'a, 'b StructStorage.reader_t) MethodID.t ->
    'a Request.t -> (('b StructStorage.reader_t * (unit -> unit)), [> `Capnp of Capnp_rpc.Error.t]) result
  (** [call_and_wait t m req] does [call t m req] and waits for the response.
      This is simpler than using [call], but doesn't support pipelining
      (you can't use any capabilities in the response in another message until the
      response arrives).
      On success, it returns [Ok (response, release_response_caps)].
      Call [release_response_caps] when done with the results, to release any capabilities it might
      contain that you didn't use (remembering that future versions of the protocol might add
      new optional capabilities you don't know about yet).
      If you don't need any capabilities from the result, consider using [call_for_value] instead.
      Cancelling the fiber will send a cancel message to the target for remote calls. *)

  val call_for_value : 't t -> ('t, 'a, 'b StructStorage.reader_t) MethodID.t ->
    'a Request.t -> ('b StructStorage.reader_t, [> `Capnp of Capnp_rpc.Error.t]) result
  (** [call_for_value t m req] is similar to [call_and_wait], but automatically
      releases any capabilities in the response before returning. Use this if
      you aren't expecting any capabilities in the response. *)

  val call_for_value_exn : 't t -> ('t, 'a, 'b StructStorage.reader_t) MethodID.t ->
    'a Request.t -> 'b StructStorage.reader_t
  (** Wrapper for [call_for_value] that turns errors into exceptions. *)

  val call_for_unit : 't t -> ('t, 'a, 'b StructStorage.reader_t) MethodID.t ->
    'a Request.t -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result
  (** Wrapper for [call_for_value] that ignores the result. *)

  val call_for_unit_exn : 't t -> ('t, 'a, 'b StructStorage.reader_t) MethodID.t ->
    'a Request.t -> unit
  (** Wrapper for [call_for_unit] that turns errors into exceptions. *)

  val call_for_caps : 't t -> ('t, 'a, 'b StructStorage.reader_t) MethodID.t ->
    'a Request.t -> ('b StructRef.t -> 'c) -> 'c
  (** [call_for_caps target m req extract] is a wrapper for [call] that passes the results promise to
      [extract], which should extract any required capability promises from it.
      In the common case where you want a single cap "foo" from the result, use
      [call_for_caps target m req Results.foo_get_pipelined].
      When the remote call finally returns, the result will be released automatically. *)

  type 'a resolver
  (** An ['a resolver] can be used to resolve a promise for an ['a]. It can only be used once. *)

  val promise : unit -> 'a t * 'a resolver
  (** [promise ()] returns a fresh local promise and a resolver for it.
      Any calls made on the promise will be queued until it is resolved. *)

  val resolve_ok : 'a resolver -> 'a t -> unit
  (** [resolve_ok r x] resolves [r]'s promise to [x]. [r] takes ownership of [x]
      (the caller must use [inc_ref] first if they want to continue using it). *)

  val resolve_exn : 'a resolver -> Capnp_rpc.Exception.t -> unit
  (** [resolve_exn r x] breaks [r]'s promise with exception [x]. *)

  val inc_ref : _ t -> unit
  (** [inc_ref t] increases the ref-count on [t] by one. *)

  val dec_ref : _ t -> unit
  (** [dec_ref t] decreases the ref-count on [t] by one. When the count reaches zero,
      the capability is released. This may involve sending a notification to a remote
      peer. Any time you extract a capability from a struct or struct promise,
      it must eventually be freed by calling [dec_ref] on it. *)

  val with_ref : 'a t -> ('a t -> 'b) -> 'b
  (** [with_ref t fn] runs [fn t] and then calls [dec_ref t] (whether [fn]
      succeeds or not). *)

  val pp : 'a t Fmt.t
end

module Sturdy_ref : sig
  type +'a t
  (** An off-line (persistent) capability reference.

      A sturdy ref contains all the information necessary to get a live reference to a service:

      - The network address of the hosting vat (e.g. TCP host and port)
      - A way to authenticate the hosting vat (e.g. a fingerprint of the vat's public key)
      - A way to identify the target service within the vat and prove permission to access it
        (e.g. a "Swiss number")
    *)

  val connect : 'a t -> ('a Capability.t, Capnp_rpc.Exception.t) result
  (** [connect t] returns a live reference to [t]'s service. *)

  val connect_exn : 'a t -> 'a Capability.t
  (** [connect_exn] is a wrapper for [connect] that raises an exception on error. *)

  val with_cap :
    'a t ->
    ('a Capability.t -> ('b, [> `Capnp of Capnp_rpc.Exception.t] as 'e) result) ->
    ('b, 'e) result
  (** [with_cap t f] uses [connect t] to get a live-ref [x],
      then does [Capability.with_ref x f]. *)

  val with_cap_exn : 'a t -> ('a Capability.t -> 'b) -> 'b
  (** [with_cap_exn t f] uses [connect_exn t] to get a live-ref [x],
      then does [Capability.with_ref x f]. *)

  val reader :
    ('a StructStorage.reader_t -> Capnp.MessageSig.ro Slice.t option) ->
    'a StructStorage.reader_t -> Uri.t
  (** [reader accessor] is a field accessor for reading a sturdy ref.
      e.g. if [sr_get] is a generated field accessor for an AnyPointer field, then
      [reader Reader.Struct.sr_get] is an accessor that treats it as a SturdyRef field.
      todo: This should really return a sturdy ref, not a URI, but that requires a change
      to the spec to add a sturdy ref cap-descriptor table entry type. *)

  val builder :
    ('a StructStorage.builder_t -> Capnp.MessageSig.rw Slice.t) ->
    'a StructStorage.builder_t -> _ t -> unit
  (** [builder setter] converts a generated AnyPointer field setter [setter] to a SturdyRef
      setter. Use it to add a SturdyRef to a message with [builder Params.sr_get params sr]. *)

  val cast : 'a t -> 'b t
end

module Service : sig
  (** Functions for service implementors. *)

  type ('a, 'b) method_t = 'a -> (unit -> unit) -> 'b StructRef.t
  (** An ('a, 'b) method_t is a method implementation that takes
      a reader for the parameters and
      a function to release the capabilities in the parameters,
      and returns a promise for the results. *)

  module Response : sig
    type 'b t
    (** An ['a t] is a builder for the out-going response. *)

    val create : ?message_size:int -> (Capnp.Message.rw Slice.t -> 'a) -> 'a t * 'a
    (** [create init] is a fresh response and contents builder.
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

  val fail : ?ty:Capnp_rpc.Exception.ty -> ('a, Format.formatter, unit, 'b StructRef.t) format4 -> 'a
  (** [fail msg] is an exception with reason [msg]. *)

  val error : Capnp_rpc.Error.t -> 'a StructRef.t
end

(**/**)

module Untyped : sig
  (** This module is only for use by the code generated by the capnp-ocaml
      schema compiler. The generated code provides type-safe wrappers for
      everything here. *)

  type abstract_method_t

  val abstract_method : ('a StructStorage.reader_t, 'b) Service.method_t -> abstract_method_t

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

module Private = Private

module Cast : sig
  val cap_of_raw : Capnp_core.Core_types.cap -> 'a Capability.t
  val cap_to_raw : 'a Capability.t -> Capnp_core.Core_types.cap

  val sturdy_of_raw : Capnp_core.sturdy_ref -> 'a Sturdy_ref.t
  val sturdy_to_raw : 'a Sturdy_ref.t -> Capnp_core.sturdy_ref
end

(**/**)

module Persistence : sig
  class type ['a] persistent = object
    method save : ('a Sturdy_ref.t, Capnp_rpc.Exception.t) result
  end

  val with_persistence :
    ('a #persistent) ->
    ('impl -> 'a Capability.t) ->
    (#Untyped.generic_service as 'impl) ->
    'a Capability.t
  (** [with_persistence persist Service.Foo.local obj] is like [Service.Foo.local obj], but the
      resulting service also handles the Cap'n Proto persistence protocol, using [persist]. *)

  val with_sturdy_ref :
    'a Sturdy_ref.t ->
    ('impl -> 'a Capability.t) ->
    (#Untyped.generic_service as 'impl) ->
    'a Capability.t
  (** [with_sturdy_ref sr Service.Foo.local obj] is like [Service.Foo.local obj],
      but responds to [save] calls by returning [sr]. *)

  val save : 'a Capability.t -> (Uri.t, [> `Capnp of Capnp_rpc.Error.t]) result
  (** [save cap] calls the persistent [save] method on [cap].
      Note that not all capabilities can be saved.
      todo: this should return an ['a Sturdy_ref.t]; see {!Sturdy_ref.reader}. *)

  val save_exn : 'a Capability.t -> Uri.t
  (** [save_exn] is a wrapper for [save] that returns a failed thread on error. *)
end
