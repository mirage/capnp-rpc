(** The core types and module signatures. *)

type 'a brand = ..
(** A way for objects to provide an optional private API to certain other modules.
    For example, CapTP proxies appear as plain services, but use this to reveal their
    target details to the CapTP layer so that it can shorten the path when sending
    such capabilties over the network. *)

type attachments = ..
(** The capabilities attached to a message.
    This is only defined as an open type to avoid a nasty recursive type definition. *)

type attachments += No_attachments

module type WIRE_PAYLOAD = sig
  type t
  (** A message payload.
      This is typically a byte array of some kind, plus a way of attaching capabilities. *)

  type path

  val pp : t Fmt.t

  val cap_index : t -> path -> int option
  (** [cap_index msg path] is the capability index at [path] in the message
      (i.e the index of the capability in the attachments table). *)

  val attachments : t -> attachments
  val with_attachments : attachments -> t -> t
end

module type WIRE = sig
  (** The core RPC logic can be used with different serialisation systems.
      The appropriate types should be provided here. *)

  type request
  type response
  type 'a msg

  module Path : sig
    (** A field in a message that refers to a capability. *)

    include Map.OrderedType

    val root : t
    (** The path for the bootstrap capability in the bootstrap message. *)

    val pp : t Fmt.t
  end

  module Request : WIRE_PAYLOAD with type t = request msg and type path := Path.t

  module Response : sig
    include WIRE_PAYLOAD with type t = response msg and type path := Path.t

    val bootstrap : unit -> t
    (** The (empty) content for the reply to the bootstrap message. *)
  end

  val ref_leak_detected : (unit -> unit) -> unit
  (** [ref_leak_detected fn] is called when a promise or capability is GC'd while
      its ref-count is non-zero, indicating that resources may have been leaked.
      [fn ()] will log a warning about this and free the resources itself.
      The reason for going via [ref_leak_detected] rather than calling [fn] directly
      is because the OCaml GC may detect the problem at any point (e.g. while we're
      sending another message). The implementation should arrange for [fn] to be
      called at a safe point (e.g. when returning to the main loop).
      Unit-tests may wish to call [fn] immediately to show the error and then
      fail the test. *)
end

module type PAYLOAD = sig
  (** Wraps {!WIRE_PAYLOAD} to deal with caps rather than attachments. *)

  type t
  type cap
  type path

  val snapshot_caps : t -> cap RO_array.t

  val field : t -> path -> cap option
  (** [field t path] looks up [path] in the message and returns the capability at that index.
      Returns [None] if the field wasn't set. Returns a broken capability if an index was
      given but does not exist (i.e. the message is corrupted). Increases the ref-count on the result. *)

  val with_caps : cap RO_array.t -> t -> t
  (** [with_caps caps t] is a copy of [t] with a new set of capabilities.
      This is useful to implement [TakeFromOtherQuestion], where the message is the same but
      embargoes may be needed, and to break cycles. *)

  val release : t -> unit
  (** [release t] frees all the capabilities attached to this message.
      It is safe to call this multiple times; only the first call has any effect. *)

  val pp : t Fmt.t
end

module type CORE_TYPES = sig
  (** This module defines a calling convention for invoking methods on objects.
      The objects could be remote, but this module doesn't define anything related
      to networks.
      These types are generated automatically from [WIRE] by [Core_types]. *)

  module Wire : WIRE

  type 'a or_error = ('a, Error.t) result

  class type base_ref = object
    method pp : Format.formatter -> unit

    method update_rc : int -> unit
    (** [c#update_rc d] adds [d] to [c]'s (local) reference count.
        When it reaches zero, [c] must not be used again. A message may be
        sent releasing any remote resources. *)

    method blocker : base_ref option
    (** [c#blocker] is the unresolved [cap] or [struct_ref] promise that must resolve for [c] to resolve.
        This is used to help detect cycles. *)

    method check_invariants : unit
    (** This is for debugging. Checks its own invariants and those of other base_refs it holds.
        Raises [Invariant_broken] if there is a problem. *)

    method sealed_dispatch : 'a. 'a brand -> 'a option
    (** [c#sealed_dispatch brand] extracts some private data of the given type. *)
  end
  (** Common methods for [struct_ref] and [cap]. *)

  val pp : #base_ref Fmt.t

  val inc_ref : #base_ref -> unit
  (** [inc_ref x] increases [x]'s ref-count by one. *)

  val dec_ref : #base_ref -> unit
  (** [dec_ref x] decreases [x]'s ref-count by one. *)

  class type struct_ref = object
    inherit base_ref

    method when_resolved : (Wire.Response.t or_error -> unit) -> unit
    (** [r#when_resolved fn] queues up [fn] to be called on the result, when it arrives.
        If the result has already arrived, [fn] is called immediately. *)

    method response : Wire.Response.t or_error option
    (** [r#response] is [Some payload] if the response has arrived,
        or [None] if we're still waiting. *)

    method cap : Wire.Path.t -> cap
    (** [r#cap path] is the capability found at [path] in the response.
        If the response has arrived, this will extract the capability from it.
        If not, it may create a capability that will pipeline through the promised
        answer until the result arrives (at which point it will use the new, more
        direct route). The caller should call [cap#dec_ref] when done.  *)
  end
  (** The result of a call, which may not have arrived yet.
      It can be used to pipeline calls to capabilities that we hope will
      be returned in the results. *)

  and cap = object
    inherit base_ref

    method call : struct_resolver -> Wire.Request.t -> unit   (* Takes ownership of [caps] *)
    (** [c#call results msg] invokes a method on [c]'s target and eventually resolves [results]
        with the answer. *)

    method shortest : cap
    (** [c#shortest] is the shortest known path to [cap]. i.e. if [c] is forwarding to another cap, we
        return that, recursively. *)

    method when_more_resolved : (cap -> unit) -> unit
    (** [c#when_more_resolved fn] calls [fn x] when this cap becomes more resolved.
        [fn x] gets a reference to [x] and needs to [dec_ref] it.
        Note that the new capability can be another promise.
        If [c] is already resolved to its final value, this does nothing.
        If [c] is a far-ref, [fn x] will be called when it breaks.
        If [c] is forwarding to another cap, it will forward this call.
        If [c] gets released before calling [fn], it will never call it. *)

    method when_released : (unit -> unit) -> unit
    (** [c#when_released fn] will call [fn ()] when [c]'s ref-count drops to zero.
        This is used for caches, to remove entries when they become invalid.
        For promises, [fn] will be transferred to the resolution if resolved.
        For broken caps, this method does nothing (exceptions are never released). *)

    method problem : Exception.t option
    (** [c#problem] is the exception for a broken reference, or [None] if it is not known to be broken. *)

  end
  (** A capability reference to an object that can handle calls.
      We might not yet know its final location, but we may be able
      to pipeline messages to it anyway. *)

  and struct_resolver = object
    method pp : Format.formatter -> unit

    method resolve : struct_ref -> unit
    (** [r#resolve x] causes [r]'s promise to behave as [x] in future.
        The promise takes ownership of [x] (is responsible for calling [dec_rec] on it). *)

    method sealed_dispatch : 'a. 'a brand -> 'a option
    (** [r#sealed_dispatch brand] extracts some private data of the given type. *)

    method set_blocker : base_ref -> (unit, [> `Cycle]) result
    (** [r#set_blocker b] means that [resolve] won't be called until [b] is resolved.
        [r]'s promise should report this as its blocker. This is needed to detect cycles.
        When the blocker is resolved, call this again with [None] to clear it (the promise
        will then report itself as the blocker again, until resolved). *)

    method clear_blocker : unit
    (** [r#clear_blocker] removes the blocker set by [set_blocker].
        [r] is then blocked by itself, if unresolved. *)
  end
  (** A [struct_resolver] can be used to resolve some promise. *)

  module Attachments : sig
    val builder : unit -> attachments
    (** [builder ()] is a fresh writable attachments array. *)

    val cap : int -> attachments -> cap
    (** [cap i t] is the capability at index [i] in [t]. The reference count is increased by one. *)

    val clear_cap : attachments -> int -> unit
    (** Replace cap at index [i] with [null] and dec_ref it. *)

    val add_cap : attachments -> cap -> int
    (** [add_cap t cap] stores [cap] in [t]. [t] must have been created by [builder].
        Increases the ref-count on [cap] by one. *)

    val release_caps : attachments -> unit
    (** [release_caps a] decreases the ref-count of every capability in [a]. *)
  end

  module Request_payload : PAYLOAD with type t = Wire.Request.t and type cap := cap and type path := Wire.Path.t
  (** The payload of a request message. *)

  module Response_payload : PAYLOAD with type t = Wire.Response.t and type cap := cap and type path := Wire.Path.t
  (** The payload of a response message. *)

  class virtual ref_counted : object
    method private virtual release : unit
    method virtual pp : Format.formatter -> unit

    method private pp_refcount : Format.formatter -> unit
    (** Write the current ref-count to the formatter (use with ["%t"]). *)

    method private check_refcount : unit
    (** Raise an exception if the ref-count is less than one
        (i.e. check that the object hasn't already been freed). *)

    method update_rc : int -> unit
    method check_invariants : unit

    method virtual blocker : base_ref option
    method sealed_dispatch : 'a. 'a brand -> 'a option
  end
  (** A mix-in to help with writing reference-counted objects.
      It will call [self#release] when the ref-count reaches zero. *)

  class virtual service : object
    inherit base_ref
    inherit ref_counted
    method virtual call : struct_resolver -> Wire.Request.t -> unit (* Takes ownership of message. *)
    method shortest : cap
    method private release : unit
    method when_more_resolved : (cap -> unit) -> unit
    method when_released : (unit -> unit) -> unit
    method problem : Exception.t option
  end
  (** A convenience base class for creating local services.
      The capability is always resolved, and the default [release] method does nothing. *)

  val null : cap
  (** A (broken) capability representing a missing pointer.
      Any attempt to call it will return an error. *)

  val return : Response_payload.t -> struct_ref
  (** [return x] is a resolved [struct_ref] with successful resolution [x]. *)

  val fail : ?ty:Exception.ty -> ('a, Format.formatter, unit, struct_ref) format4 -> 'a
  (** [fail fmt] is a [struct_ref] that is broken with the given capnp exception message. *)

  val broken_cap : Exception.t -> cap
  (** [broken_cap ex] is a [cap] that is broken with the given exception. *)

  val broken_struct : Error.t -> struct_ref
  (** [broken_struct err] is a [struct_ref] that is broken with the given error. *)

  val resolved : Response_payload.t or_error -> struct_ref
  (** [resolved x] is a resolved [struct_ref] with resolution [x]. *)

  val resolve_payload : #struct_resolver -> Response_payload.t or_error -> unit
  (** [resolve_payload r x] is [r#resolve (resolved x)]. [r] takes ownership of [x]. *)

  val resolve_ok : #struct_resolver -> Wire.Response.t -> unit
  (** [resolve_ok r msg] is [resolve_payload r (Ok msg)].
      [r] takes ownership of [msg]. *)

  val resolve_exn : #struct_resolver -> Exception.t -> unit
  (** [resolve_exn r exn] is [resolve_payload r (Error (`Exception exn))]. *)

  val when_broken : (Exception.t -> unit) -> cap -> unit
  (** [when_broken fn x] calls [fn problem] when [x] becomes broken.
      If [x] is already broken, [fn] is called immediately.
      If [x] can never become broken (e.g. it is a near ref), this does nothing. *)
end

module type NETWORK_TYPES = sig
  (* These depend on the particular network details. *)
  type provision_id
  type recipient_id
  type third_party_cap_id
  type join_key_part
end
