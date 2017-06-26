type 'a brand = ..

module type WIRE = sig
  (** The core RPC logic can be used with different serialisation systems.
      The appropriate types should be provided here. *)

  module Path : sig
    (** A field in a message that refers to a capability. *)

    include Map.OrderedType

    val root : t
    (** The path for the bootstrap capability in the bootstrap message. *)

    val pp : t Fmt.t
  end

  module Request : sig
    type t
    (** The content part of a request message payload.
        This is typically a byte array of some kind (no pointers). *)

    val pp : t Fmt.t

    val cap_index : t -> Path.t -> int option
    (** [cap_index msg path] is the capability index at [path]. *)
  end

  module Response : sig
    type t
    (** The content part of a response message payload.
        This is typically a byte array of some kind (no pointers). *)

    val bootstrap : t
    (** The (empty) content for the reply to the bootstrap message. *)

    val pp : t Fmt.t

    val cap_index : t -> Path.t -> int option
    (** [cap_index msg path] is the capability index at [path]. *)
  end
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

    method blocker : base_ref option
    (** [c#blocker] is the unresolved [cap] or [struct_ref] promise that must resolve for [c] to resolve.
        This is used to help detect cycles. *)

    method check_invariants : unit
    (** This is for debugging. Checks its own invariants and those of other base_refs it holds.
        Raises [Invariant_broken] if there is a problem. *)
  end

  val pp : #base_ref Fmt.t

  class type struct_ref = object
    inherit base_ref

    method when_resolved : ((Wire.Response.t * cap RO_array.t) or_error -> unit) -> unit
    (** [r#when_resolved fn] queues up [fn] to be called on the result, when it arrives.
        If the result has already arrived, [fn] is called immediately. *)

    method response : (Wire.Response.t * cap RO_array.t) or_error option
    (** [r#response] is [Some payload] if the response has arrived,
        or [None] if we're still waiting. *)

    method finish : unit
    (** [r#finish] indicates that this object will never be used again.
        If the results have not yet arrived, we send a cancellation request (which
        may or may not succeed). As soon as the results are available, they are
        released. It is an error to use [r] after calling this. *)

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

  and cap =
    object
      inherit base_ref

      method call : Wire.Request.t -> cap RO_array.t -> struct_ref   (* Takes ownership of [caps] *)
      (** [c#call msg args] invokes a method on [c]'s target and returns a promise for the result. *)

      method inc_ref : unit
      (** [c#inc_ref] increases [c]'s (local) reference count. *)

      method dec_ref : unit
      (** [c#dec_ref] decreases [c]'s (local) reference count. When it reaches zero, [c] must not
          be used again. A message may be sent releasing any remote resources. *)

      method shortest : cap
      (** [c#shortest] is the shortest known path to [cap]. i.e. if [c] is forwarding to another cap, we
          return that, recursively. *)

      method when_more_resolved : (cap -> unit) -> unit
      (** [c#when_more_resolved fn] calls [fn x] when this cap becomes more resolved.
          [fn c] gets a reference to [c] and needs to [dec_ref] it.
          Note that the new capability can be another promise.
          If [c] is already resolved to a value, this does nothing.
          If [c] is forwarding to another cap, it will forward this call. *)

      method sealed_dispatch : 'a. 'a brand -> 'a option
      (** [c#sealed_dispatch brand] extracts some private data of the given type. *)
          
    end
  (** A capability reference to an object that can handle calls.
      We might not yet know its final location, but we may be able
      to pipeline messages to it anyway. *)

  module Request_payload : sig
    type t = Wire.Request.t * cap RO_array.t
    (** The payload of a request or response message. *)

    val field : t -> Wire.Path.t -> (cap, [`Invalid_index of int]) result
    (** [field t path] looks up [path] in the message and returns the capability at that index.
        Returns [Ok null] if the field wasn't set. *)

    val pp : t Fmt.t
  end

  module Response_payload : sig
    type t = Wire.Response.t * cap RO_array.t
    (** The payload of a request or response message. *)

    val field : t -> Wire.Path.t -> (cap, [`Invalid_index of int]) result
    (** [field t path] looks up [path] in the message and returns the capability at that index.
        Returns [Ok null] if the field wasn't set. *)

    val field_or_err : t -> Wire.Path.t -> cap
    (** Like [field], but returns a broken cap on error. *)

    val pp : t Fmt.t
  end

  class type struct_resolver = object
    inherit struct_ref

    method connect : struct_ref -> unit
    (** [r#connect x] causes [r] to behave as [x] in future.
        [r] takes ownership of [x] (is responsible for calling [finish] on it). *)

    method resolve : Response_payload.t or_error -> unit
    (** [r#resolve x] is [r#resolve (return x)]. *)
  end
  (** A [struct_ref] that allows its caller to resolve it. *)

  class virtual ref_counted : object
    method private virtual release : unit
    method virtual pp : Format.formatter -> unit

    method private pp_refcount : Format.formatter -> unit
    (** Write the current ref-count to the formatter (use with ["%t"]). *)

    method private check_refcount : unit
    (** Raise an exception if the ref-count is less than one
        (i.e. check that the object hasn't already been freed). *)

    method inc_ref : unit
    method dec_ref : unit
    method check_invariants : unit

    method virtual blocker : base_ref option
    method virtual call : Wire.Request.t -> cap RO_array.t -> struct_ref
    method virtual shortest : cap
    method virtual when_more_resolved : (cap -> unit) -> unit
    method sealed_dispatch : 'a. 'a brand -> 'a option
  end
  (** A mix-in to help with writing reference-counted objects.
      It will call [self#release] when the ref-count reaches zero. *)

  class virtual service : object
    inherit base_ref
    inherit ref_counted
    method virtual call : Wire.Request.t -> cap RO_array.t -> struct_ref   (* Takes ownership of [caps] *)
    method shortest : cap
    method private release : unit
    method when_more_resolved : (cap -> unit) -> unit
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
end

module type NETWORK_TYPES = sig
  (**  Extends the core types with types related to networking. *)

  include CORE_TYPES

  (* These depend on the particular network details. *)
  type sturdy_ref
  type provision_id
  type recipient_id
  type third_party_cap_id
  type join_key_part
end

