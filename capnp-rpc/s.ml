module type CONCRETE = sig
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

    val cap_index : t -> Path.t -> int
    (** [cap_index msg path] is the capability index at [path]. *)
  end

  module Response : sig
    type t
    (** The content part of a response message payload.
        This is typically a byte array of some kind (no pointers). *)

    val bootstrap : t
    (** The (empty) content for the reply to the bootstrap message. *)

    val pp : t Fmt.t

    val cap_index : t -> Path.t -> int
    (** [cap_index msg path] is the capability index at [path]. *)
  end
end

module type TABLE_TYPES = sig
  (** For the unit tests it is convenient to pass in the types of table indexes.
      This allows the tests to make both ends of a connection, with the types
      matched up. *)

  module QuestionId : Id.S
  module AnswerId : Id.S
  module ImportId : Id.S
  module ExportId : Id.S
end

module type NETWORK_TYPES = sig
  (** These depend on the particular network details. *)

  type sturdy_ref
  type provision_id
  type recipient_id
  type third_party_cap_id
  type join_key_part
end

module type CORE_TYPES = sig
  (** These types are generated automatically from [CONCRETE] by [Core_types]. *)

  include CONCRETE

  type 'a or_error = ('a, Error.t) result

  class type struct_ref = object
    method when_resolved : ((Response.t * cap RO_array.t) or_error -> unit) -> unit
    (** [r#when_resolved fn] queues up [fn] to be called on the result, when it arrives.
        If the result has already arrived, [fn] is called immediately. *)

    method response : (Response.t * cap RO_array.t) or_error option
    (** [r#response] is [Some payload] if the response has arrived,
        or [None] if we're still waiting. *)

    method pp : Format.formatter -> unit

    method finish : unit
    (** [r#finish] indicates that this object will never be used again.
        If the results have not yet arrived, we send a cancellation request (which
        may or may not succeed). As soon as the results are available, they are
        released. It is an error to use [r] after calling this. *)

    method cap : Path.t -> cap
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
      method call : Request.t -> cap RO_array.t -> struct_ref   (* Takes ownership of [caps] *)
      (** [c#call msg args] invokes a method on [c]'s target and returns a promise for the result. *)

      method inc_ref : unit
      (** [c#inc_ref] increases [c]'s (local) reference count. *)

      method dec_ref : unit
      (** [c#dec_ref] decreases [c]'s (local) reference count. When it reaches zero, [c] must not
          be used again. A message may be sent releasing any remote resources. *)

      method shortest : cap
      (** [c#shortest] is the shortest known path to [cap]. i.e. if [c] is forwarding to another cap, we
          return that, recursively. *)

      method pp : Format.formatter -> unit
    end
  (** A capability reference to an object that can handle calls.
      We might not yet know its final location, but we may be able
      to pipeline messages to it anyway. *)

  module Request_payload : sig
    type t = Request.t * cap RO_array.t
    (** The payload of a request or response message. *)

    val field : t -> Path.t -> cap
    (** [field t path] looks up [path] in the message and returns the capability at that index. *)

    val pp : t Fmt.t
  end

  module Response_payload : sig
    type t = Response.t * cap RO_array.t
    (** The payload of a request or response message. *)

    val field : t -> Path.t -> cap
    (** [field t path] looks up [path] in the message and returns the capability at that index. *)

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
    val mutable ref_count : int
    method private virtual release : unit
    method virtual pp : Format.formatter -> unit
    method inc_ref : unit
    method dec_ref : unit
  end
  (** A mix-in to help with writing reference-counted objects.
      It will call [self#release] when the ref-count reaches zero. *)

  val null : cap
  (** A (broken) capability representing a missing pointer.
      Any attempt to call it will return an error. *)

  val return : Response_payload.t -> struct_ref
  (** [return x] is a resolved [struct_ref] with successful resolution [x]. *)

  val resolved : Response_payload.t or_error -> struct_ref
  (** [resolved x] is a resolved [struct_ref] with resolution [x]. *)
end
