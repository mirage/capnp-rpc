module Make(C : S.CORE_TYPES) : sig
  class type embargo_cap = object
    inherit C.cap
    method disembargo : unit
  end

  class local_promise : object
    inherit C.cap
    method resolve : C.cap -> unit
  end
  (** A [new local_promise] is a promise that buffers calls until it is resolved. *)

  class switchable : C.cap -> object
    inherit C.cap
    method resolve : C.cap -> unit
  end
  (** A [new switchable init] forwards messages to [init], which can be changed by calling resolve. *)

  val embargo : C.cap -> embargo_cap
end
