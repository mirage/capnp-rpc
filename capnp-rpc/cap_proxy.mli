module Make(C : S.CORE_TYPES) : sig
  class type resolver_cap = object
    inherit C.cap
    method resolve : C.cap -> unit
  end

  class type embargo_cap = object
    inherit C.cap
    method disembargo : unit
    method break : Exception.t -> unit
  end

  val local_promise : unit -> resolver_cap
  (** A [local_promise ()] is a promise that buffers calls until it is resolved. *)

  val embargo : C.cap -> embargo_cap
end
