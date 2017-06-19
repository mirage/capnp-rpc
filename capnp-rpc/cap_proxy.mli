module Make(C : S.CORE_TYPES) : sig
  class type embargo_cap = object
    inherit C.cap
    method disembargo : unit
  end

  class local_promise : object
    inherit C.cap
    method resolve : C.cap -> unit
  end

  val embargo : C.cap -> embargo_cap
end
