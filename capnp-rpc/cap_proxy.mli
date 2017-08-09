(** A local promise for a capability.
    Queues messages locally. *)

module Make(C : S.CORE_TYPES) : sig
  class type resolver_cap = object
    inherit C.cap
    method resolve : C.cap -> unit
    method break : Exception.t -> unit
  end

  val local_promise : unit -> resolver_cap
  (** A [local_promise ()] is a promise that buffers calls until it is resolved. *)
end
