(** A local promise for a call response.
    Queues messages locally. *)

module Make (C : S.CORE_TYPES) : sig
  val make : unit -> C.struct_ref * C.struct_resolver
end
