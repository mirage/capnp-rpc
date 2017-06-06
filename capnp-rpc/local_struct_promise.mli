module Make (C : S.CORE_TYPES) : sig
  val make : unit -> C.struct_resolver
end
