module Make (C : S.CORE_TYPES) = struct
  module Struct_proxy = Struct_proxy.Make(C)

  type target = (C.struct_ref -> unit) Queue.t

  let local_promise () = object (self : _ #Struct_proxy.t)
    inherit [target] Struct_proxy.t (Queue.create ())

    val name = "local-promise"

    method private do_pipeline q i results msg caps =
      (* We add an extra resolver here so that we can report [self]
         as the blocker. *)
      results#set_blocker (Some (self :> C.base_ref));
      q |> Queue.add (fun p ->
          results#set_blocker None;
          let cap = p#cap i in
          cap#call results msg caps;
          C.dec_ref cap
        )

    method pp_unresolved f _ =
      Fmt.string f "(unresolved)"

    method private on_resolve q x =
      Queue.iter (fun fn -> fn x) q

    method private send_cancel _ = ()

    method field_sealed_dispatch _ _ = None
  end

  let make () =
    let p = local_promise () in
    (p :> C.struct_ref), p#resolver
end
