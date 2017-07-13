module Make (C : S.CORE_TYPES) = struct
  module Struct_proxy = Struct_proxy.Make(C)

  type target = (C.struct_ref -> unit) Queue.t

  let rec local_promise ?parent () = object (self : _ #Struct_proxy.t)
    inherit [target] Struct_proxy.t (Queue.create ()) as super

    val name = "local-promise"

    method private do_pipeline q i results msg caps =
      (* We add an extra resolver here so that we can report [self]
         as the blocker. *)
      let local_results = local_promise ~parent:self () in
      q |> Queue.add (fun p ->
          let cap = p#cap i in
          cap#call (local_results :> C.struct_resolver) msg caps;
          C.dec_ref cap
        );
      results#resolve (local_results :> C.struct_ref)

    method pp_unresolved f _ =
      match parent with
      | None -> Fmt.string f "(unresolved)"
      | Some p -> Fmt.pf f "blocked on %t" p#pp

    method private on_resolve q x =
      Queue.iter (fun fn -> fn x) q

    method private send_cancel _ = ()

    method! blocker =
      match super#blocker, parent with
      | None, _ -> None            (* Not blocked *)
      | Some _self, Some b -> b#blocker
      | Some _ as x, None -> x

    method field_sealed_dispatch _ _ = None
  end

  let make () =
    let p = local_promise () in
    (p :> C.struct_ref), p#resolver
end
