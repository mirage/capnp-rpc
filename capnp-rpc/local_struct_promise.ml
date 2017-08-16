module Make (C : S.CORE_TYPES) = struct
  module Struct_proxy = Struct_proxy.Make(C)

  type target = {
    mutable blocking : C.struct_resolver list;  (* Resolvers blocked on us *)
    pending: (C.struct_ref -> unit) Queue.t;
  }

  let local_promise () = object (self : _ #Struct_proxy.t)
    inherit [target] Struct_proxy.t { blocking = []; pending = Queue.create () }

    val name = "local-promise"

    method private do_pipeline target i results msg =
      (* We add an extra resolver here so that we can report [self]
         as the blocker. *)
      match results#set_blocker (self :> C.base_ref) with
      | Error `Cycle ->
        C.Request_payload.release msg;
        C.resolve_exn results (Exception.v "Attempt to use pipelined call's result as pipeline target!")
      | Ok () ->
        target.blocking <- results :: target.blocking;
        target.pending |> Queue.add (fun p ->
            let cap = p#cap i in
            cap#call results msg;
            C.dec_ref cap
          )

    method pp_unresolved f _ =
      Fmt.string f "(unresolved)"

    method private on_resolve target x =
      List.iter (fun r -> r#clear_blocker) target.blocking;
      Queue.iter (fun fn -> fn x) target.pending

    method private send_cancel _ = ()

    method field_sealed_dispatch _ _ = None
  end

  let make () =
    let p = local_promise () in
    (p :> C.struct_ref), p#resolver
end
