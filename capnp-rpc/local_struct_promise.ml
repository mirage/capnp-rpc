module Make (C : S.CORE_TYPES) = struct
  module Struct_proxy = Struct_proxy.Make(C)

  type target = (C.struct_ref -> unit) Queue.t

  let rec local_promise ?parent () = object (self : #C.struct_resolver)
    inherit [target] Struct_proxy.t (Queue.create ()) as super

    method private do_pipeline q i msg caps =
      let result = local_promise ~parent:self () in
      q |> Queue.add (fun p ->
          let cap = p#cap i in
          let r = result#connect (cap#call msg caps) in
          C.dec_ref cap;
          r
        );
      (result :> C.struct_ref)

    method! pp f =
      let pp_promise f _ =
        match parent with
        | None -> Fmt.string f "(unresolved)"
        | Some p -> Fmt.pf f "blocked on %t" p#pp
      in
      Fmt.pf f "local-struct-ref(%a) -> %a"
        Debug.OID.pp id
        (Struct_proxy.pp_state ~pp_promise) state

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

  let make () = (local_promise () :> C.struct_resolver)
end
