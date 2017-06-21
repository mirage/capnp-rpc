module Make (C : S.CORE_TYPES) = struct
  open C

  module Struct_proxy = Struct_proxy.Make(C)

  type target = (struct_ref -> unit) Queue.t

  let rec local_promise ?parent () = object (self : #struct_resolver)
    inherit [target] Struct_proxy.t (Queue.create ()) as super

    method private do_pipeline q i msg caps =
      let result = local_promise ~parent:self () in
      q |> Queue.add (fun p ->
          Logs.info (fun f -> f "%d:%a forwarding %t" id Wire.Path.pp i p#pp);
          result#connect ((p#cap i)#call msg caps)      (* XXX: dec_ref? *)
        );
      (result :> struct_ref)

    method! pp f =
      let pp_promise f _ =
        match parent with
        | None -> Fmt.string f "(unresolved)"
        | Some p -> Fmt.pf f "blocked on %t" p#pp
      in
      Fmt.pf f "local-struct-ref(%a) -> %a"
        (Fmt.styled `Blue Fmt.int) id
        (Struct_proxy.pp_state ~pp_promise) state

    method private on_resolve q x =
      Queue.iter (fun fn -> fn x) q

    method private do_finish _ = ()

    method! blocker =
      match super#blocker, parent with
      | None, _ -> None            (* Not blocked *)
      | Some _self, Some b -> b#blocker
      | Some _ as x, None -> x
  end

  let make () = (local_promise () :> struct_resolver)
end
