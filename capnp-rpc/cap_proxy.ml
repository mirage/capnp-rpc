module Log = Debug.Log

module Make(C : S.CORE_TYPES) = struct
  open C
  module Local_struct_promise = Local_struct_promise.Make(C)

  class type embargo_cap = object
    inherit cap
    method disembargo : unit
  end

  (* Operations to perform when resolved. *)
  type pending =
    | Call of struct_resolver * Wire.Request.t * cap RO_array.t
    | Watcher of (cap -> unit)

  type cap_promise_state =
    | Unresolved of pending Queue.t * bool      (* bool = release-pending *)
    | Resolved of cap

  let released = C.broken_cap (Exception.v "(released)")

  class local_promise =
    object (self : #cap)
      inherit ref_counted as super

      val mutable state = Unresolved (Queue.create (), false)

      val id = Debug.OID.next ()

      method call msg caps =
        match state with
        | Unresolved (q, release_pending) ->
          assert (not release_pending);
          let result = Local_struct_promise.make () in
          Queue.add (Call (result, msg, caps)) q;
          (result :> struct_ref)
        | Resolved cap -> cap#call msg caps

      method resolve (cap:cap) =
        match state with
        | Unresolved (q, release_pending) ->
          let cap =
            match cap#blocker with
            | Some blocker when blocker = (self :> base_ref) ->
              let msg = Fmt.strf "@[<v>Attempt to create a cycle detected:@,\
                                  Resolving %t with %t would create a cycle@]" self#pp cap#pp in
              Log.info (fun f -> f "%s" msg);
              cap#dec_ref;
              C.broken_cap (Exception.v msg)
            | _ -> cap
          in
          state <- Resolved cap;
          Log.info (fun f -> f "Resolved local cap promise: %t" self#pp);
          let forward = function
            | Watcher fn -> cap#inc_ref; fn cap
            | Call (result, msg, caps) ->
              let r = cap#call msg caps in
              result#connect r      (* Or should it be connect [r] to [result], for tail-recursion? *)
          in
          Queue.iter forward q;
          if release_pending then (
            Log.info (fun f -> f "Completing delayed release of %t" self#pp);
            cap#dec_ref;
            state <- Resolved released
          )
        | Resolved _ -> failwith "Already resolved!"

      method private release =
        match state with
        | Unresolved (target, release_pending) ->
          Log.info (fun f -> f "Delaying release of %t until resolved" self#pp);
          assert (not release_pending);
          state <- Unresolved (target, true)
        | Resolved cap ->
          cap#dec_ref;
          state <- Resolved released

      method shortest =
        match state with
        | Unresolved _ -> (self :> cap)
        | Resolved cap -> cap#shortest

      method blocker =
        match state with
        | Unresolved _ -> Some (self :> base_ref)
        | Resolved cap -> cap#blocker

      method when_more_resolved fn =
        match state with
        | Unresolved (q, _) -> Queue.add (Watcher fn) q
        | Resolved x -> x#when_more_resolved fn

      method pp f =
        match state with
        | Unresolved _ -> Fmt.pf f "local-cap-promise(%a, rc=%d) -> (unresolved)" Debug.OID.pp id ref_count
        | Resolved cap -> Fmt.pf f "local-cap-promise(%a, rc=%d) -> %t" Debug.OID.pp id ref_count cap#pp

      method! check_invariants =
        super#check_invariants;
        match state with
        | Unresolved _ -> ()
        | Resolved cap -> cap#check_invariants
    end

  let embargo underlying : embargo_cap =
    let cap =
      object
        inherit local_promise as super

        method! release =
          underlying#dec_ref;
          state <- Resolved released

        method disembargo =
          super#resolve underlying

        method! pp f =
          match state with
          | Unresolved _ -> Fmt.pf f "embargoed(%a, rc=%d) -> %t" Debug.OID.pp id ref_count underlying#pp
          | Resolved cap -> Fmt.pf f "disembargoed(%a, rc=%d) -> %t" Debug.OID.pp id ref_count cap#pp
      end
    in
    (cap :> embargo_cap)

  class switchable init =
    let is_settled x = (x#blocker = None) in
    let pp_state f = function
      | `Unsettled (x, _) -> Fmt.pf f "(unsettled) -> %t" x#pp
      | `Settled x -> Fmt.pf f "(settled) -> %t" x#pp
    in
    object (self : #cap)
      inherit ref_counted as super

      val mutable state =
        if is_settled init then `Settled init
        else `Unsettled (init, Queue.create ())

      method call msg caps =
        match state with
        | `Unsettled (x, _)
        | `Settled x -> x#call msg caps

      method resolve cap =
        match state with
        | `Settled _ -> failwith "Can't resolve settled switchable!"
        | `Unsettled (old, q) ->
          if is_settled cap then (
            state <- `Settled cap;
            Queue.iter (fun f -> f (cap#inc_ref; cap)) q
          ) else (
            state <- `Unsettled (cap, q)
          );
          old#dec_ref

      method private release =
        begin
          match state with
          | `Unsettled (x, _)
          | `Settled x -> x#dec_ref
        end;
        state <- `Settled released

      method shortest =
        match state with
        | `Unsettled _ -> (self :> cap)     (* Can't shorten, as we may change later *)
        | `Settled x -> x#shortest

      method blocker =
        match state with
        | `Unsettled _ -> Some (self :> base_ref)
        | `Settled x -> x#blocker

      method when_more_resolved fn =
        match state with
        | `Unsettled (_, q) -> Queue.add fn q
        | `Settled x -> x#when_more_resolved fn

      method! check_invariants =
        super#check_invariants;
        match state with
        | `Unsettled (x, _) | `Settled x -> x#check_invariants

      method pp f =
        Fmt.pf f "switchable %a" pp_state state
    end
end
