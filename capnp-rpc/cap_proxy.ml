module Log = Debug.Log

module Make(C : S.CORE_TYPES) = struct
  open C
  module Local_struct_promise = Local_struct_promise.Make(C)

  class type resolver_cap = object
    inherit C.cap
    method resolve : C.cap -> unit
  end

  class type embargo_cap = object
    inherit cap
    method disembargo : unit
    method break : Exception.t -> unit
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
        self#check_refcount;
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

      method break ex =
        self#resolve (broken_cap ex)

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
        | Unresolved _ -> Fmt.pf f "local-cap-promise(%a, %t) -> (unresolved)" Debug.OID.pp id self#pp_refcount
        | Resolved cap -> Fmt.pf f "local-cap-promise(%a, %t) -> %t" Debug.OID.pp id self#pp_refcount cap#pp

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
          super#check_refcount;
          super#resolve underlying

        method! pp f =
          match state with
          | Unresolved _ -> Fmt.pf f "embargoed(%a, %t) -> %t" Debug.OID.pp id super#pp_refcount underlying#pp
          | Resolved cap -> Fmt.pf f "disembargoed(%a, %t) -> %t" Debug.OID.pp id super#pp_refcount cap#pp
      end
    in
    (cap :> embargo_cap)

  let local_promise () = (new local_promise :> resolver_cap)
end
