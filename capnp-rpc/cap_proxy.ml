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
    | Call of C.struct_resolver * Wire.Request.t * cap RO_array.t
    | Watcher of (cap -> unit)

  type unresolved = {
    queue : pending Queue.t;
    mutable rc : RC.t;
    mutable release_pending : bool;
  }

  type cap_promise_state =
    | Unresolved of unresolved
    | Resolved of cap

  let released = C.broken_cap (Exception.v "(released)")

  class local_promise =
    object (self : #cap)
      val mutable state = Unresolved {rc = RC.one; queue = Queue.create (); release_pending = false}

      val id = Debug.OID.next ()

      method private release_while_unresolved = ()

      method call results msg caps =
        match state with
        | Unresolved {queue; release_pending; rc = _} ->
          assert (not release_pending);
          Queue.add (Call (results, msg, caps)) queue
        | Resolved cap -> cap#call results msg caps

      method update_rc d =
        match state with
        | Unresolved u ->
          u.rc <- RC.sum u.rc d ~pp:(fun f -> self#pp f);
          if RC.is_zero u.rc then self#release_while_unresolved
        | Resolved x -> x#update_rc d

      method resolve (cap:cap) =
        match state with
        | Unresolved {queue; release_pending; rc} ->
          let pp f = self#pp f in
          RC.check ~pp rc;
          let cap =
            match cap#blocker with
            | Some blocker when blocker = (self :> base_ref) ->
              let msg = Fmt.strf "@[<v>Attempt to create a cycle detected:@,\
                                  Resolving %t with %t would create a cycle@]" self#pp cap#pp in
              Log.info (fun f -> f "%s" msg);
              C.dec_ref cap;
              C.broken_cap (Exception.v msg)
            | _ -> cap
          in
          begin match RC.to_int rc with
            | Some rc -> cap#update_rc (rc - 1);     (* Transfer our ref-count *)
            | None -> ()
          end;
          state <- Resolved cap;
          Log.info (fun f -> f "Resolved local cap promise: %t" self#pp);
          let forward = function
            | Watcher fn -> C.inc_ref cap; fn cap
            | Call (result, msg, caps) -> cap#call result msg caps
          in
          Queue.iter forward queue;
          if release_pending then (
            Log.info (fun f -> f "Completing delayed release of %t" self#pp);
            C.dec_ref cap;
            state <- Resolved released
          )
        | Resolved _ -> failwith "Already resolved!"

      method break ex =
        self#resolve (broken_cap ex)

      method private release =
        match state with
        | Unresolved u ->
          Log.info (fun f -> f "Delaying release of %t until resolved" self#pp);
          assert (not u.release_pending);
          u.release_pending <- true
        | Resolved cap ->
          C.dec_ref cap;
          state <- Resolved released

      method shortest =
        match state with
        | Unresolved _ -> (self :> cap)
        | Resolved cap -> cap#shortest

      method problem =
        match state with
        | Unresolved _ -> None
        | Resolved cap -> cap#problem

      method blocker =
        match state with
        | Unresolved _ -> Some (self :> base_ref)
        | Resolved cap -> cap#blocker

      method when_more_resolved fn =
        match state with
        | Unresolved {queue; _} -> Queue.add (Watcher fn) queue
        | Resolved x -> x#when_more_resolved fn

      method pp f =
        match state with
        | Unresolved u -> Fmt.pf f "local-cap-promise(%a, %a) -> (unresolved)"
                            Debug.OID.pp id
                            RC.pp u.rc
        | Resolved cap -> Fmt.pf f "local-cap-promise(%a) -> %t" Debug.OID.pp id cap#pp

      method check_invariants =
        let pp f = self#pp f in
        match state with
        | Unresolved u -> RC.check ~pp u.rc
        | Resolved cap -> cap#check_invariants

      method sealed_dispatch _ = None
    end

  let embargo underlying : embargo_cap =
    let cap =
      object
        inherit local_promise as super

        method! private release_while_unresolved =
          C.dec_ref underlying

        method disembargo =
          super#resolve underlying

        method! pp f =
          match state with
          | Unresolved u -> Fmt.pf f "embargoed(%a, %a)" Debug.OID.pp id RC.pp u.rc
          | Resolved cap -> Fmt.pf f "disembargoed(%a) -> %t" Debug.OID.pp id cap#pp
      end
    in
    (cap :> embargo_cap)

  let local_promise () = (new local_promise :> resolver_cap)
end
