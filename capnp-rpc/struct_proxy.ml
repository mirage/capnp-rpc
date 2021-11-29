open Asetmap

module Log = Debug.Log

module Make (C : S.CORE_TYPES) = struct
  open C

  (* Only used internally to detect cycles. *)
  let cycle_marker = C.broken_cap (Exception.v "<cycle marker>")

  let cycle_err fmt =
    "@[<v>Attempt to create a cycle detected:@," ^^ fmt ^^ "@]" |> Fmt.kstr @@ fun msg ->
    Log.info (fun f -> f "%s" msg);
    C.broken_struct (`Exception (Exception.v msg))

  class type struct_ref_internal = object
    inherit struct_resolver

    method pipeline : Wire.Path.t -> C.struct_resolver -> Wire.Request.t -> unit
    method field_update_rc : Wire.Path.t -> int -> unit

    method field_blocker : Wire.Path.t -> base_ref option

    method field_when_resolved : Wire.Path.t -> (cap -> unit) -> unit
    (* (can't use [when_resolved] because that checks the promise isn't finished) *)

    method field_check_invariants : Wire.Path.t -> unit

    method field_sealed_dispatch : 'a. Wire.Path.t -> 'a S.brand -> 'a option

    method field_pp : Wire.Path.t -> Format.formatter -> unit
  end

  let invalid_cap = object (_ : C.cap)
    method call _ _ = failwith "invalid_cap"
    method update_rc = failwith "invalid_cap"
    method shortest = failwith "invalid_cap"
    method when_more_resolved _ = failwith "invalid_cap"
    method pp f = Fmt.string f "(invalid cap)"
    method blocker = failwith "invalid_cap"
    method check_invariants = failwith "invalid_cap"
    method sealed_dispatch _ = failwith "invalid_cap"
    method problem = failwith "invalid_cap"
    method when_released = failwith "invalid_cap"
  end

  module Field_map = Map.Make(Wire.Path)

  class type field_cap = object
    inherit cap
    method resolve : cap -> unit
  end

  type field = {
    cap : field_cap;
    mutable ref_count : RC.t;
  }

  type 'a unresolved = {
    mutable target : 'a;
    mutable rc : RC.t;
    mutable fields : field Field_map.t;
    when_resolved : (struct_ref -> unit) Queue.t;

    (* This is non-None only while we are resolving. Then, it initially contains the fields
       we're resolving to. Asking for the blocker of a field returns it, but also updates the
       array so you can't ask again. *)
    mutable cycle_detector : (Wire.Response.t * cap array) option;
  }

  type 'a state =
    | Unresolved of 'a unresolved
    | Forwarding of struct_ref
    | Finished

  let pp_fields = Field_map.dump (fun f (k, v) -> Fmt.pf f "%a:%a" Wire.Path.pp k RC.pp v.ref_count)

  let pp_opt_blocked_on f = function
    | None -> ()
    | Some b -> Fmt.pf f " (blocked on %t)" b#pp

  let pp_state ~pp_promise f = function
    | Unresolved {target; _} -> Fmt.pf f "%a" pp_promise target
    | Forwarding p -> p#pp f
    | Finished -> Fmt.pf f "(finished)"

  let dispatch state ~unresolved ~forwarding =
    match state with
    | Finished -> failwith "Already finished"
    | Unresolved x -> unresolved x
    | Forwarding x -> forwarding x

  type promise_field = {
    sr : struct_ref_internal;
    path : Wire.Path.t;
    on_release : (unit -> unit) Queue.t;
    (* Note: currently, a field can never be released while unresolved.
       Possibly fields should have their own ref-counts.
       However, this doesn't matter for the only user of [on_release], which
       is the restorer system (that just needs to know if something becomes
       invalid, so it doesn't keep it cached). *)
  }

  type field_state =
    | PromiseField of promise_field
    | ForwardingField of cap

  let field path (p:#struct_ref_internal) =
    object (self : #field_cap)
      val mutable state = PromiseField {sr = p; path; on_release = Queue.create ()}

      val id = Debug.OID.next ()

      method call results msg =
        match state with
        | PromiseField p -> p.sr#pipeline p.path results msg
        | ForwardingField c -> c#call results msg

      method pp f =
        match state with
        | PromiseField p -> Fmt.pf f "field(%a)%t" Debug.OID.pp id (p.sr#field_pp p.path)
        | ForwardingField c -> Fmt.pf f "field(%a) -> %t" Debug.OID.pp id c#pp

      method update_rc d =
        match state with
        | ForwardingField c -> c#update_rc d
        | PromiseField p -> p.sr#field_update_rc p.path d

      method when_released fn =
        match state with
        | PromiseField p -> Queue.add fn p.on_release
        | ForwardingField x -> x#when_released fn

      method resolve cap =
        Log.debug (fun f -> f "Resolved field(%a) to %t" Debug.OID.pp id cap#pp);
        match state with
        | ForwardingField _ -> failwith "Field already resolved!"
        | PromiseField p ->
          state <- ForwardingField cap;
          Queue.iter (fun fn -> cap#when_released fn) p.on_release

      method shortest =
        match state with
        | ForwardingField c -> c#shortest
        | PromiseField _ -> (self :> cap)

      method blocker =
        match state with
        | ForwardingField c -> c#blocker
        | PromiseField p -> p.sr#field_blocker p.path

      method problem =
        match state with
        | ForwardingField c -> c#problem
        | PromiseField _ -> None

      method when_more_resolved fn =
        match state with
        | ForwardingField c -> c#when_more_resolved fn
        | PromiseField p -> p.sr#field_when_resolved p.path fn

      method check_invariants =
        match state with
        | ForwardingField c -> c#check_invariants
        | PromiseField p -> p.sr#field_check_invariants p.path

      method sealed_dispatch brand =
        match state with
        | ForwardingField _ -> None
        | PromiseField p -> p.sr#field_sealed_dispatch p.path brand
    end

  class virtual ['promise] t init = object (self : 'self)
    constraint 'self = #C.struct_ref
    constraint 'self = #C.struct_resolver

    val mutable state =
      Unresolved {
        target = init;
        fields = Field_map.empty;
        cycle_detector = None;
        when_resolved = Queue.create ();
        rc = RC.one;
      }

    val virtual name : string
    (* e.g. "local-promise" *)

    val mutable blocker = None

    val id = Debug.OID.next ()

    method private virtual pp_unresolved : 'promise Fmt.t

    method private virtual do_pipeline : 'promise ->
      Wire.Path.t -> C.struct_resolver -> Wire.Request.t -> unit

    method private virtual on_resolve : 'promise -> struct_ref -> unit
    (* We have just started forwarding. Send any queued data onwards. *)

    method private virtual send_cancel : 'promise -> unit
    (* There is no longer a need for this (unresolved) proxy's result. *)

    method virtual field_sealed_dispatch : 'a. Wire.Path.t -> 'a S.brand -> 'a option

    method private field_resolved _f = ()
    (** [field_resolved f] is called when [f] has been resolved. *)

    method pipeline path results msg =
      dispatch state
        ~unresolved:(fun x -> self#do_pipeline x.target path results msg)
        ~forwarding:(fun x -> (x#cap path)#call results msg)

    method response =
      dispatch state
        ~unresolved:(fun _ -> None)
        ~forwarding:(fun x -> x#response)

    method blocker =
      dispatch state
        ~unresolved:(fun _ ->
            match blocker with
            | None -> Some (self :> base_ref)
            | Some x -> x#blocker
          )
        ~forwarding:(fun x -> x#blocker)

    method set_blocker (b : C.base_ref) =
      if b#blocker = Some (self :> C.base_ref) then Error `Cycle
      else (
        blocker <- Some b;
        Ok ()
      )

    method clear_blocker =
      blocker <- None

    method cap path =
      dispatch state
        ~unresolved:(fun u ->
            let field =
              match Field_map.find path u.fields with
              | Some f -> f
              | None ->
                let cap = field path (self :> struct_ref_internal) in
                let field = {cap; ref_count = RC.one} in
                u.fields <- Field_map.add path field u.fields; (* Map takes initial ref *)
                C.inc_ref self;    (* Field takes ref on us too *)
                field
            in
            field.ref_count <- RC.succ field.ref_count ~pp:self#pp;  (* Ref for user *)
            (field.cap :> cap)
          )
        ~forwarding:(fun x -> x#cap path)

    method pp f =
      match state with
      | Unresolved u ->
        Fmt.pf f "%s(%a, %a) -> %a%a"
          name
          Debug.OID.pp id RC.pp u.rc
          self#pp_unresolved u.target
          pp_opt_blocked_on blocker
      | Forwarding x ->
        Fmt.pf f "%s(%a) -> %t"
          name
          Debug.OID.pp id
          x#pp
      | Finished ->
        Fmt.pf f "%s(%a) (finished)"
          name
          Debug.OID.pp id

    method resolve x =
      Log.debug (fun f -> f "@[Updating: %t@\n\
                             @      to: -> %t" self#pp x#pp);
      match state with
      | Finished -> dec_ref x (* Cancelled *)
      | Forwarding x -> failwith (Fmt.str "Already forwarding (to %t)!" x#pp)
      | Unresolved u ->
        (* Check for cycles *)
        let x =
          let blocked_on_us r = r#blocker = Some (self :> base_ref) in
          if blocked_on_us x then
            cycle_err "Resolving:@,  %t@,with:@,  %t" self#pp x#pp
          else match x#response with
            | Some (Error _) | None -> x
            | Some (Ok payload) ->
              (* Only break the fields which have cycles, not the whole promise.
                 Otherwise, it can lead to out-of-order delivery where a message
                 that should have been delivered to a working target instead gets
                 dropped. Note also that fields can depend on other fields. *)
              let caps = C.Response_payload.snapshot_caps payload in
              let detector_caps = Array.make (RO_array.length caps) cycle_marker in
              u.cycle_detector <- Some (payload, detector_caps);
              let break_cycles c =
                for i = 0 to Array.length detector_caps - 1 do
                  detector_caps.(i) <- RO_array.get_exn caps i
                done;
                if c#blocker = Some (self :> C.base_ref) then
                  C.broken_cap (Exception.v (Fmt.str "<cycle: %t>" c#pp))
                else c
              in
              let fixed_caps = RO_array.map break_cycles caps in
              if RO_array.equal (=) fixed_caps caps then x
              else (
                RO_array.iter C.inc_ref fixed_caps;
                C.dec_ref x;
                C.return @@ C.Response_payload.with_caps fixed_caps payload
              )
        in
        state <- Forwarding x;
        begin match RC.to_int u.rc with
          | None -> assert false            (* Can't happen; we don't detect leaks *)
          | Some rc -> x#update_rc rc;      (* Transfer our ref-count to [x]. *)
        end;
        u.fields |> Field_map.iter (fun path f ->
            let pp = self#field_pp path in
            let ref_count = RC.pred f.ref_count ~pp in (* Count excluding our ref *)
            f.ref_count <- RC.zero;
            begin match RC.to_int ref_count with
              | None        (* Field was leaked; shouldn't happen since we hold a copy anyway. *)
              | Some 0 -> f.cap#resolve invalid_cap (* No other users *)
              | Some ref_count ->                   (* Someone else is using it too *)
                let c = x#cap path in   (* Increases ref by one *)
                (* Transfer our refs to the new target, excluding the one already addded by [x#cap]. *)
                c#update_rc (ref_count - 1);
                f.cap#resolve c
            end;
            self#field_resolved (f.cap :> cap)
          );
        self#on_resolve u.target x;
        Queue.iter (fun fn -> fn x) u.when_resolved;
        let refs_dropped = Field_map.cardinal u.fields in
        x#update_rc (-(refs_dropped + 1)) (* Also, we take ownership of [x]. *)

    method resolver = (self :> C.struct_resolver)

    method update_rc d =
      dispatch state
        ~unresolved:(fun u ->
            let { target; rc; fields; when_resolved; cycle_detector = _ } = u in
            u.rc <- RC.sum rc d ~pp:self#pp;
            if RC.is_zero u.rc then (
              assert (Field_map.is_empty fields);
              state <- Finished;
              let err = C.broken_struct `Cancelled in
              Queue.iter (fun fn -> fn err) when_resolved;
              self#send_cancel target;
            )
          )
        ~forwarding:(fun x -> x#update_rc d)

    method when_resolved fn =
      dispatch state
        ~unresolved:(fun u -> Queue.add (fun p -> p#when_resolved fn) u.when_resolved)
        ~forwarding:(fun x -> x#when_resolved fn)

    method field_blocker path =
      match state with
      | Unresolved { cycle_detector = Some (msg, caps); _ } ->
        begin match Wire.Response.cap_index msg path with
        | Some i when i >= 0 && i < Array.length caps ->
          (* We're in the process of resolving.
             Pretend that we've already resolved for the purpose of finding the blocker,
             because one field might depend on another, and that's OK. But also disable
             this path from being followed again, in case there's a cycle. *)
          let cap = caps.(i) in
          if cap = cycle_marker then Some (self :> C.base_ref)
          else (
            caps.(i) <- cycle_marker;
            cap#blocker
          )
        | _ -> None
        end
      | _ -> self#blocker

    method field_when_resolved i fn =
      let fn : Response_payload.t or_error -> unit = function
        | Error (`Exception e) -> fn (C.broken_cap e)
        | Error `Cancelled -> fn (C.broken_cap Exception.cancelled)
        | Ok payload ->
          match C.Response_payload.field payload i with
          | None -> fn C.null
          | Some cap -> fn cap
      in
      dispatch state
        ~unresolved:(fun u -> Queue.add (fun p -> p#when_resolved fn) u.when_resolved)
        ~forwarding:(fun x -> x#when_resolved fn)

    method field_update_rc path d =
      dispatch state
        ~unresolved:(fun u ->
            (* When we resolve, we'll be holding references to all the caps in the resolution, so
               so they must still be alive by the time we pass on any extra inc or dec refs. *)
            let f = Field_map.get path u.fields in
            assert (f.ref_count > RC.one);   (* rc can't be one because that's our reference *)
            let pp = self#field_pp path in
            f.ref_count <- RC.sum f.ref_count d ~pp
          )
        ~forwarding:(fun x ->
            let c = x#cap path in       (* Increases rc by one *)
            c#update_rc (d - 1)
          )

    method field_dec_ref path =
      dispatch state
        ~unresolved:(fun u ->
            let f = Field_map.get path u.fields in
            assert (f.ref_count > RC.one);   (* rc can't be one because that's our reference *)
            let pp = self#field_pp path in
            f.ref_count <- RC.pred f.ref_count ~pp
          )
        ~forwarding:(fun x ->
            let c = x#cap path in       (* Increases ref by one *)
            c#update_rc (-2)
          )

    method private update_target target =
      dispatch state
        ~unresolved:(fun u -> u.target <- target)
        ~forwarding:(fun _ -> failwith "Already forwarding!")

    method field_check_invariants i =
      dispatch state
        ~unresolved:(fun u ->
            let f = Field_map.get i u.fields in
            assert (f.ref_count > RC.one);
            self#check_invariants
          )
        ~forwarding:(fun _ -> Debug.failf "Promise is resolved, but field %a isn't!" Wire.Path.pp i)

    method field_pp path f =
      match state with
      | Finished -> Fmt.pf f "Promise is finished, but field %a isn't!" Wire.Path.pp path
      | Forwarding _ -> Fmt.pf f "Promise is resolved, but field %a isn't!" Wire.Path.pp path
      | Unresolved u ->
        let field = Field_map.get path u.fields in
        match RC.to_int field.ref_count with
        | None ->
          Fmt.pf f "(rc=LEAKED) -> #%a -> %t" Wire.Path.pp path self#pp
        | Some rc ->
          (* (separate the ref-count that we hold on it) *)
          Fmt.pf f "(rc=1+%d) -> #%a -> %t" (rc - 1) Wire.Path.pp path self#pp

    method check_invariants =
      dispatch state
        ~unresolved:(fun u ->
            RC.check ~pp:self#pp u.rc;
            Field_map.iter (fun i f -> RC.check f.ref_count ~pp:(self#field_pp i)) u.fields;
            match blocker with
            | Some x when x#blocker = None ->
              Debug.invariant_broken (fun f ->
                  Fmt.pf f "%t is blocked on %t, but that isn't blocked!" self#pp x#pp
                )
            | _ -> ()
          )
        ~forwarding:(fun x -> x#check_invariants)

    method sealed_dispatch _ = None

    initializer
      Log.debug (fun f -> f "Created %t" self#pp)
  end
end
