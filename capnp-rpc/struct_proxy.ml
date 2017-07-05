open Asetmap

module Log = Debug.Log

module Make (C : S.CORE_TYPES) = struct
  open C

  (* Only used internally to detect cycles. *)
  let cycle_marker = C.broken_cap (Exception.v "<cycle marker>")

  let cycle_err fmt =
    "@[<v>Attempt to create a cycle detected:@," ^^ fmt ^^ "@]" |> Fmt.kstrf @@ fun msg ->
    Log.info (fun f -> f "%s" msg);
    C.broken_struct (`Exception (Exception.v msg))

  class type struct_ref_internal = object
    inherit struct_resolver

    method pipeline : Wire.Path.t -> Wire.Request.t -> cap RO_array.t -> struct_ref
    method inc_ref : Wire.Path.t -> unit
    method dec_ref : Wire.Path.t -> unit

    method field_blocker : Wire.Path.t -> base_ref option

    method field_when_resolved : Wire.Path.t -> (cap -> unit) -> unit
    (* (can't use [when_resolved] because that checks the promise isn't finished) *)

    method field_check_invariants : Wire.Path.t -> unit

    method field_sealed_dispatch : 'a. Wire.Path.t -> 'a S.brand -> 'a option

    method field_pp : Wire.Path.t -> Format.formatter -> unit
  end

  let invalid_cap = object (_ : C.cap)
    method call _ _ = failwith "invalid_cap"
    method inc_ref = failwith "invalid_cap"
    method dec_ref = failwith "invalid_cap"
    method shortest = failwith "invalid_cap"
    method when_more_resolved _ = failwith "invalid_cap"
    method pp f = Fmt.string f "(invalid cap)"
    method blocker = failwith "invalid_cap"
    method check_invariants = failwith "invalid_cap"
    method sealed_dispatch _ = failwith "invalid_cap"
    method problem = failwith "invalid_cap"
  end

  module Field_map = Map.Make(Wire.Path)

  class type field_cap = object
    inherit cap
    method resolve : cap -> unit
  end

  type field = {
    cap : field_cap;
    mutable ref_count : int;
  }

  type 'a unresolved = {
    mutable target : 'a;
    mutable fields : field Field_map.t;
    when_resolved : (struct_ref -> unit) Queue.t;
    mutable cancelling : bool;    (* User called [finish], but results may still arrive. *)

    (* This is non-None only while we are resolving. Then, it initially contains the fields
       we're resolving to. Asking for the blocker of a field returns it, but also updates the
       array so you can't ask again. *)
    mutable cycle_detector : (Wire.Response.t * cap array) option;
  }

  type 'a state =
    | Unresolved of 'a unresolved
    | Forwarding of struct_ref
    | Finished

  let pp_fields = Field_map.dump (fun f (k, v) -> Fmt.pf f "%a:rc=%d" Wire.Path.pp k v.ref_count)

  let pp_state ~pp_promise f = function
    | Unresolved {target; cancelling = true; _} -> Fmt.pf f "%a (cancelling)" pp_promise target
    | Unresolved {target; _} -> Fmt.pf f "%a" pp_promise target
    | Forwarding p -> p#pp f
    | Finished -> Fmt.pf f "(finished)"

  let dispatch state ~cancelling_ok ~unresolved ~forwarding =
    match state with
    | Finished -> failwith "Already finished"
    | Unresolved { cancelling = true; _ } when not cancelling_ok -> failwith "Cancelling"
    | Unresolved x -> unresolved x
    | Forwarding x -> forwarding x

  type field_state =
    | PromiseField of struct_ref_internal * Wire.Path.t
    | ForwardingField of cap

  let field path (p:#struct_ref_internal) =
    object (self : #field_cap)
      val mutable state = PromiseField (p, path)

      val id = Debug.OID.next ()

      method call msg caps =
        match state with
        | PromiseField (p, path) -> p#pipeline path msg caps
        | ForwardingField c -> c#call msg caps

      method pp f =
        match state with
        | PromiseField (p, path) -> Fmt.pf f "field(%a)%t" Debug.OID.pp id (p#field_pp path)
        | ForwardingField c -> Fmt.pf f "field(%a) -> %t" Debug.OID.pp id c#pp

      method inc_ref =
        match state with
        | PromiseField (p, path) -> p#inc_ref path
        | ForwardingField c -> c#inc_ref

      method dec_ref =
        Log.info (fun f -> f "dec_ref %t" self#pp);
        match state with
        | PromiseField (p, path) -> p#dec_ref path
        | ForwardingField c -> c#dec_ref

      method resolve cap =
        Log.info (fun f -> f "Resolved field(%a) to %t" Debug.OID.pp id cap#pp);
        match state with
        | ForwardingField _ -> failwith "Field already resolved!"
        | PromiseField _ -> state <- ForwardingField cap

      method shortest =
        match state with
        | ForwardingField c -> c#shortest
        | PromiseField _ -> (self :> cap)

      method blocker =
        match state with
        | ForwardingField c -> c#blocker
        | PromiseField (p, i) -> p#field_blocker i

      method problem =
        match state with
        | ForwardingField c -> c#problem
        | PromiseField _ -> None

      method when_more_resolved fn =
        match state with
        | ForwardingField c -> c#when_more_resolved fn
        | PromiseField (p, i) -> p#field_when_resolved i fn

      method check_invariants =
        match state with
        | ForwardingField c -> c#check_invariants
        | PromiseField (p, i) -> p#field_check_invariants i

      method sealed_dispatch brand =
        match state with
        | ForwardingField _ -> None
        | PromiseField (p, i) -> p#field_sealed_dispatch i brand
    end

  class virtual ['promise] t init = object (self : #struct_resolver)
    val mutable state =
      Unresolved {
        target = init;
        fields = Field_map.empty;
        cycle_detector = None;
        when_resolved = Queue.create ();
        cancelling = false
      }

    val id = Debug.OID.next ()

    method private virtual do_pipeline : 'promise -> Wire.Path.t -> Wire.Request.t -> cap RO_array.t -> struct_ref

    method private virtual on_resolve : 'promise -> struct_ref -> unit
    (* We have just started forwarding. Send any queued data onwards. *)

    method private virtual do_finish : 'promise -> unit

    method virtual field_sealed_dispatch : 'a. Wire.Path.t -> 'a S.brand -> 'a option

    method private field_resolved _f = ()
    (** [field_resolved f] is called when [f] has been resolved. *)

    method pipeline path msg caps =
      dispatch state
        ~cancelling_ok:true
        ~unresolved:(fun x -> self#do_pipeline x.target path msg caps)
        ~forwarding:(fun x -> (x#cap path)#call msg caps)

    method response =
      match state with
      | Unresolved {cancelling = true; _} | Finished -> Some (Error `Cancelled)
      | Unresolved _ -> None
      | Forwarding x -> x#response

    method blocker =
      (* It's OK to have a cancelling struct_ref here. Although the struct_ref itself
         can't be used by users, it is still available to its fields, which may call this. *)
      dispatch state
        ~cancelling_ok:true
        ~unresolved:(fun _ -> Some (self :> base_ref))
        ~forwarding:(fun x -> x#blocker)

    method cap path =
      dispatch state
        ~cancelling_ok:false
        ~unresolved:(fun u ->
            let field =
              match Field_map.find path u.fields with
              | Some f -> f
              | None ->
                let cap = field path (self :> struct_ref_internal) in
                let field = {cap; ref_count = 1} in
                u.fields <- Field_map.add path field u.fields; (* Map takes initial ref *)
                field
            in
            field.ref_count <- field.ref_count + 1;  (* Ref for user *)
            (field.cap :> cap)
          )
        ~forwarding:(fun x -> x#cap path)

    method pp f =
      let pp_promise f _ = Fmt.string f "(unresolved)" in
      Fmt.pf f "proxy[%a] -> %a"
        Debug.OID.pp id
        (pp_state ~pp_promise) state

    method connect x =
      Log.info (fun f -> f "@[Updating: %t@\n\
                            @      to: -> %t" self#pp x#pp);
      dispatch state
        ~cancelling_ok:true
        ~unresolved:(fun u ->
            (* Check for cycles *)
            let x =
              let blocked_on_us r = r#blocker = Some (self :> base_ref) in
              if blocked_on_us x then
                cycle_err "Resolving:@,  %t@,with:@,  %t" self#pp x#pp
              else match x#response with
                | Some (Error _) | None -> x
                | Some (Ok (msg, caps)) ->
                  (* Only break the fields which have cycles, not the whole promise.
                     Otherwise, it can lead to out-of-order delivery where a message
                     that should have been delivered to a working target instead gets
                     dropped. Note also that fields can depend on other fields. *)
                  let detector_caps = Array.make (RO_array.length caps) cycle_marker in
                  u.cycle_detector <- Some (msg, detector_caps);
                  let break_cycles c =
                    for i = 0 to Array.length detector_caps - 1 do
                      detector_caps.(i) <- RO_array.get caps i
                    done;
                    if c#blocker = Some (self :> C.base_ref) then
                      C.broken_cap (Exception.v (Fmt.strf "<cycle: %t>" c#pp))
                    else c
                  in
                  let fixed_caps = RO_array.map break_cycles caps in
                  if fixed_caps == caps then x
                  else C.return (msg, fixed_caps)
            in
            state <- Forwarding x;
            u.fields |> Field_map.iter (fun path f ->
                let ref_count = f.ref_count in
                assert (ref_count > 0);
                f.ref_count <- 0;
                if ref_count > 1 then (   (* Someone else is using it too *)
                  let c = x#cap path in   (* Increases ref by one *)
                  (* We dropped our ref to [f], and [x#cap] added one above. The rest we pass on. *)
                  for _ = 3 to ref_count do c#inc_ref done;
                  f.cap#resolve c
                ) else (
                  f.cap#resolve invalid_cap
                );
                self#field_resolved (f.cap :> cap)
              );
            self#on_resolve u.target x;
            Queue.iter (fun fn -> fn x) u.when_resolved;
            if u.cancelling then self#finish
          )
        ~forwarding:(fun t ->
            failwith (Fmt.strf "Already forwarding (to %t)!" t#pp)
          )

    method resolve result =
      self#connect (resolved result)

    method finish =
      dispatch state
        ~cancelling_ok:false
        ~unresolved:(fun u ->
            u.cancelling <- true;
            if Field_map.is_empty u.fields then
              self#do_finish u.target;
            (* else disable locally but don't send a cancel because we still
               want the caps. *)
          )
        ~forwarding:(fun x ->
            state <- Finished;
            x#finish
          )

    method when_resolved fn =
      dispatch state
        ~cancelling_ok:false
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
          let cap = C.Response_payload.field_or_err payload i in
          cap#inc_ref;
          fn cap
      in
      dispatch state
        ~cancelling_ok:true
        ~unresolved:(fun u -> Queue.add (fun p -> p#when_resolved fn) u.when_resolved)
        ~forwarding:(fun x -> x#when_resolved fn)

    method inc_ref path =
      dispatch state
        ~cancelling_ok:true
        ~unresolved:(fun u ->
            (* When we resolve, we'll be holding references to all the caps in the resolution, so
               so they must still be alive by the time we pass on any extra inc or dec refs. *)
            let f = Field_map.get path u.fields in
            assert (f.ref_count > 1);   (* rc can't be one because that's our reference *)
            f.ref_count <- f.ref_count + 1
          )
        ~forwarding:(fun x -> (x#cap path)#inc_ref)

    method dec_ref path =
      dispatch state
        ~cancelling_ok:true
        ~unresolved:(fun u ->
            let f = Field_map.get path u.fields in
            assert (f.ref_count > 1);   (* rc can't be one because that's our reference *)
            f.ref_count <- f.ref_count - 1
          )
        ~forwarding:(fun x -> (x#cap path)#dec_ref)

    method private update_target target =
      dispatch state
        ~cancelling_ok:false
        ~unresolved:(fun u -> u.target <- target)
        ~forwarding:(fun _ -> failwith "Already forwarding!")

    method field_check_invariants i =
      dispatch state
        ~cancelling_ok:true
        ~unresolved:(fun u ->
            let f = Field_map.get i u.fields in
            assert (f.ref_count > 1)
          )
        ~forwarding:(fun _ -> Debug.failf "Promise is resolved, but field %a isn't!" Wire.Path.pp i)

    method field_pp path f =
      match state with
      | Finished -> Fmt.pf f "Promise is finished, but field %a isn't!" Wire.Path.pp path
      | Forwarding _ -> Fmt.pf f "Promise is resolved, but field %a isn't!" Wire.Path.pp path
      | Unresolved u ->
        let field = Field_map.get path u.fields in
        (* (exclude the ref-count that we hold on it) *)
        Fmt.pf f "(rc=%d) -> #%a -> %t" (field.ref_count - 1) Wire.Path.pp path self#pp

    method check_invariants =
      dispatch state
        ~cancelling_ok:true
        ~unresolved:(fun u ->
            Field_map.iter (fun _ f -> assert (f.ref_count > 0)) u.fields
          )
        ~forwarding:(fun x -> x#check_invariants)

    initializer
      Log.info (fun f -> f "Created %t" self#pp)
  end
end
