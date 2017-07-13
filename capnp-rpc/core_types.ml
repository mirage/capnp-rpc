module Log = Debug.Log

module Make(Wire : S.WIRE) = struct
  module Wire = Wire

  open Wire

  type 'a or_error = ('a, Error.t) result

  class type base_ref = object
    method pp : Format.formatter -> unit
    method blocker : base_ref option
    method check_invariants : unit
    method update_rc : int -> unit
    method sealed_dispatch : 'a. 'a S.brand -> 'a option
  end

  let pp f x = x#pp f

  class type struct_ref = object
    inherit base_ref
    method when_resolved : ((Response.t * cap RO_array.t) or_error -> unit) -> unit
    method response : (Response.t * cap RO_array.t) or_error option
    method cap : Path.t -> cap
  end
  and cap = object
    inherit base_ref
    method call : struct_resolver -> Request.t -> cap RO_array.t -> unit   (* Takes ownership of [caps] *)
    method shortest : cap
    method when_more_resolved : (cap -> unit) -> unit
    method problem : Exception.t option
  end
  and struct_resolver = object
    method pp : Format.formatter -> unit
    method resolve : struct_ref -> unit
  end

  let pp_cap_list f caps = RO_array.pp pp f caps

  type 'a S.brand += Gc : unit S.brand

  let inc_ref x = x#update_rc 1
  let dec_ref x = x#update_rc (-1)

  class virtual ref_counted =
    object (self : #base_ref)
      val mutable ref_count = RC.one
      method private virtual release : unit
      method virtual pp : Format.formatter -> unit

      method private pp_refcount f = RC.pp f ref_count

      method private check_refcount =
        RC.check ref_count ~pp:self#pp

      method update_rc d =
        ref_count <- RC.sum ref_count d ~pp:self#pp;
        if RC.is_zero ref_count then (
          self#release;          (* We can get GC'd once we enter [release], but ref_count is 0 by then so OK. *)
        );
        ignore (Sys.opaque_identity self)

      method check_invariants = self#check_refcount

      method sealed_dispatch : type a. a S.brand -> a option = function
        | Gc ->
          if not (RC.is_zero ref_count) then (
            ref_leak_detected (fun () ->
                if RC.is_zero ref_count then (
                  Log.warn (fun f -> f "@[<v2>Reference GC'd with non-zero ref-count!@,%t@,\
                                        But, ref-count is now zero, so a previous GC leak must have fixed it.@]"
                               self#pp);
                ) else (
                  Log.warn (fun f -> f "@[<v2>Reference GC'd with %a!@,%t@]"
                               RC.pp ref_count self#pp);
                  ref_count <- RC.leaked;
                  self#release
                )
              )
          );
          Some ()
        | _ ->
          None

      method virtual blocker : base_ref option

      initializer
        Gc.finalise (fun (self:#base_ref) -> ignore (self#sealed_dispatch Gc)) self
    end

  let rec broken_cap ex = object (self : cap)
    method call results _ caps =
      RO_array.iter dec_ref caps;
      results#resolve (broken_struct (`Exception ex))
    method update_rc _ = ()
    method pp f = Exception.pp f ex
    method shortest = self
    method blocker = None
    method when_more_resolved _ = ()
    method check_invariants = ()
    method sealed_dispatch _ = None
    method problem = Some ex
  end
  and broken_struct err = object (_ : struct_ref)
    method response = Some (Error err)
    method when_resolved fn = fn (Error err)
    method cap _ =
      match err with
      | `Exception ex -> broken_cap ex
      | `Cancelled -> broken_cap Exception.cancelled
    method pp f = Error.pp f err
    method update_rc _ = ()
    method blocker = None
    method check_invariants = ()
    method sealed_dispatch _ = None
  end

  let null = broken_cap {Exception.ty = `Failed; reason = "null"}
  let cancelled = broken_cap Exception.cancelled

  let cap_failf ?(ty=`Failed) msg = msg |> Fmt.kstrf (fun reason -> broken_cap {Exception.ty; reason})

  let cap_in_cap_list i caps =
    match i with
    | None -> Ok null (* The field wasn't set - OK *)
    | Some i when i < 0 || i >= RO_array.length caps -> Error (`Invalid_index i)
    | Some i ->
      let cap = RO_array.get caps i in
      if cap == null then Error (`Invalid_index i)  (* Index was marked as unused *)
      else Ok cap

  let cap_in_cap_list_or_err i caps =
    match cap_in_cap_list i caps with
    | Ok cap -> cap
    | Error (`Invalid_index i) ->
      cap_failf "Invalid cap index %d in %a" i pp_cap_list caps

  let cap_in_payload i (_, caps) = cap_in_cap_list_or_err i caps

  let cap_of_err = function
    | `Exception msg -> broken_cap msg
    | `Cancelled -> cancelled

  let cap_in_result i = function
    | Ok p -> cap_in_payload i p
    | Error e -> cap_of_err e

  module Request_payload = struct
    type t = Request.t * cap RO_array.t
    let pp f (msg, caps) = Fmt.pf f "@[%a%a@]" Request.pp msg pp_cap_list caps

    let field (msg, caps) path =
      let i = Request.cap_index msg path in
      cap_in_cap_list i caps
  end

  module Response_payload = struct
    type t = Response.t * cap RO_array.t
    let pp f (msg, caps) = Fmt.pf f "@[%a%a@]" Response.pp msg pp_cap_list caps

    let field (msg, caps) path =
      let i = Response.cap_index msg path in
      cap_in_cap_list i caps

    let field_or_err (msg, caps) path =
      let i = Response.cap_index msg path in
      cap_in_cap_list_or_err i caps
  end

  let return (msg, caps) = object (self : struct_ref)
    inherit ref_counted as super

    val mutable caps = caps

    val id = Debug.OID.next ()

    method response = Some (Ok (msg, caps))

    method when_resolved fn = fn (Ok (msg, caps))

    method cap path =
      let i = Response.cap_index msg path in
      let cap = cap_in_cap_list_or_err i caps in
      inc_ref cap;
      cap

    method pp f = Fmt.pf f "returned(%a, %t):%a"
        Debug.OID.pp id
        self#pp_refcount
        Response_payload.pp (msg, caps)

    method private release =
      RO_array.iter dec_ref caps;
      caps <- RO_array.empty;
      ignore (Sys.opaque_identity self) (* Prevent self from being GC'd until this point *)

    method blocker = None

    method! check_invariants =
      super#check_invariants;
      RO_array.iter (fun c -> c#check_invariants) caps
  end

  class virtual service = object (self : #cap)
    inherit ref_counted

    method virtual call : struct_resolver -> Request.t -> cap RO_array.t -> unit
    method private release = ()
    method pp f = Fmt.string f "<service>"
    method shortest = (self :> cap)
    method blocker = None
    method when_more_resolved _ = ()
    method problem = None
  end

  let fail ?(ty=`Failed) msg =
    msg |> Fmt.kstrf @@ fun reason ->
    broken_struct (`Exception {Exception.ty; reason})

  let resolved = function
    | Ok x -> return x
    | Error msg -> broken_struct msg

  let resolve_payload (r:#struct_resolver) (x:Response_payload.t or_error) = r#resolve (resolved x)
  let resolve_ok r msg caps = resolve_payload r (Ok (msg, caps))
  let resolve_exn r ex = resolve_payload r (Error (`Exception ex))
end
