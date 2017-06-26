module Log = Debug.Log

module Make(Wire : S.WIRE) = struct
  module Wire = Wire

  open Wire

  type 'a or_error = ('a, Error.t) result

  class type base_ref = object
    method pp : Format.formatter -> unit
    method blocker : base_ref option
    method check_invariants : unit
  end

  let pp f x = x#pp f

  class type struct_ref = object
    inherit base_ref
    method when_resolved : ((Response.t * cap RO_array.t) or_error -> unit) -> unit
    method response : (Response.t * cap RO_array.t) or_error option
    method finish : unit
    method cap : Path.t -> cap
  end
  and cap = object
    inherit base_ref
    method call : Request.t -> cap RO_array.t -> struct_ref   (* Takes ownership of [caps] *)
    method inc_ref : unit
    method dec_ref : unit
    method shortest : cap
    method when_more_resolved : (cap -> unit) -> unit
  end

  class type struct_resolver = object
    inherit struct_ref
    method resolve : (Response.t * cap RO_array.t) or_error -> unit
    method connect : struct_ref -> unit
  end

  let pp_cap_list f caps = RO_array.pp pp f caps

  class virtual ref_counted = object (self)
    val mutable ref_count = 1

    method private virtual release : unit
    method virtual pp : Format.formatter -> unit

    method inc_ref =
      if ref_count < 1 then (
        failwith (Fmt.strf "inc_ref: already destroyed %t" self#pp)
      );
      ref_count <- ref_count + 1

    method dec_ref =
      if ref_count < 1 then failwith (Fmt.strf "Already unref'd! %t" self#pp);
      ref_count <- ref_count - 1;
      if ref_count = 0 then
        self#release

    method check_invariants =
      if ref_count <= 0 then (
        Debug.invariant_broken @@ fun f ->
        Fmt.pf f "%a for %t"
          (Fmt.styled `Red (Fmt.fmt "ref_count=%d")) ref_count
          self#pp
      )
  end

  let rec broken_cap ex = object (self : cap)
    method call _ caps =
      RO_array.iter (fun c -> c#dec_ref) caps;
      broken_struct (`Exception ex)
    method inc_ref = ()
    method dec_ref = ()
    method pp f = Exception.pp f ex
    method shortest = self
    method blocker = None
    method when_more_resolved _ = ()
    method check_invariants = ()
  end
  and broken_struct err = object (_ : struct_ref)
    method response = Some (Error err)
    method when_resolved fn = fn (Error err)
    method cap _ =
      match err with
      | `Exception ex -> broken_cap ex
      | `Cancelled -> broken_cap Exception.cancelled
    method pp f = Error.pp f err
    method finish = ()
    method blocker = None
    method check_invariants = ()
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

  let return (msg, caps) = object (_ : struct_ref)
    val mutable caps = caps

    method response = Some (Ok (msg, caps))

    method when_resolved fn = fn (Ok (msg, caps))

    method cap path =
      let i = Response.cap_index msg path in
      let cap = cap_in_cap_list_or_err i caps in
      cap#inc_ref;
      cap

    method pp f = Fmt.pf f "returned:%a" Response_payload.pp (msg, caps)

    method finish =
      RO_array.iter (fun c -> c#dec_ref) caps;
      caps <- RO_array.empty

    method blocker = None

    method check_invariants =
      RO_array.iter (fun c -> c#check_invariants) caps
  end

  class virtual service = object (self : #cap)
    inherit ref_counted

    method virtual call : Request.t -> cap RO_array.t -> struct_ref
    method private release = ()
    method pp f = Fmt.string f "<service>"
    method shortest = (self :> cap)
    method blocker = None
    method when_more_resolved _ = ()
  end

  let fail ?(ty=`Failed) msg =
    msg |> Fmt.kstrf @@ fun reason ->
    broken_struct (`Exception {Exception.ty; reason})

  let resolved = function
    | Ok x -> return x
    | Error msg -> broken_struct msg
end
