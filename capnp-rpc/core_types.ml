module Make(C : S.CONCRETE) = struct
  include  C

  type 'a or_error = ('a, Error.t) result

  class type struct_ref = object
    method when_resolved : ((Response.t * cap RO_array.t) or_error -> unit) -> unit
    method response : (Response.t * cap RO_array.t) or_error option
    method pp : Format.formatter -> unit
    method finish : unit
    method cap : Path.t -> cap
  end
  and cap = object
    method call : Request.t -> cap RO_array.t -> struct_ref   (* Takes ownership of [caps] *)
    method pp : Format.formatter -> unit
    method inc_ref : unit
    method dec_ref : unit
    method shortest : cap
  end

  class type struct_resolver = object
    inherit struct_ref
    method resolve : (Response.t * cap RO_array.t) or_error -> unit
    method connect : struct_ref -> unit
  end

  let pp_cap f x = x#pp f
  let pp_cap_list f caps = RO_array.pp pp_cap f caps

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
  end

  let rec broken_cap msg = object (self : cap)
    method call _ caps =
      RO_array.iter (fun c -> c#dec_ref) caps;
      broken (`Exception msg)
    method inc_ref = ()
    method dec_ref = ()
    method pp f = Fmt.pf f "broken:%s" msg
    method shortest = self
  end
  and broken err = object (_ : struct_ref)
    method response = Some (Error err)
    method when_resolved fn = fn (Error err)
    method cap _ =
      match err with
      | `Exception msg -> broken_cap msg
      | `Cancelled -> broken_cap "Cancelled"
    method pp f = Fmt.pf f "broken:%a" Error.pp err
    method finish = ()
  end

  let null = broken_cap "null"

  let cap_failf msg = msg |> Fmt.kstrf broken_cap

  let cap_in_cap_list i caps =
    match i with
    | None -> null
    | Some i when i < 0 || i >= RO_array.length caps -> cap_failf "Invalid cap index %d in %a" i pp_cap_list caps
    | Some i -> RO_array.get caps i

  let cap_in_payload i (_, caps) = cap_in_cap_list i caps

  let cap_of_err = function
    | `Exception msg -> broken_cap msg
    | `Cancelled -> broken_cap "Cancelled"

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
  end

  let return (msg, caps) = object (_ : struct_ref)
    val mutable caps = caps

    method response = Some (Ok (msg, caps))

    method when_resolved fn = fn (Ok (msg, caps))

    method cap path =
      let i = Response.cap_index msg path in
      let cap = cap_in_cap_list i caps in
      cap#inc_ref;
      cap

    method pp f = Fmt.pf f "returned:%a" Response_payload.pp (msg, caps)

    method finish =
      RO_array.iter (fun c -> c#dec_ref) caps;
      caps <- RO_array.empty
  end

  let resolved = function
    | Ok x -> return x
    | Error msg -> broken msg
end
