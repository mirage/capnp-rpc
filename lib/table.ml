let failf fmt = Fmt.kstrf failwith fmt

module Allocating ( ) = struct
  type key = Uint32.t

  type 'a t = {
    mutable next : key;       (* The lowest ID we've never used *)
    mutable free : key list;  (* Keys we can recycle *)
    used : (key, 'a) Hashtbl.t;
  }

  let make () =
    { next = Uint32.zero; free = []; used = Hashtbl.create 11 }

  let alloc t v =
    let use x =
      Hashtbl.add t.used x v;
      x
    in
    match t.free with
    | x::xs -> t.free <- xs; use x
    | [] ->
      let x = t.next in
      t.next <- Uint32.succ x;
      use x

  let release t x =
    Hashtbl.remove t.used x;
    t.free <- x :: t.free

  let find_exn t x =
    try Hashtbl.find t.used x
    with Not_found ->
      failf "Key %a is no longer allocated!" Uint32.printer x

  let uint32 x = x
  let of_uint32 x = x
  let pp_key = Fmt.of_to_string Uint32.to_string
end

module Tracking ( ) = struct
  type key = Uint32.t

  type 'a t = (key, 'a) Hashtbl.t

  let make () = Hashtbl.create 17

  let set = Hashtbl.replace

  let release = Hashtbl.remove

  let find_exn t k =
    match Hashtbl.find t k with
    | exception Not_found -> failf "Key %a not found in table" Uint32.printer k
    | x -> x

  let uint32 x = x
  let of_uint32 x = x
  let pp_key = Fmt.of_to_string Uint32.to_string
end
