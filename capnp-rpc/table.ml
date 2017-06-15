let failf fmt = Fmt.kstrf failwith fmt

module Allocating (Key : Id.S) = struct
  type 'a t = {
    mutable next : Key.t;       (* The lowest ID we've never used *)
    mutable free : Key.t list;  (* Keys we can recycle *)
    used : (Key.t, 'a) Hashtbl.t;
  }

  let make () =
    { next = Key.zero; free = []; used = Hashtbl.create 11 }

  let alloc t f =
    let use x =
      let v = f x in
      Hashtbl.add t.used x v;
      v
    in
    match t.free with
    | x::xs -> t.free <- xs; use x
    | [] ->
      let x = t.next in
      t.next <- Key.succ x;
      use x

  let release t x =
    assert (Hashtbl.mem t.used x);
    Hashtbl.remove t.used x;
    t.free <- x :: t.free

  let find_exn t x =
    try Hashtbl.find t.used x
    with Not_found ->
      failf "Key %a is no longer allocated!" Key.pp x

  let active t = Hashtbl.length t.used

  let pp_item pp f (k, v) =
    Fmt.pf f "%a -> @[%a@]" Key.pp k pp v

  let dump pp f t =
    let add k v acc = (k, v) :: acc in
    let items = Hashtbl.fold add t.used [] in
    let items = List.sort compare items in
    (Fmt.Dump.list (pp_item pp)) f items
end

module Tracking (Key : Id.S) = struct
  type 'a t = (Key.t, 'a) Hashtbl.t

  let make () = Hashtbl.create 17

  let set = Hashtbl.replace

  let release = Hashtbl.remove

  let find t k =
    match Hashtbl.find t k with
    | exception Not_found -> None
    | x -> Some x

  let find_exn t k =
    match Hashtbl.find t k with
    | exception Not_found -> failf "Key %a not found in table" Key.pp k
    | x -> x

  let active = Hashtbl.length

  let pp_item pp f (k, v) =
    Fmt.pf f "%a -> @[%a@]" Key.pp k pp v

  let dump pp f t =
    let add k v acc = (k, v) :: acc in
    let items = Hashtbl.fold add t [] in
    let items = List.sort compare items in
    (Fmt.Dump.list (pp_item pp)) f items
end
