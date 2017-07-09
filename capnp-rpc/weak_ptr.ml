type 'a t = 'a Weak.t

let empty () =
  Weak.create 1

let set t x =
  Weak.set t 0 (Some x)

let clear t =
  Weak.set t 0 None

let wrap x =
  let t = empty () in
  set t x;
  t

let get t =
  Weak.get t 0
