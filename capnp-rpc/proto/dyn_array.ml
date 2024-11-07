type 'a t = {
  unused : 'a;
  mutable items : 'a array;
  mutable len : int;
}

let create ~unused size =
  let size = if size < 1 then 1 else size in
  let items = Array.make size unused in
  { unused; items; len = 0 }

let add t item =
  if t.len = Array.length t.items then (
    let new_cap = Array.length t.items * 2 in
    t.items <- Array.init new_cap (fun i ->
        if i < t.len then t.items.(i)
        else t.unused
      );
  );
  t.items.(t.len) <- item;
  t.len <- t.len + 1

let get_exn t i =
  if i < 0 || i >= t.len then failwith "Dyn_array: out of bounds array access";
  Array.get t.items i

let get ~oob t i =
  if i < 0 || i >= t.len then oob
  else Array.get t.items i

let replace t i v =
  if i < 0 || i >= t.len then failwith "Dyn_array: out of bounds array access";
  let old = t.items.(i) in
  Array.set t.items i v;
  old

let reset t =
  for i = 0 to t.len - 1 do
    t.items.(i) <- t.unused;
  done;
  t.len <- 0

let iter f t =
  let items = t.items in
  for i = 0 to t.len - 1 do
    f items.(i);
  done

let snapshot t =
  let items = t.items in
  RO_array.init t.len (Array.get items)

let length t = t.len

let pp pp f t =
  for i = 0 to t.len - 1 do
    if i > 0 then Fmt.pf f ",@ ";
    pp f t.items.(i)
  done
