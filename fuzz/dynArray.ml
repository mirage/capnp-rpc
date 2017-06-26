type 'a t = {
  mutable items : 'a array;
  mutable len : int;
  default : 'a;
}

let create default = {
  items = Array.make 10 default;
  len = 0;
  default;
}

let add t v =
  if t.len = Array.length t.items then (
    t.items <- Array.init (t.len * 2) (fun i ->
        if i < t.len then t.items.(i)
        else t.default
      )
  );
  t.items.(t.len) <- v;
  t.len <- t.len + 1

let pick t =
  if t.len = 0 then None
  else Some (t.items.(Choose.int t.len))

let iter fn t =
  for i = 0 to t.len - 1 do
    fn t.items.(i)
  done

let dump ~compare pp f t =
  let items = Array.sub t.items 0 t.len in
  Array.sort compare items;
  Fmt.array ~sep:Fmt.cut pp f items
let dump ~compare pp f = Fmt.fmt "[@[<v0>%a@]]" f (dump ~compare pp)

let pop t =
  if t.len = 0 then None
  else (
    let i = Choose.int t.len in
    let v = t.items.(i) in
    t.len <- t.len - 1;
    t.items.(i) <- t.items.(t.len);
    Some v
  )

let pop_first t =
  if t.len = 0 then None
  else (
    let i = 0 in
    let v = t.items.(i) in
    t.len <- t.len - 1;
    t.items.(i) <- t.items.(t.len);
    Some v
  )
