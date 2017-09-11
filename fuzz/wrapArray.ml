type 'a t = {
  items : 'a option array;
  free : 'a -> unit;
}

let create ~free size = {
  items = Array.make size None;
  free;
}

let add t v =
  let i =
    try Choose.int (Array.length t.items)
    with Choose.End_of_fuzz_data -> 0
  in
  let old = t.items.(i) in
  t.items.(i) <- Some v;
  match old with
  | None -> ()
  | Some old -> t.free old

let pick t =
  t.items.(Choose.int (Array.length t.items))

let iter fn t =
  Array.iter (function
      | None -> ()
      | Some item -> fn item
    ) t.items

let free t =
  for i = 0 to Array.length t.items - 1 do
    match t.items.(i) with
    | None -> ()
    | Some item ->
      t.free item;
      t.items.(i) <- None
  done

let dump ~compare pp f t =
  let items = Array.fold_left (fun acc -> function
      | None -> acc
      | Some x -> x :: acc) [] t.items in
  let items = List.sort compare items in
  Fmt.list ~sep:Fmt.cut pp f items
let dump ~compare pp f = Fmt.fmt "[@[<v0>%a@]]" f (dump ~compare pp)

let get t i = t.items.(i)

(* Note: does NOT free the item *)
let remove t i = t.items.(i) <- None

let choose_i t =
  Choose.int (Array.length t.items)

let pop t =
  let i = choose_i t in
  let v = get t i in
  t.items.(i) <- None;
  v
