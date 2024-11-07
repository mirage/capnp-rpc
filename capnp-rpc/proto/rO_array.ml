type 'a t = 'a array

let init = Array.init
let of_list = Array.of_list
let get_exn t i = t.(i)
let length = Array.length
let map = Array.map
let mapi = Array.mapi
let iter = Array.iter
let iteri = Array.iteri
let fold_left = Array.fold_left

let get ~oob t i =
  if i < 0 || i >= Array.length t then oob
  else Array.get t i

let find fn t =
  let rec loop i =
    if i = Array.length t then None
    else (
      let item = t.(i) in
      if fn item then Some item
      else loop (i + 1)
    )
  in
  loop 0

let empty = [| |]
let pp x = Fmt.(brackets (array ~sep:(const string ", ") x))

let equal eq a b =
  let l = Array.length a in
  if l <> Array.length b then false
  else (
    let rec loop i =
      if i = 0 then true
      else (
        let i = i - 1 in
        eq a.(i) b.(i) && loop i
      )
    in
    loop l
  )

let release t v =
  for i = 0 to Array.length t - 1 do
    t.(i) <- v;
  done
