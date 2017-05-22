type 'a t = 'a array

let of_list = Array.of_list
let get t i = t.(i)
let length = Array.length
let map = Array.map
let mapi = Array.mapi
let iter = Array.iter
let iteri = Array.iteri
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
