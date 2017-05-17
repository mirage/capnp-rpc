type 'a t = 'a array

let of_list = Array.of_list
let get t i = t.(i)
let length = Array.length
let map = Array.map
let iteri = Array.iteri
let empty = [| |]
