type t =
  | Field of int

let pp f = function
  | Field i -> Fmt.pf f "field-%d" i

let to_cap_index = function
  | None -> None
  | Some i -> Some (Stdint.Uint32.to_int i)

(* [walk ss x xs] is the interface cap index at path [x :: xs] within struct storage [ss]. *)
let rec walk ss x = function
  | Field x2 :: xs -> walk (Schema.ReaderOps.get_struct ss x) x2 xs
  | [] -> Schema.ReaderOps.get_interface ss x |> to_cap_index

let resolve payload path =
  let open Schema.Reader in
  match path with
  | [] -> Payload.content_get_interface payload |> to_cap_index    (* Bootstrap only *)
  | Field x :: xs ->
    let ss = Payload.content_get payload |> Schema.ReaderOps.deref_opt_struct_pointer in
    walk ss x xs
