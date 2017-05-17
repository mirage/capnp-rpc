type pointer =
  | Cap of Rpc.value
  | Struct of pointer Ro_array.t

let value (pointers:pointer Ro_array.t) =
  object (self : Rpc.value)
    method apply xs =
      let rec walk ~ptrs = function
        | [] -> self
        | (Xform.Field f) :: xs ->
          match Ro_array.get ptrs f with
          | Cap c when xs = [] -> c
          | Cap _ -> failwith "Can't transform a capability!"
          | Struct ptrs -> walk ~ptrs xs
      in
      walk ~ptrs:pointers xs

    method call xs p caps =
      match xs with
      | [] -> failwith "Can't call a value!"
      | _ -> (self#apply xs)#call [] p caps
  end

let of_content ~caps _payload =
  (* XXX: this is very wrong; need to parse [payload]. *)
  value (Ro_array.map (fun c -> Cap c) caps)
