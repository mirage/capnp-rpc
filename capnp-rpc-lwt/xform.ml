type t =
  | Field of int

let pp f = function
  | Field i -> Fmt.pf f "field-%d" i

