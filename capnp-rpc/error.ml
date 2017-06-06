type t = [
  | `Exception of string
  | `Cancelled
]

let pp f = function
  | `Exception msg -> Fmt.pf f "exn:%s" msg
  | `Cancelled -> Fmt.pf f "cancelled"
