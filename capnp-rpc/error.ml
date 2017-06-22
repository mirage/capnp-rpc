type t = [
  | `Exception of Exception.t
  | `Cancelled
]

let pp f = function
  | `Exception ex -> Exception.pp f ex
  | `Cancelled -> Fmt.pf f "cancelled"

let exn ?ty msg =
  msg |> Fmt.kstrf @@ fun reason ->
  `Exception (Exception.v ?ty reason)
