(** Cap'n Proto exceptions. *)

type ty = [
  | `Failed
  | `Overloaded
  | `Disconnected
  | `Unimplemented
  | `Undefined of int
]

type t = {
  ty : ty;
  reason : string;
}

let pp_ty f x =
  Fmt.string f (match x with
      | `Failed        -> "Failed"
      | `Overloaded    -> "Overloaded"
      | `Disconnected  -> "Disconnected"
      | `Unimplemented -> "Unimplemented"
      | `Undefined x   -> "Undefined:" ^ string_of_int x
    )

let pp f ex =
  Fmt.pf f "%a: %s" pp_ty ex.ty ex.reason

let v ?(ty = `Failed) reason = { ty; reason }

let cancelled = v "Cancelled"
