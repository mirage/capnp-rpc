(* A ref-count type that raises an exception on overflow. *)

type t
(** A number used as a reference count. *)

val zero : t

val one : t

val leaked : t
(** [leaked] is used to represent a ref-count that is invalid because we detected a GC leak.
    Has the value [-1] (for [to_int] or polymorphic compare). *)

val succ : pp:(Format.formatter -> unit) -> t -> t
(** [succ ~pp t] is t plus one.
    Raises an exception (including [pp]) if this overflows, or if [t] is [zero] or [leaked]. *)

val pred : pp:(Format.formatter -> unit) -> t -> t
(** [pred ~pp t] is t minus one. Raises an exception (including [pp]) if [t] is [zero].
    [pred ~pp leaked = leaked], so that one leaked resource freeing another doesn't generate further errors. *)

val is_zero : t -> bool
(** [is_zero t] is [true] if the ref-count is zero (the resource should be released).
    [is_zero leaked = false], since whatever detected the leak should free it. *)

val pp : t Fmt.t

val check : pp:(Format.formatter -> unit) -> t -> unit
(** [check ~pp t] raises an exception (including [pp]) if [t] is zero or leaked.
    Useful in sanity checks. *)

val to_int : t -> int
