(** A ref-count type that raises an exception on overflow. *)

type t
(** A number used as a reference count. *)

val zero : t

val one : t

val leaked : t
(** [leaked] is used to represent a ref-count that is invalid because we detected a GC leak. *)

val sum : pp:(Format.formatter -> unit) -> t -> int -> t
(** [sum ~pp t d] is [t + d].
    Raises an exception (including [pp]) if this would overflow or become negative, or if [t] is [zero] or [leaked]. *)

val succ : pp:(Format.formatter -> unit) -> t -> t
(** [succ ~pp t] is [add ~pp t 1]. *)

val pred : pp:(Format.formatter -> unit) -> t -> t
(** [pred ~pp t] is [add ~pp t (-1)]. *)

val is_zero : t -> bool
(** [is_zero t] is [true] if the ref-count is zero (the resource should be released).
    [is_zero leaked = false], since whatever detected the leak should free it. *)

val pp : t Fmt.t

val check : pp:(Format.formatter -> unit) -> t -> unit
(** [check ~pp t] raises an exception (including [pp]) if [t] is zero or leaked.
    Useful in sanity checks. *)

val to_int : t -> int option
(** [to_int t] is the non-negative integer ref-count, or [None] if [t = leaked]. *)
