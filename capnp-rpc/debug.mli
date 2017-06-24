module Log : Logs.LOG

val qid_tag : Uint32.t Logs.Tag.def
(** [qid_tag] is used in log reports to tag the question (or answer) ID in the call. *)

exception Invariant_broken of (Format.formatter -> unit)

val pp_exn : exn Fmt.t
(** [pp_exn] is like [Fmt.exn], but pretty-prints [Invariant_broken]. *)

val failf : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [failf msg] raises [Failure msg]. *)

val invariant_broken : (Format.formatter -> unit) -> 'a
(** [invariant_broken msg] raises [Invariant_broken msg]. *)
