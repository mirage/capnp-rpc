(** Diagnostics. *)

module Log : Logs.LOG
(** The library's logger. *)

val src : Logs.src
(** Control the log level for [Log]. *)

val qid_tag : Stdint.Uint32.t Logs.Tag.def
(** [qid_tag] is used in log reports to tag the question (or answer) ID in the call. *)

exception Invariant_broken of (Format.formatter -> unit)

val pp_exn : exn Fmt.t
(** [pp_exn] is like [Fmt.exn], but pretty-prints [Invariant_broken]. *)

val failf : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [failf msg] raises [Failure msg]. *)

val invariant_broken : (Format.formatter -> unit) -> 'a
(** [invariant_broken msg] raises [Invariant_broken msg]. *)

module OID : sig
  (** Unique object IDs, for debugging. *)

  type t
  (** A unique ID which can be attached to objects to aid debugging. *)

  val next : unit -> t
  (** [next ()] is a fresh ID, unique since the last [reset]. *)

  val pp : t Fmt.t

  val reset : unit -> unit
  (** Reset the counter. Possibly useful in unit or fuzz tests. *)
end
