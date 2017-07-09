type 'a t
(** An ['a t] is a weak pointer to an ['a]. *)

val empty : unit -> 'a t
(** [empty ()] is a fresh empty pointer. *)

val wrap : 'a -> 'a t
(** [wrap x] is a weak pointer to [x]. *)

val get : 'a t -> 'a option
(** [get t] is the target of [t], or [None] if it has been GC'd. *)

val set : 'a t -> 'a -> unit
(** [set t x] puts [x] in the pointer. *)

val clear : 'a t -> unit
(** [clear t] empties the pointer. *)
