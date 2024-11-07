(** A read-only array. *)

type 'a t

val of_list : 'a list -> 'a t
val init : int -> (int -> 'a) -> 'a t
val get_exn : 'a t -> int -> 'a
val get : oob:'a -> 'a t -> int -> 'a
val length : 'a t -> int
val map : ('a -> 'b) -> 'a t -> 'b t
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int -> 'a -> unit) -> 'a t -> unit
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val find : ('a -> bool) -> 'a t -> 'a option
val empty : 'a t
val pp : 'a Fmt.t -> 'a t Fmt.t
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val release : 'a t -> 'a -> unit
(** [release t null] replaces every element in the array with [null].
    This is useful to mark the array as finished. *)
