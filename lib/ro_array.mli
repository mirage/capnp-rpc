type 'a t

val of_list : 'a list -> 'a t
val get : 'a t -> int -> 'a
val length : 'a t -> int
val map : ('a -> 'b) -> 'a t -> 'b t
val iteri : (int -> 'a -> unit) -> 'a t -> unit
val empty : 'a t
