type 'a t

val of_list : 'a list -> 'a t
val get : 'a t -> int -> 'a
val length : 'a t -> int
val map : ('a -> 'b) -> 'a t -> 'b t
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int -> 'a -> unit) -> 'a t -> unit
val empty : 'a t
val pp : 'a Fmt.t -> 'a t Fmt.t
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
