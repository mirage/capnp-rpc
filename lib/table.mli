module Allocating ( ) : sig
  (** A table where we choose the keys. *)

  type key = private Uint32.t

  type 'a t
  (** A mapping from keys to values. *)

  val make : unit -> 'a t
  (** [make ()] is a fresh table. *)

  val alloc : 'a t -> 'a -> key
  (** [alloc t value] stores [value] in [t] under an unused key and returns the key. *)

  val release : 'a t -> key -> unit
  (** [release t k] releases a key previously allocated with [alloc]. [k] must not be used after this. *)

  val find_exn : 'a t -> key -> 'a
  (** [find_exn t k] is the value associated with [k].
      Raises an exception if [k] isn't bound. *)

  val uint32 : key -> Uint32.t
  val of_uint32 : Uint32.t -> key

  val pp_key : key Fmt.t
end

module Tracking ( ) : sig
  (** A table where our peer chooses the keys. *)

  type key = private Uint32.t

  type 'a t
  (** A mapping from keys to values. *)

  val make : unit -> 'a t
  (** [make ()] is a fresh table. *)

  val set : 'a t -> key -> 'a -> unit
  (** [set t key value] stores [key=value] in [t]. *)

  val release : 'a t -> key -> unit
  (** [release t k] releases a binding previously allocated with [set]. [k] must not be used after this. *)

  val find_exn : 'a t -> key -> 'a
  (** [find_exn t k] is the value associated with [k].
      Raises an exception if [k] isn't bound. *)

  val uint32 : key -> Uint32.t
  val of_uint32 : Uint32.t -> key

  val pp_key : key Fmt.t
end
