type t =
  | Field of int

val pp : t Fmt.t

val resolve : Schema.Reader.Payload.t -> t list -> int option
(** [resolve payload path] is the index in the payload's cap descriptor table
    of the interface at [path] within the payload's content. *)
