(** Handling of capnp:// URI format addresses.
    This code is shared between the unix and mirage networks. *)

module Location : sig
  type t = [
    | `Unix of string
    | `TCP of string * int
  ]

  val pp : t Fmt.t

  val equal : t -> t -> bool
end

include S.ADDRESS with
  type t = Location.t * Auth.Digest.t
