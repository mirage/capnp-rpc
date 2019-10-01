(** Unique identifiers. *)

open Stdint

module type S = sig
  type t = private Uint32.t
  val zero : t
  val succ : t -> t
  val uint32 : t -> Uint32.t
  val of_uint32 : Uint32.t -> t
  val pp : t Fmt.t
end

module Make ( ) : S = struct
  type t = Uint32.t
  let pp = Fmt.of_to_string Uint32.to_string
  let uint32 x = x
  let of_uint32 x = x
  let zero = Uint32.zero
  let succ = Uint32.succ
end
