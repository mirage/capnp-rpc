(** This is the OCaml version of the C++ capnp calculator example. *)

open Capnp_rpc_lwt

type calc = [`Calculator_97983392df35cc36]
type value = [`Value_c3e69d34d3ee48d2]
type fn = [`Function_ede83a3d96840394]

type expr =
  | Float of float              (** [Float v] evaluates to [v]. *)
  | Prev of value Capability.t  (** [Prev v] evaluates to the value returned by service [v]. *)
  | Param of int                (** [Param i] evaluates to parameter number [i] (starting from zero). *)
  | Call of fn Capability.t * expr list (** [Call fn args] evaluates to [fn args]. *)

module Client : sig
  type t = calc Capability.t

  module Value : sig
    type t = value Capability.t

    val read : t -> float Lwt.t
    (** [read t] reads the value of the remote value object. *)

    val final_read : t -> float Lwt.t
    (** [final_read t] reads the value and dec_ref's [t]. *)
  end

  module Fn : sig
    type t = fn Capability.t

    val call : t -> float list -> float Lwt.t
    (** [call fn args] does [fn args]. *)
  end

  val evaluate : t -> expr -> Value.t
  (** [evaluate t expr] evaluates [expr] using calculator service [t] and
      returns a capability to the result service. *)

  val getOperator : t -> [`Add | `Subtract | `Multiply | `Divide] -> Fn.t
  (** [getOperator t op] is a remote operator function provided by [t]. *)
end

val literal : float -> Client.Value.t
(** [literal v] is a capability to a local value service holding value [v]. *)

val fn : int -> expr -> Client.Fn.t
(** [fn n_args body] is a capability to a local service that takes [n_args] arguments
    and evaluates [body]. *)

val binop : (float -> float -> float) -> Client.Fn.t
(** [binop fn] is a capability to a local service providing function [op] (of two arguments). *)

val add : Client.Fn.t
(** [add] is [binop ( +. )] *)

val sub : Client.Fn.t
(** [sub] is [binop ( -. )] *)

val mul : Client.Fn.t
(** [mul] is [binop ( *. )] *)

val div : Client.Fn.t
(** [div] is [binop ( /. )] *)

val service : Client.t
(* A capability to a local calculator service *)
