(** This is the OCaml version of the C++ capnp calculator example. *)

open Capnp_rpc_lwt

type t = [`Calculator_97983392df35cc36] Capability.t

module rec Value : sig
  type t = [`Value_c3e69d34d3ee48d2] Capability.t

  val read : t -> float
  (** [read t] reads the value of the remote value object. *)

  val final_read : t -> float
  (** [final_read t] reads the value and dec_ref's [t]. *)

  val local : float -> t
  (** [local v] is a capability to a local value service holding value [v]. *)
end

and Fn : sig
  type t = [`Function_ede83a3d96840394] Capability.t

  val call : t -> float list -> float
  (** [call fn args] does [fn args]. *)

  val local : int -> Expr.t -> Fn.t
  (** [local n_args body] is a capability to a local service that takes [n_args] arguments
      and evaluates [body]. *)

  val local_binop : (float -> float -> float) -> Fn.t
  (** [local_binop fn] is a capability to a local service providing function [fn] (of two arguments). *)

  val add : t
  (** [add] is [local_binop ( +. )] *)

  val sub : t
  (** [sub] is [local_binop ( -. )] *)

  val mul : t
  (** [mul] is [local_binop ( *. )] *)

  val div : t
  (** [div] is [local_binop ( /. )] *)
end

and Expr : sig
  type t =
    | Float of float              (** [Float v] evaluates to [v]. *)
    | Prev of Value.t             (** [Prev v] evaluates to the value returned by service [v]. *)
    | Param of int                (** [Param i] evaluates to parameter number [i] (starting from zero). *)
    | Call of Fn.t * Expr.t list  (** [Call fn args] evaluates to [fn args]. *)
end

val evaluate : t -> Expr.t -> Value.t
(** [evaluate t expr] evaluates [expr] using calculator service [t] and
    returns a capability to the result service. *)

val getOperator : t -> [`Add | `Subtract | `Multiply | `Divide] -> Fn.t
(** [getOperator t op] is a remote operator function provided by [t]. *)

val local : sw:Eio.Switch.t -> t
(** A capability to a local calculator service.
    It may immediately return a promise of a result, while continuing the calculation in [sw]. *)
