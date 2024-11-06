open Capnp_rpc.Std

module Version : sig
  type t = [`Version_ed7d11372e0a7243] Capability.t

  val read : t -> string Lwt.t
end

type t = [`Registry_d9975f668b337b6d] Capability.t

val set_echo_service : t -> Echo.t -> unit Lwt.t

val echo_service : t -> Echo.t
(** Waits until unblocked before returning. *)

val echo_service_promise : t -> Echo.t
(** Returns a promise immediately. Resolves promise when unblocked.
    (should appear to work the same as [echo_service] to users) *)

val unblock : t -> unit Lwt.t

val complex : t -> Echo.t * Version.t
(** [complex t] returns two capabilities in a single, somewhat complex, message. *)

val local : unit -> t
(** [local ()] is a new local registry. *)
