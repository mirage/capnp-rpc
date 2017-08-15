open Capnp_rpc_lwt

type t = [`Echo_bb48258560861cec] Capability.t

val service : unit -> t
(** [service ()] is a capability to a new local echo service. *)

module Client : sig
  val ping : t -> ?slow:bool -> string -> string Lwt.t
  (** [ping t msg] sends [msg] to [t] and returns its response.
      If [slow] is given, the service will wait until [unblock] is called before replying. *)

  val unblock : t -> unit Lwt.t
  (** [unblock t] tells the service to return any blocked ping responses. *)
end
