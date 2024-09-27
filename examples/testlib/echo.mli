open Capnp_rpc_lwt

type t = [`Echo_bb48258560861cec] Capability.t

val local : unit -> t
(** [local ()] is a capability to a new local echo service. *)

val ping : t -> ?slow:bool -> string -> string
(** [ping t msg] sends [msg] to [t] and returns its response.
    If [slow] is given, the service will wait until [unblock] is called before replying. *)

val ping_result : t -> ?slow:bool -> string -> (string, [> `Capnp of Capnp_rpc.Error.t]) result
(** [ping t msg] sends [msg] to [t] and returns its response.
    If [slow] is given, the service will wait until [unblock] is called before replying. *)

val unblock : t -> unit
(** [unblock t] tells the service to return any blocked ping responses. *)
