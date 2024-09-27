open Capnp_rpc_lwt
open Capnp_rpc_net

include Restorer.LOADER

type loader = [`Logger_beacebd78653e9af] Sturdy_ref.t -> label:string -> Restorer.resolution
(** A function to create a new in-memory logger with the given label and sturdy-ref. *)

val create : make_sturdy:(Restorer.Id.t -> Uri.t) -> _ Eio.Path.t -> t * loader Eio.Promise.u
(** [create ~make_sturdy dir] is a database that persists services in [dir] and
    a resolver to let you set the loader (we're not ready to set the loader
    when we create the database). *)

val save_new : t -> label:string -> Restorer.Id.t
(** [save_new t ~label] adds a new logger with label [label] to the store and
    returns its newly-generated ID. *)
