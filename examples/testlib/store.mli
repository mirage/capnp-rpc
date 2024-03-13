(** A store of persistent files.
    The user can create a new file, get and set its contents, and get a sturdy ref to it.
    See [test_store] for an example using this. *)

open Capnp_rpc_lwt
open Capnp_rpc_net

module DB : sig
  type t
  (** An in-memory data-store. *)

  val create : unit -> t
  (** [create ()] is a new empty database. *)
end

module File : sig
  type t = [`File_aec5916d9557ed0e] Capability.t

  val set : t -> string -> unit
  (** [set t data] saves [data] as [t]'s contents. *)

  val get : t -> string
  (** [get t] is the current contents of [t]. *)

  val table : sw:Eio.Switch.t -> make_sturdy:(Restorer.Id.t -> Uri.t) -> DB.t -> Restorer.Table.t
  (** [table ~make_sturdy db] is a table of file services, backed by [db].
      [make_sturdy] is used to generate sturdy URIs for files. *)
end

type t = [`Store_adb0759941321f8d] Capability.t

val create_file : t -> File.t
(** [create_file t] is a new file in store [t]. *)

val local : restore:Restorer.t -> DB.t -> t
(** [local ~restore db] is a file store backed by DB. *)
