open Capnp_rpc_lwt
open Capnp_rpc_net

type digest = string

module DB : sig
  type t
  (** An in-memory data-store. *)

  val hash : Auth.hash
  (** [hash] is the algorithm used to calculate digests. *)

  val create : unit -> t
  (** [create ()] is a new empty database. *)

  val add : t -> Restorer.Id.t
  (** [add t] returns the ID of a new empty file. *)

  val set : t -> digest -> string -> unit
  val get : t -> digest -> string
  val mem : t -> digest -> bool
end = struct
  let hash : Capnp_rpc_net.Auth.hash = `SHA256

  type t = (digest, string) Hashtbl.t

  let digest = Restorer.Id.digest hash

  let create () =
    Hashtbl.create 7

  let add t =
    let id = Restorer.Id.generate () in
    Hashtbl.add t (digest id) "";
    id

  let get t digest =
    Hashtbl.find t digest

  let set t digest data =
    Hashtbl.replace t digest data

  let mem t digest =
    Hashtbl.mem t digest
end

module File = struct
  type t = Api.Client.File.t Capability.t

  let set t data =
    let open Api.Client.File.Set in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.data_set params data;
    Capability.call_for_unit_exn t method_id request

  let get t =
    let open Api.Client.File.Get in
    let request = Capability.Request.create_no_args () in
    Capability.call_for_value_exn t method_id request |> Results.data_get

  let local (db:DB.t) sr digest =
    let module File = Api.Service.File in
    Persistence.with_sturdy_ref sr File.local @@ object
      inherit File.service

      method get_impl _ release_params =
        let open File.Get in
        release_params ();
        let resp, results = Service.Response.create Results.init_pointer in
        Results.data_set results (DB.get db digest);
        Service.return resp

      method set_impl params release_params =
        let open File.Set in
        let data = Params.data_get params in
        release_params ();
        DB.set db digest data;
        Service.return_empty ()
    end

  module Loader = struct
    type t = {
      db : DB.t;
      make_sturdy : Restorer.Id.t -> Uri.t;
    }

    let hash _ = DB.hash

    let make_sturdy t = t.make_sturdy

    let load t sr digest =
      if DB.mem t.db digest then (
        let sr = Sturdy_ref.cast sr in
        Restorer.grant @@ local t.db sr digest
      ) else (
        Restorer.unknown_service_id
      )
  end

  let table ~sw ~make_sturdy db =
    Restorer.Table.of_loader ~sw (module Loader) {Loader.db; make_sturdy}
end

type t = Api.Client.Store.t Capability.t

let create_file t =
  let open Api.Client.Store.CreateFile in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_caps t method_id request Results.file_get_pipelined

(* The main store service. *)
let local ~restore db =
  let module Store = Api.Service.Store in
  Store.local @@ object
    inherit Store.service

    (* Allows the user to add a new file to [db], uses [restore] to instantiate a
       service for it, and returns a capability reference to the service. *)
    method create_file_impl _ release_params =
      let open Store.CreateFile in
      release_params ();
      let id = DB.add db in
      match Restorer.restore restore id with
      | Error e -> Service.error (`Exception e)
      | Ok x ->
        let resp, results = Service.Response.create Results.init_pointer in
        Results.file_set results (Some x);
        Capability.dec_ref x;
        Service.return resp
  end
