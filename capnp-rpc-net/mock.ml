open Lwt
open Capnp_rpc_lwt

module Persistent_store (T : sig type args end) = struct
  module Secret = Restorer.Id
  module Collection_map = Map.Make(String)

  type load_fn =
      Load : (validate:(unit -> bool) ->
              sturdy_ref:'a Sturdy_ref.t ->
              T.args ->
              Restorer.resolution Lwt.t) -> load_fn [@@unboxed]

  module Loader = struct

    (* Note: we could simplify this by removing [collections], or even [db] and
       just keeping everything in the table. However, this code is intended to
       be used as a template for writing real stores, which will typically need
       some unique string to identify collections across restarts. *)
    type t = {
      make_sturdy_uri : Secret.t -> Uri.t;
      db : (string, string * T.args) Hashtbl.t;         (* digest -> (collection, args) *)
      mutable collections : load_fn Collection_map.t;
    }

    let create ~make_sturdy_uri () =
      let db = Hashtbl.create 10 in
      let collections = Collection_map.empty in
      { make_sturdy_uri; db; collections }

    let hash _ = `SHA256

    let validate t digest () =
      Hashtbl.mem t.db digest

    let load t self digest =
      match Hashtbl.find_opt t.db digest with
      | None -> Lwt.return Restorer.unknown_service_id
      | Some (cname, data) ->
        match Collection_map.find_opt cname t.collections with
        | None -> Fmt.failwith "Unregistered collection %S found in database!" cname
        | Some (Load fn) -> fn ~validate:(validate t digest) ~sturdy_ref:(Capnp_rpc_lwt.Sturdy_ref.cast self) data

    let make_sturdy t = t.make_sturdy_uri
  end

  type t = {
    loader : Loader.t;
    table : Restorer.Table.t;
    restorer : Restorer.t;
  }

  let create ~make_sturdy_uri () =
    let loader = Loader.create ~make_sturdy_uri () in
    let table = Restorer.Table.of_loader (module Loader) loader in
    let restorer = Restorer.of_table table in
    { loader; table; restorer }, restorer

  let register_collection t name load =
    let loader = t.loader in
    if Collection_map.mem name loader.collections then Fmt.invalid_arg "Collection %S already registered!" name;
    loader.collections <- Collection_map.add name (Load load) loader.collections;
    Persistence.collection @@ object (self : ('a, 'args) Persistence.backend)
      method add args =
        let secret = Secret.generate () in
        let digest = Secret.digest (Loader.hash ()) secret in
        Hashtbl.add loader.db digest (name, args);
        Restorer.restore t.restorer secret >|= function
        | Ok cap -> cap
        | Error ex -> Capability.broken ex

      method remove digest =
        if not (Hashtbl.mem loader.db digest) then raise Not_found;
        Hashtbl.remove loader.db digest;
        Restorer.Table.remove_digest t.table digest

      method list =
        Hashtbl.fold (fun k (n, args) acc -> if n = name then (k, args) :: acc else acc) loader.db []

      method find_all args =
        List.filter_map (fun (k, a) -> if a = args then Some k else None) self#list
    end

  let table t = t.table
  let restorer t = t.restorer
end
