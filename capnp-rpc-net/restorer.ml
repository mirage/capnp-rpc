open Lwt.Infix
open Capnp_rpc_lwt

module Core_types = Private.Capnp_core.Core_types
module Log = Capnp_rpc.Debug.Log

module Id = struct
  type t = string

  let generate () =
    Nocrypto.Rng.generate 20 |> Cstruct.to_string

  let public x = x

  let derived ~secret name =
    Nocrypto.Hash.mac `SHA256 ~key:(Cstruct.of_string secret) (Cstruct.of_string name)
    |> Cstruct.to_string

  let digest alg t =
    let alg = (alg :> Nocrypto.Hash.hash) in
    Nocrypto.Hash.digest alg (Cstruct.of_string t)
    |> Cstruct.to_string

  let to_string x = x

  let equal = ( = )
  let pp = Fmt.string
end

type resolution = (Core_types.cap, Capnp_rpc.Exception.t) result

module type LOADER = sig
  type t
  val hash : t -> Auth.hash
  val make_sturdy : t -> Id.t -> Uri.t
  val load : t -> 'a Sturdy_ref.t -> string -> resolution Lwt.t
end

type t = Id.t -> resolution Lwt.t

let grant x : resolution = Ok (Cast.cap_to_raw x)
let reject ex = Error ex

let unknown_service_id = reject (Capnp_rpc.Exception.v "Unknown persistent service ID")

let fn (r:t) =
  fun k object_id ->
    Lwt.async (fun () ->
        Lwt.try_bind
          (fun () -> r object_id)
          (fun r -> k r; Lwt.return_unit)
          (fun ex ->
             Log.err (fun f -> f "Uncaught exception restoring object: %a" Fmt.exn ex);
             k (reject (Capnp_rpc.Exception.v "Internal error restoring object"));
             Lwt.return_unit
          )
      )

let restore (f:t) x = f x |> Lwt_result.map Cast.cap_of_raw

let none : t = fun _ ->
  Lwt.return @@ Error (Capnp_rpc.Exception.v "This vat has no restorer")

let single id cap =
  let cap = Cast.cap_to_raw cap in
  (* Hash the ID to prevent timing attacks. *)
  let id = Nocrypto.Hash.digest `SHA256 (Cstruct.of_string id) in
  fun requested_id ->
    let requested_id = Nocrypto.Hash.digest `SHA256 (Cstruct.of_string requested_id) in
    if Cstruct.equal id requested_id then (
      Core_types.inc_ref cap;
      Lwt.return (Ok cap)
    ) else Lwt.return unknown_service_id

module Table = struct
  type digest = string

  type entry =
    | Cached of resolution Lwt.t
    | Manual of Core_types.cap          (* We hold a ref on the cap *)

  type t = {
    hash : Nocrypto.Hash.hash;
    cache : (digest, entry) Hashtbl.t;
    load : Id.t -> digest -> resolution Lwt.t;
    make_sturdy : Id.t -> Uri.t;
  }

  (* [cache] contains promises or capabilities with positive ref-counts. *)

  let create make_sturdy =
    let hash = `SHA256 in
    let cache = Hashtbl.create 53 in
    let load _ _ = Lwt.return unknown_service_id in
    { hash; cache; load; make_sturdy }

  let hash t id =
    Id.digest t.hash id

  let resolve t id =
    let digest = hash t id in
    match Hashtbl.find t.cache digest with
    | Manual cap ->
      Core_types.inc_ref cap;
      Lwt.return @@ Ok cap
    | Cached res ->
      begin res >>= function
        | Error _ as e -> Lwt.return e
        | Ok cap ->
          Core_types.inc_ref cap;
          Lwt.pause () >|= fun () ->
          Ok cap
      end
    | exception Not_found ->
      let cap = t.load id digest in
      Hashtbl.add t.cache digest (Cached cap);
      Lwt.try_bind
        (fun () -> cap)
        (fun result ->
           begin match result with
             | Error _ -> Hashtbl.remove t.cache digest
             | Ok cap -> cap#when_released (fun () -> Hashtbl.remove t.cache digest)
           end;
           (* Ensure all [inc_ref]s are done before handing over to the user. *)
           Lwt.pause () >|= fun () ->
           result
        )
        (fun ex ->
           Hashtbl.remove t.cache digest;
           Lwt.fail ex
        )

  let of_loader (type l) (module L : LOADER with type t = l) loader =
    let hash = (L.hash loader :> Nocrypto.Hash.hash) in
    let cache = Hashtbl.create 53 in
    let rec load id digest =
      let sr : Private.Capnp_core.sturdy_ref = object
        method connect = resolve t id
        method to_uri_with_secrets = L.make_sturdy loader id
      end in
      L.load loader (Cast.sturdy_of_raw sr) digest
    and t = { hash; cache; load; make_sturdy = L.make_sturdy loader } in
    t

  let add t id cap =
    let cap = Cast.cap_to_raw cap in
    let id = hash t id in
    assert (not (Hashtbl.mem t.cache id));
    Hashtbl.add t.cache id (Manual cap)

  let sturdy_ref t id =
    Cast.sturdy_of_raw @@ object
      method connect = resolve t id
      method to_uri_with_secrets = t.make_sturdy id
    end

  let release = function
    | Manual cap -> Core_types.dec_ref cap;
    | Cached _ -> ()

  let remove t id =
    let id = hash t id in
    match Hashtbl.find t.cache id with
    | exception Not_found -> failwith "Service ID not in restorer table"
    | value ->
      release value;
      Hashtbl.remove t.cache id

  let clear t =
    Hashtbl.iter (fun _ v -> release v) t.cache;
    Hashtbl.clear t.cache
end

let of_table = Table.resolve
