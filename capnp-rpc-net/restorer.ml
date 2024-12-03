open Eio.Std
open Capnp_rpc

module Core_types = Private.Capnp_core.Core_types
module Log = Capnp_rpc.Debug.Log

module Id = struct
  type t = string

  let generate () =
    Mirage_crypto_rng.generate 20

  let public x = x

  let derived ~secret name =
    Digestif.SHA256.hmac_string ~key:secret name
    |> Digestif.SHA256.to_raw_string

  let digest alg t =
    let alg = (alg :> Digestif.hash') in
    let module H = (val Digestif.module_of_hash' alg : Digestif.S) in
    H.digest_string t |> H.to_raw_string

  let to_string x = x

  let equal = ( = )
  let pp = Fmt.string
end

type resolution = (Core_types.cap, Capnp_rpc.Exception.t) result

module type LOADER = sig
  type t
  val hash : t -> Auth.hash
  val make_sturdy : t -> Id.t -> Uri.t
  val load : t -> 'a Sturdy_ref.t -> string -> resolution
end

type t = Id.t -> resolution

let grant x : resolution = Ok (Cast.cap_to_raw x)
let reject ex = Error ex

let unknown_service_id = reject (Capnp_rpc.Exception.v "Unknown persistent service ID")

let fn (r:t) =
  fun k object_id ->
  match r object_id with
  | r -> k r
  | exception (Eio.Cancel.Cancelled _ as ex) ->
    k (reject Capnp_rpc.Exception.cancelled);
    raise ex
  | exception ex ->
    Log.err (fun f -> f "Uncaught exception restoring object: %a" Fmt.exn ex);
    k (reject (Capnp_rpc.Exception.v "Internal error restoring object"))

let restore (f:t) x = f x |> Result.map Cast.cap_of_raw

let none : t = fun _ ->
  Error (Capnp_rpc.Exception.v "This vat has no restorer")

let single id cap =
  let cap = Cast.cap_to_raw cap in
  (* Hash the ID to prevent timing attacks. *)
  let id = Digestif.SHA256.digest_string id |> Digestif.SHA256.to_raw_string in
  fun requested_id ->
    let requested_id = Digestif.SHA256.digest_string requested_id |> Digestif.SHA256.to_raw_string in
    if String.equal id requested_id then (
      Core_types.inc_ref cap;
      Ok cap
    ) else unknown_service_id

module Table = struct
  type digest = string

  type entry =
    | Cached of resolution Promise.or_exn
    | Manual of Core_types.cap          (* We hold a ref on the cap *)

  type t = {
    sw : Switch.t;
    hash : Digestif.hash';
    cache : (digest, entry) Hashtbl.t;
    load : Id.t -> digest -> resolution Promise.or_exn;
    make_sturdy : Id.t -> Uri.t;
  }

  (* [cache] contains promises or capabilities with positive ref-counts. *)

  let release = function
    | Manual cap -> Core_types.dec_ref cap;
    | Cached _ -> ()

  let clear t =
    Hashtbl.iter (fun _ v -> release v) t.cache;
    Hashtbl.clear t.cache

  let create ~sw make_sturdy =
    let hash = `SHA256 in
    let cache = Hashtbl.create 53 in
    let load _ _ = Promise.create_resolved (Ok unknown_service_id) in
    let t = { sw; hash; cache; load; make_sturdy } in
    Switch.on_release sw (fun () -> clear t);
    t

  let hash t id =
    Id.digest t.hash id

  let resolve t id =
    let digest = hash t id in
    match Hashtbl.find t.cache digest with
    | Manual cap ->
      Core_types.inc_ref cap;
      Ok cap
    | Cached res ->
      let res = Promise.await_exn res in
      Result.iter Core_types.inc_ref res;
      res
    | exception Not_found ->
      let cap = t.load id digest in
      Hashtbl.add t.cache digest (Cached cap);
      match Promise.await_exn cap with
      | exception ex ->
        Hashtbl.remove t.cache digest;
        raise ex
      | result ->
        begin match result with
          | Error _ -> Hashtbl.remove t.cache digest
          | Ok cap ->
            cap#when_released (fun () -> Hashtbl.remove t.cache digest);
            (* Ensure all [inc_ref]s are done before handing over to the user. *)
            try Fiber.yield ()
            with ex -> Core_types.dec_ref cap; raise ex
        end;
        result

  let of_loader (type l) ~sw (module L : LOADER with type t = l) loader =
    let hash = (L.hash loader :> Digestif.hash') in
    let cache = Hashtbl.create 53 in
    let rec load id digest =
      Fiber.fork_promise ~sw (fun () ->
          let sr : Private.Capnp_core.sturdy_ref = object
            method connect = resolve t id
            method to_uri_with_secrets = L.make_sturdy loader id
          end in
          L.load loader (Cast.sturdy_of_raw sr) digest
        )
    and t = { sw; hash; cache; load; make_sturdy = L.make_sturdy loader } in
    Switch.on_release sw (fun () -> clear t);
    t

  let add t id cap =
    let cap = Cast.cap_to_raw cap in
    let id = hash t id in
    assert (not (Hashtbl.mem t.cache id));
    Switch.check t.sw;
    Hashtbl.add t.cache id (Manual cap)

  let sturdy_ref t id =
    Cast.sturdy_of_raw @@ object
      method connect = resolve t id
      method to_uri_with_secrets = t.make_sturdy id
    end

  let remove t id =
    let id = hash t id in
    match Hashtbl.find t.cache id with
    | exception Not_found -> failwith "Service ID not in restorer table"
    | value ->
      release value;
      Hashtbl.remove t.cache id
end

let of_table = Table.resolve
