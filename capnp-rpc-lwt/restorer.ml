module Core_types = Capnp_core.Core_types
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
end

type resolution = (Core_types.cap, Capnp_rpc.Exception.t) result

type t = Id.t -> resolution Lwt.t

let grant x = Ok x
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

let restore f x = f x

let none : t = fun _ ->
  Lwt.return @@ Error (Capnp_rpc.Exception.v "This vat has no restorer")

let single id cap =
  (* Hash the ID to prevent timing attacks. *)
  let id = Nocrypto.Hash.digest `SHA256 (Cstruct.of_string id) in
  fun requested_id ->
    let requested_id = Nocrypto.Hash.digest `SHA256 (Cstruct.of_string requested_id) in
    if Cstruct.equal id requested_id then Lwt.return (Ok cap)
    else Lwt.return unknown_service_id

module Table = struct
  type t = (string, Core_types.cap) Hashtbl.t

  let create () = Hashtbl.create 7

  let hash = Id.digest `SHA256

  let add t id cap =
    let id = hash id in
    assert (not (Hashtbl.mem t id));
    Hashtbl.add t id cap

  let remove t id =
    let id = hash id in
    match Hashtbl.find t id with
    | exception Not_found -> failwith "Service ID not in restorer table"
    | cap ->
      Core_types.dec_ref cap;
      Hashtbl.remove t id

  let clear t =
    Hashtbl.iter (fun _ c -> Core_types.dec_ref c) t;
    Hashtbl.clear t

  let restorer t =
    fun id ->
      let id = hash id in
      match Hashtbl.find t id with
      | exception Not_found -> Lwt.return unknown_service_id
      | cap ->
        Core_types.inc_ref cap;
        Lwt.return @@ Ok cap
end

let of_table = Table.restorer
