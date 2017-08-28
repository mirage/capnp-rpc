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
