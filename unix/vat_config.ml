let docs = "CAP'N PROTO OPTIONS"

module Auth = Capnp_rpc_net.Auth
module Log = Capnp_rpc.Debug.Log

module Listen_address = struct
  include Network.Location

  open Cmdliner

  let cmd =
    let i = Arg.info ~docs ["capnp-listen-address"] ~docv:"ADDR" ~doc:"Address to listen on, e.g. $(b,unix:/run/my.socket)." in
    Arg.(required @@ opt (some cmdliner_conv) None i)
end

module Secret_hash : sig
  type t

  val of_pem_data : string -> t
  val to_string : t -> string
end = struct
  type t = string

  let of_pem_data data = Mirage_crypto.Hash.SHA256.digest (Cstruct.of_string data) |> Cstruct.to_string
  let to_string x = x
end

type t = {
  backlog : int;
  secret_key : (Auth.Secret_key.t * Secret_hash.t) Lazy.t;
  serve_tls : bool;
  listen_address : Listen_address.t;
  public_address : Network.Location.t;
}

let secret_key t = fst @@ Lazy.force t.secret_key

let hashed_secret t = Secret_hash.to_string @@ snd @@ Lazy.force t.secret_key

let secret_key_file =
  let open Cmdliner in
  let i = Arg.info ~docs ["capnp-secret-key-file"] ~docv:"PATH"
      ~doc:"File in which to store secret key (or \"\" for an ephemeral key)." in
  Arg.(required @@ opt (some string) None i)

let read_whole_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
  let len = in_channel_length ic in
  really_input_string ic len

let write_whole_file path data =
  let oc = open_out_gen [Open_wronly; Open_creat; Open_excl; Open_binary] 0o600 path in
  Fun.protect ~finally:(fun () -> close_out oc) @@ fun () ->
  output_string oc data

let init_secret_key_file key_file =
  if Sys.file_exists key_file then (
    Log.info (fun f -> f "Restoring saved secret key from existing file %S" key_file);
    let data = read_whole_file key_file in
    (Auth.Secret_key.of_pem_data data, Secret_hash.of_pem_data data)
  ) else (
    Log.info (fun f -> f "Generating new secret key to store in %S" key_file);
    let secret_key = Auth.Secret_key.generate () in
    let data = Auth.Secret_key.to_pem_data secret_key in
    write_whole_file key_file data;
    (secret_key, Secret_hash.of_pem_data data)
  )

let create ?(backlog=5) ?public_address ~secret_key ?(serve_tls=true) listen_address =
  let public_address =
    match public_address with
    | Some x -> x
    | None -> listen_address
  in
  Network.Location.validate_public public_address;
  let secret_key = lazy (
    match secret_key with
    | `File path -> init_secret_key_file path
    | `PEM data -> (Auth.Secret_key.of_pem_data data, Secret_hash.of_pem_data data)
    | `Ephemeral ->
      let key = Auth.Secret_key.generate () in
      let data = Auth.Secret_key.to_pem_data key in
      (key, Secret_hash.of_pem_data data)
  ) in
  { backlog; secret_key; serve_tls; listen_address; public_address }

let secret_key_term =
  let get = function
    | "" -> `Ephemeral
    | path -> `File path
  in
  Cmdliner.Term.(const get $ secret_key_file)

let derived_id t name =
  let secret = hashed_secret t in
  Capnp_rpc_net.Restorer.Id.derived ~secret name

let auth t =
  if t.serve_tls then Capnp_rpc_net.Auth.Secret_key.digest (secret_key t)
  else Capnp_rpc_net.Auth.Digest.insecure

let sturdy_uri t service =
  let address = (t.public_address, auth t) in
  Network.Address.to_uri (address, Capnp_rpc_net.Restorer.Id.to_string service)

open Cmdliner

let pp f {backlog; secret_key; serve_tls; listen_address; public_address} =
  Fmt.pf f "{backlog=%d; fingerprint=%a; serve_tls=%b; listen_address=%a; public_address=%a}"
    backlog
    (Auth.Secret_key.pp_fingerprint `SHA256) (fst @@ Lazy.force secret_key)
    serve_tls
    Listen_address.pp listen_address
    Network.Location.pp public_address

let equal {backlog; secret_key; serve_tls; listen_address; public_address} b =
  backlog = b.backlog &&
  serve_tls = serve_tls &&
  Listen_address.equal listen_address b.listen_address &&
  Network.Location.equal public_address b.public_address &&
  Auth.Secret_key.equal (fst @@ Lazy.force secret_key) (fst @@ Lazy.force b.secret_key)

let public_address =
  let i = Arg.info ~docs ["capnp-public-address"] ~docv:"ADDR" ~doc:"Address to tell others to connect on." in
  Arg.(value @@ opt (some Network.Location.cmdliner_conv) None i)

let disable_tls =
  let i = Arg.info ~docs ["capnp-disable-tls"] ~doc:"Do not use TLS for incoming connections." in
  Arg.(value @@ flag i)

let cmd =
  let make secret_key disable_tls listen_address public_address =
    let public_address =
      match public_address with
      | None -> listen_address
      | Some x -> x
    in
    create ~secret_key ~serve_tls:(not disable_tls) ~public_address listen_address
  in
  Term.(const make $ secret_key_term $ disable_tls $ Listen_address.cmd $ public_address)
