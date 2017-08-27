open Astring

module Auth = Capnp_rpc_lwt.Auth
module Log = Capnp_rpc.Debug.Log

module Listen_address = struct
  include Network.Socket_address

  let abs_path p =
    if Filename.is_relative p then
      Filename.concat (Sys.getcwd ()) p
    else p

  let parse_tcp s =
    match String.cut ~sep:":" s with
    | None -> Error (`Msg "Missing :PORT in listen address")
    | Some (host, port) ->
      match String.to_int port with
      | None -> Error (`Msg "PORT must be an integer")
      | Some port ->
        Ok (`TCP (host, port))

  let of_string s =
    match String.cut ~sep:":" s with
    | Some ("unix", path) -> Ok (`Unix (abs_path path))
    | Some ("tcp", tcp) -> parse_tcp tcp
    | None -> Error (`Msg "Missing ':'")
    | Some _ -> Error (`Msg "Only tcp:HOST:PORT and unix:PATH addresses are currently supported")

  open Cmdliner

  let addr_conv = Arg.conv (of_string, pp)

  let cmd =
    let i = Arg.info ["listen-address"] ~docv:"ADDR" ~doc:"Address to listen on, e.g. $(b,unix:/run/my.socket)." in
    Arg.(required @@ opt (some addr_conv) None i)
end

type t = {
  backlog : int;
  secret_key : Auth.Secret_key.t Lazy.t;
  serve_tls : bool;
  listen_address : Network.Socket_address.t;
  public_address : Network.Socket_address.t;
}

let secret_key t = Lazy.force t.secret_key

let secret_key_file =
  let open Cmdliner in
  let i = Arg.info ["secret-key-file"] ~docv:"PATH"
      ~doc:"File in which to store secret key (or \"\" for an ephemeral key)." in
  Arg.(required @@ opt (some string) None i)

let read_whole_file path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let data = really_input_string ic len in
  close_in ic;
  data

let write_whole_file path data =
  let oc = open_out_gen [Open_wronly; Open_creat; Open_excl; Open_binary] 0o700 path in
  output_string oc data;
  close_out oc

let init_secret_key_file key_file =
  if Sys.file_exists key_file then (
    Log.info (fun f -> f "Restoring saved secret key from existing file %S" key_file);
    Auth.Secret_key.of_pem_data (read_whole_file key_file)
  ) else (
    Log.info (fun f -> f "Generating new secret key to store in %S" key_file);
    let secret_key = Auth.Secret_key.generate () in
    write_whole_file key_file (Auth.Secret_key.to_pem_data secret_key);
    secret_key
  )

let create ?(backlog=5) ?public_address ~secret_key ?(serve_tls=true) listen_address =
  let public_address =
    match public_address with
    | Some x -> x
    | None -> listen_address
  in
  let secret_key = lazy (
    match secret_key with
    | `File path -> init_secret_key_file path
    | `PEM data -> Auth.Secret_key.of_pem_data data
    | `Ephemeral -> Auth.Secret_key.generate ()
  ) in
  { backlog; secret_key; serve_tls; listen_address; public_address }

let secret_key_term =
  let get = function
    | "" -> `Ephemeral
    | path -> `File path
  in
  Cmdliner.Term.(pure get $ secret_key_file)

open Cmdliner

let pp f {backlog; secret_key; serve_tls; listen_address; public_address} =
  Fmt.pf f "{backlog=%d; fingerprint=%a; serve_tls=%b; listen_address=%a; public_address=%a}"
    backlog
    (Auth.Secret_key.pp_fingerprint `SHA256) (Lazy.force secret_key)
    serve_tls
    Network.Socket_address.pp listen_address
    Network.Socket_address.pp public_address

let equal {backlog; secret_key; serve_tls; listen_address; public_address} b =
  backlog = b.backlog &&
  serve_tls = serve_tls &&
  Network.Socket_address.equal listen_address b.listen_address &&
  Network.Socket_address.equal public_address b.public_address &&
  Auth.Secret_key.equal (Lazy.force secret_key) (Lazy.force b.secret_key)

let public_address =
  let i = Arg.info ["public-address"] ~docv:"ADDR" ~doc:"Address to tell others to connect on." in
  Arg.(value @@ opt (some Listen_address.addr_conv) None i)

let disable_tls =
  let i = Arg.info ["disable-tls"] ~doc:"Do not use TLS for incoming connections." in
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
  Term.(pure make $ secret_key_term $ disable_tls $ Listen_address.cmd $ public_address)
