open Lwt.Infix

module Log = Capnp_rpc.Debug.Log
module Unix_flow = Unix_flow

let () = Nocrypto_entropy_lwt.initialize () |> ignore

type flow = Unix_flow.flow
type 'a capability = 'a Capnp_rpc_lwt.Capability.t

module Vat_network = Capnp_rpc_lwt.Networking (Network) (Unix_flow)
module CapTP = Vat_network.CapTP
module Vat = Vat_network.Vat
module Sturdy_ref = Vat_network.Sturdy_ref
module Network = Network
module Vat_config = Vat_config

let error fmt =
  fmt |> Fmt.kstrf @@ fun msg ->
  Error (`Msg msg)

let sturdy_ref () =
  let of_string s =
    match Uri.of_string s with
    | exception ex -> error "Failed to parse URI %S: %a" s Fmt.exn ex
    | uri -> Sturdy_ref.of_uri uri
  in
  Cmdliner.Arg.conv (of_string, Sturdy_ref.pp_with_secrets)

let handle_connection ~secret_key vat client =
  let switch = Lwt_switch.create () in
  let raw_flow = Unix_flow.connect ~switch client in
  Network.accept_connection ~switch ~secret_key raw_flow >|= function
  | Error (`Msg msg) -> Log.warn (fun f -> f "Rejecting new connection: %s" msg)
  | Ok ep ->
    let _ : CapTP.t = Vat.add_connection vat ep in
    ()

let addr_of_host host =
  match Unix.gethostbyname host with
  | exception Not_found ->
    Capnp_rpc.Debug.failf "Unknown host %S" host
  | addr ->
    if Array.length addr.Unix.h_addr_list = 0 then
      Capnp_rpc.Debug.failf "No addresses found for host name %S" host
    else
      addr.Unix.h_addr_list.(0)

let serve ?offer {Vat_config.backlog; secret_key; serve_tls; listen_address; public_address} =
  let auth =
    if serve_tls then Capnp_rpc_lwt.Auth.Secret_key.digest (Lazy.force secret_key)
    else Capnp_rpc_lwt.Auth.Digest.insecure
  in
  let secret_key =
    if serve_tls then Some (Lazy.force secret_key)
    else None
  in
  let vat = Vat.create ?bootstrap:offer ~address:(public_address, auth) () in
  let socket =
    match listen_address with
    | `Unix path ->
      begin match Unix.lstat path with
        | { Unix.st_kind = Unix.S_SOCK; _ } -> Unix.unlink path
        | _ -> ()
        | exception Unix.Unix_error(Unix.ENOENT, _, _) -> ()
      end;
      let socket = Unix.(socket PF_UNIX SOCK_STREAM 0) in
      Unix.bind socket (Unix.ADDR_UNIX path);
      socket
    | `TCP (host, port) ->
      let socket = Unix.(socket PF_INET SOCK_STREAM 0) in
      Unix.setsockopt socket Unix.SO_REUSEADDR true;
      Unix.bind socket (Unix.ADDR_INET (addr_of_host host, port));
      socket
  in
  Unix.listen socket backlog;
  let pp_auth f = function
    | Some _ -> Fmt.string f "(encrypted)"
    | None -> Fmt.string f "UNENCRYPTED"
  in
  Logs.info (fun f -> f "Waiting for %a connections on %a"
                pp_auth secret_key
                Vat_config.Listen_address.pp listen_address);
  let lwt_socket = Lwt_unix.of_unix_file_descr socket in
  let rec loop () =
    Lwt_unix.accept lwt_socket >>= fun (client, _addr) ->
    Logs.info (fun f -> f "Accepting new connection");
    Lwt.async (fun () -> handle_connection ~secret_key vat client);
    loop ()
  in
  Lwt.async loop;
  Lwt.return vat

let client_only_vat ?switch ?offer () =
  Vat.create ?switch ?bootstrap:offer ()
