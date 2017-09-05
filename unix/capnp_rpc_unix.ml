open Lwt.Infix

module Log = Capnp_rpc.Debug.Log
module Unix_flow = Unix_flow

let () = Nocrypto_entropy_unix.initialize ()

type flow = Unix_flow.flow
type 'a capability = 'a Capnp_rpc_lwt.Capability.t
type restorer = Capnp_rpc_lwt.Restorer.t

module CapTP = Vat_network.CapTP
module Vat = Vat_network.Vat
module Network = Network
module Vat_config = Vat_config
module File_store = File_store

type service_id = Capnp_rpc_lwt.Restorer.Id.t
type 'a sturdy_ref = 'a Capnp_rpc_lwt.Sturdy_ref.t

let error fmt =
  fmt |> Fmt.kstrf @@ fun msg ->
  Error (`Msg msg)

let sturdy_uri =
  let of_string s =
    match Uri.of_string s with
    | exception ex -> error "Failed to parse URI %S: %a" s Fmt.exn ex
    | uri -> Ok uri
  in
  Cmdliner.Arg.conv (of_string, Uri.pp_hum)

let handle_connection ?tags ~secret_key vat client =
  let switch = Lwt_switch.create () in
  let raw_flow = Unix_flow.connect ~switch client in
  Network.accept_connection ~switch ~secret_key raw_flow >>= function
  | Error (`Msg msg) ->
    Log.warn (fun f -> f ?tags "Rejecting new connection: %s" msg);
    Lwt.return_unit
  | Ok ep ->
    Vat.add_connection vat ~switch ~mode:`Accept ep >|= fun (_ : CapTP.t) ->
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

let serve ?switch ?tags ?restore config =
  let {Vat_config.backlog; secret_key = _; serve_tls; listen_address; public_address} = config in
  let vat =
    let auth = Vat_config.auth config in
    let secret_key = lazy (fst (Lazy.force config.secret_key)) in
    Vat.create ?switch ?tags ?restore ~address:(public_address, auth) ~secret_key ()
  in
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
  Logs.info (fun f -> f ?tags "Waiting for %s connections on %a"
                (if serve_tls then "(encrypted)" else "UNENCRYPTED")
                Vat_config.Listen_address.pp listen_address);
  let lwt_socket = Lwt_unix.of_unix_file_descr socket in
  let rec loop () =
    Lwt_switch.check switch;
    Lwt_unix.accept lwt_socket >>= fun (client, _addr) ->
    Logs.info (fun f -> f ?tags "Accepting new connection");
    let secret_key = if serve_tls then Some (Vat_config.secret_key config) else None in
    Lwt.async (fun () -> handle_connection ?tags ~secret_key vat client);
    loop ()
  in
  Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
           let th = loop () in
           Lwt_switch.add_hook switch (fun () -> Lwt.cancel th; Lwt.return_unit);
           th
        )
        (function
          | Lwt.Canceled -> Lwt.return_unit
          | ex -> Lwt.fail ex
        )
    );
  Lwt.return vat

let client_only_vat ?switch ?tags ?restore () =
  let secret_key = lazy (Capnp_rpc_lwt.Auth.Secret_key.generate ()) in
  Vat.create ?switch ?tags ?restore ~secret_key ()
