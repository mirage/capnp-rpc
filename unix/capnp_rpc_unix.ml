open Lwt.Infix

module Log = Capnp_rpc.Debug.Log
module Unix_flow = Unix_flow

let () = Nocrypto_entropy_lwt.initialize () |> ignore

include Capnp_rpc_lwt.Networking (Network) (Unix_flow)
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

let handle_connection vat client =
  let switch = Lwt_switch.create () in
  let raw_flow = Unix_flow.connect ~switch client in
  Vat.connect_as_server ~switch vat raw_flow >>= function
  | Error (`Msg msg) ->
    Log.warn (fun f -> f "Rejecting new connection: %s" msg);
    Lwt.return_unit
  | Ok (_ : CapTP.t) ->
    Lwt.return_unit

let addr_of_host host =
  match Unix.gethostbyname host with
  | exception Not_found ->
    Capnp_rpc.Debug.failf "Unknown host %S" host
  | addr ->
    if Array.length addr.Unix.h_addr_list = 0 then
      Capnp_rpc.Debug.failf "No addresses found for host name %S" host
    else
      addr.Unix.h_addr_list.(0)

let serve ?offer {Vat_config.backlog; secret_key; listen_address; public_address} =
  let vat = Vat.create ?bootstrap:offer ?secret_key ~address:public_address () in
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
    Lwt.async (fun () -> handle_connection vat client);
    loop ()
  in
  Lwt.async loop;
  Lwt.return vat

let connect_socket = function
  | `Unix path ->
    Logs.info (fun f -> f "Connecting to %S..." path);
    let socket = Unix.(socket PF_UNIX SOCK_STREAM 0) in
    Unix.connect socket (Unix.ADDR_UNIX path);
    socket
  | `TCP (host, port) ->
    Logs.info (fun f -> f "Connecting to %s:%d..." host port);
    let socket = Unix.(socket PF_INET SOCK_STREAM 0) in
    Unix.connect socket (Unix.ADDR_INET (addr_of_host host, port));
    socket

let connect ?switch ?offer sr =
  let switch =
    match switch with
    | None -> Lwt_switch.create ()
    | Some x -> x
  in
  match connect_socket (Sturdy_ref.address sr) with
  | exception ex ->
    Capnp_rpc.Debug.failf "@[<v2>Network connection for %a failed:@,%a@]"
      Sturdy_ref.pp_address sr
      Fmt.exn ex
  | socket ->
    let vat = Vat.create ~switch ?bootstrap:offer () in
    let auth = Sturdy_ref.auth sr in
    let raw_flow = Unix_flow.connect ~switch (Lwt_unix.of_unix_file_descr socket) in
    Vat.connect_as_client ~switch vat auth raw_flow >|= function
    | Error (`Msg msg) ->
      Capnp_rpc.Debug.failf "Connection to %a failed: %s" Sturdy_ref.pp_address sr msg
    | Ok conn ->
      CapTP.bootstrap conn
