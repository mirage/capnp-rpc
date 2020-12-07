open Lwt.Infix

module Log = Capnp_rpc.Debug.Log
module Tls_wrapper = Capnp_rpc_net.Tls_wrapper.Make(Unix_flow)

module Location = struct
  open Astring

  include Capnp_rpc_net.Capnp_address.Location

  let abs_path p =
    if Filename.is_relative p then
      Filename.concat (Sys.getcwd ()) p
    else p

  let validate_public = function
    | `Unix path -> if Filename.is_relative path then Fmt.failwith "Path %S is relative!" path
    | `TCP _ -> ()

  let unix x = `Unix (abs_path x)
  let tcp ~host ~port = `TCP (host, port)

  let parse_tcp s =
    match String.cut ~sep:":" s with
    | None -> Error (`Msg "Missing :PORT in listen address")
    | Some (host, port) ->
      match String.to_int port with
      | None -> Error (`Msg "PORT must be an integer")
      | Some port ->
        Ok (tcp ~host ~port)

  let of_string s =
    match String.cut ~sep:":" s with
    | Some ("unix", path) -> Ok (unix path)
    | Some ("tcp", tcp) -> parse_tcp tcp
    | None -> Error (`Msg "Missing ':'")
    | Some _ -> Error (`Msg "Only tcp:HOST:PORT and unix:PATH addresses are currently supported")

  let cmdliner_conv = Cmdliner.Arg.conv (of_string, pp)
end

module Address
  : Capnp_rpc_net.S.ADDRESS with type t = Location.t * Capnp_rpc_net.Auth.Digest.t
  = Capnp_rpc_net.Capnp_address

module Types = struct
  type provision_id
  type recipient_id
  type third_party_cap_id = [`Two_party_only]
  type join_key_part
end

type t = unit

let error fmt =
  fmt |> Fmt.kstrf @@ fun msg ->
  Error (`Msg msg)

let parse_third_party_cap_id _ = `Two_party_only

let addr_of_host host =
  match Unix.gethostbyname host with
  | exception Not_found ->
    Capnp_rpc.Debug.failf "Unknown host %S" host
  | addr ->
    if Array.length addr.Unix.h_addr_list = 0 then
      Capnp_rpc.Debug.failf "No addresses found for host name %S" host
    else
      addr.Unix.h_addr_list.(0)

let have_tcp_keepidle =
  try ExtUnix.All.have_sockopt_int ExtUnix.All.TCP_KEEPIDLE
  with ExtUnix.All.Not_available _ -> false

let try_set_keepalive_idle socket i =
  if have_tcp_keepidle then (
    let socket = Lwt_unix.unix_file_descr socket in
    ExtUnix.All.setsockopt_int socket ExtUnix.All.TCP_KEEPIDLE i
  )

let connect_socket = function
  | `Unix path ->
    Logs.info (fun f -> f "Connecting to %S..." path);
    let socket = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
    Lwt.catch
      (fun () -> Lwt_unix.connect socket (Unix.ADDR_UNIX path) >|= fun () -> socket)
      (fun ex -> Lwt_unix.close socket >>= fun () -> Lwt.fail ex)
  | `TCP (host, port) ->
    Logs.info (fun f -> f "Connecting to %s:%d..." host port);
    let socket = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
    Lwt.catch
      (fun () ->
         Lwt_unix.setsockopt socket Unix.SO_KEEPALIVE true;
         try_set_keepalive_idle socket 60;
         Lwt_unix.connect socket (Unix.ADDR_INET (addr_of_host host, port)) >|= fun () ->
         socket
      )
      (fun ex -> Lwt_unix.close socket >>= fun () -> Lwt.fail ex)

let connect () ~switch ~secret_key (addr, auth) =
  Lwt.try_bind
    (fun () -> connect_socket addr)
    (fun socket ->
       let flow = Unix_flow.connect ~switch socket in
       Tls_wrapper.connect_as_client ~switch flow secret_key auth
    )
    (fun ex ->
       Lwt.return @@ error "@[<v2>Network connection for %a failed:@,%a@]" Location.pp addr Fmt.exn ex
    )

let accept_connection ~switch ~secret_key flow =
  Tls_wrapper.connect_as_server ~switch flow secret_key
