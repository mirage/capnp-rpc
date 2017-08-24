module Log = Capnp_rpc.Debug.Log
module Tls_wrapper = Capnp_rpc_lwt.Auth.Tls_wrapper(Unix_flow)

module Types = struct
  type provision_id
  type recipient_id
  type third_party_cap_id = [`Two_party_only]
  type join_key_part
end

let parse_third_party_cap_id _ = `Two_party_only

let error fmt =
  fmt |> Fmt.kstrf @@ fun msg ->
  Error (`Msg msg)

let none_if_empty = function
  | None | Some "" -> None
  | Some _ as x -> x

module Socket_address = struct
  type t = [
    | `Unix of string
    | `TCP of string * int
  ]

  let pp f = function
    | `Unix path -> Fmt.pf f "unix:%s" path
    | `TCP (host, port) -> Fmt.pf f "tcp:%s:%d" host port

  let equal = ( = )
end

module Address = struct
  type t = Socket_address.t * Capnp_rpc_lwt.Auth.Digest.t

  let to_uri (addr, auth) =
    let uri =
      match addr with
      | `Unix path -> Uri.make ~scheme:"capnp" ~path ()
      | `TCP (host, port) -> Uri.make ~scheme:"capnp" ~host ~port ()
    in
    Capnp_rpc_lwt.Auth.Digest.add_to_uri auth uri

  let pp f (addr, auth) =
    Fmt.pf f "%a@%a" Capnp_rpc_lwt.Auth.Digest.pp auth Socket_address.pp addr

  let ( >>= ) x f =
    match x with
    | Error _ as e -> e
    | Ok y -> f y

  let parse_uri uri =
    let host = Uri.host uri |> none_if_empty in
    let port = Uri.port uri in
    let path = Uri.path uri in
    Capnp_rpc_lwt.Auth.Digest.from_uri uri >>= fun auth ->
    match host, port with
    | Some host, Some port when path = "" -> Ok (`TCP (host, port), auth)
    | Some _,    Some _ -> error "Unexpected path component %S in %a" path Uri.pp_hum uri
    | Some _,    None   -> error "Missing port in %a" Uri.pp_hum uri
    | None,      Some _ -> error "Port without host in %a!" Uri.pp_hum uri
    | None,      None   -> Ok (`Unix path, auth)

  let equal (addr, auth) (addr_b, auth_b) =
    Socket_address.equal addr addr_b &&
    Capnp_rpc_lwt.Auth.Digest.equal auth auth_b
end

let addr_of_host host =
  match Unix.gethostbyname host with
  | exception Not_found ->
    Capnp_rpc.Debug.failf "Unknown host %S" host
  | addr ->
    if Array.length addr.Unix.h_addr_list = 0 then
      Capnp_rpc.Debug.failf "No addresses found for host name %S" host
    else
      addr.Unix.h_addr_list.(0)

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

let connect (addr, auth) =
  match connect_socket addr with
  | exception ex ->
    Lwt.return @@ error "@[<v2>Network connection for %a failed:@,%a@]" Socket_address.pp addr Fmt.exn ex
  | socket ->
    let switch = Lwt_switch.create () in
    let flow = Unix_flow.connect ~switch (Lwt_unix.of_unix_file_descr socket) in
    Tls_wrapper.connect_as_client ~switch flow auth

let accept_connection ~switch ~secret_key flow =
  Tls_wrapper.connect_as_server ~switch flow secret_key
