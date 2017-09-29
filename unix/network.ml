open Astring
module Log = Capnp_rpc.Debug.Log
module Tls_wrapper = Capnp_rpc_lwt.Tls_wrapper.Make(Unix_flow)

module Types = struct
  type provision_id
  type recipient_id
  type third_party_cap_id = [`Two_party_only]
  type join_key_part
end

type t = unit

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

  let abs_path p =
    if Filename.is_relative p then
      Filename.concat (Sys.getcwd ()) p
    else p

  let unix x = `Unix (abs_path x)
  let tcp ~host ~port = `TCP (host, port)

  let pp f = function
    | `Unix path -> Fmt.pf f "unix:%s" path
    | `TCP (host, port) -> Fmt.pf f "tcp:%s:%d" host port

  let validate_public = function
    | `Unix path -> if Filename.is_relative path then Fmt.failwith "Path %S is relative!" path
    | `TCP _ -> ()

  let equal = ( = )
end

module Address = struct
  type t = Socket_address.t * Capnp_rpc_lwt.Auth.Digest.t

  let digest = snd

  let alphabet = B64.uri_safe_alphabet

  let b64encode s = B64.encode ~alphabet ~pad:false s

  let b64decode s =
    try Ok (B64.decode ~alphabet s)
    with ex -> error "Bad base64 digest %S: %a" s Fmt.exn ex

  let to_uri ((addr, auth), service_id) =
    let service_id = b64encode service_id in
    let uri =
      match addr with
      | `Unix path ->
        let path = Printf.sprintf "%s/%s" path service_id in
        Uri.make ~scheme:"capnp" ~path ()
      | `TCP (host, port) ->
        Uri.make ~scheme:"capnp" ~host ~port ~path:service_id ()
    in
    Capnp_rpc_lwt.Auth.Digest.add_to_uri auth uri

  let pp f t =
    Uri.pp_hum f (to_uri (t, ""))

  let ( >>= ) x f =
    match x with
    | Error _ as e -> e
    | Ok y -> f y

  let strip_leading_slash s =
    if String.is_prefix ~affix:"/" s then String.with_range ~first:1 s
    else s

  let check_sheme uri =
    match Uri.scheme uri with
    | Some "capnp" -> Ok ()
    | Some scheme -> error "Unknown scheme %S (expected 'capnp://...')" scheme
    | None -> error "Missing scheme in %a (expected 'capnp://...')" Uri.pp_hum uri

  let parse_uri uri =
    check_sheme uri >>= fun () ->
    let host = Uri.host uri |> none_if_empty in
    let port = Uri.port uri in
    let path = Uri.path uri in
    Capnp_rpc_lwt.Auth.Digest.from_uri uri >>= fun auth ->
    match host, port with
    | Some host, Some port ->
      b64decode (strip_leading_slash path) >>= fun service_id ->
      Ok ((`TCP (host, port), auth), service_id)
    | Some _,    None   -> error "Missing port in %a" Uri.pp_hum uri
    | None,      Some _ -> error "Port without host in %a!" Uri.pp_hum uri
    | None,      None   ->
      match String.cut ~rev:true ~sep:"/" path with
      | None -> Ok ((`Unix path, auth), "")
      | Some (path, service_id) ->
        b64decode service_id >>= fun service_id ->
        Ok ((`Unix path, auth), service_id)

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

let connect () ~switch ~secret_key (addr, auth) =
  match connect_socket addr with
  | exception ex ->
    Lwt.return @@ error "@[<v2>Network connection for %a failed:@,%a@]" Socket_address.pp addr Fmt.exn ex
  | socket ->
    let flow = Unix_flow.connect ~switch (Lwt_unix.of_unix_file_descr socket) in
    Tls_wrapper.connect_as_client ~switch flow secret_key auth

let accept_connection ~switch ~secret_key flow =
  Tls_wrapper.connect_as_server ~switch flow secret_key
