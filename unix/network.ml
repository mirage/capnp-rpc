open Eio.Std

module Log = Capnp_rpc.Debug.Log
module Tls_wrapper = Capnp_rpc_net.Tls_wrapper

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

type t = [`Generic] Eio.Net.ty r

let error fmt =
  fmt |> Fmt.kstr @@ fun msg ->
  Error (`Msg msg)

let parse_third_party_cap_id _ = `Two_party_only

let try_set_nodelay socket =
  try Unix.setsockopt socket Unix.TCP_NODELAY true
  with Unix.Unix_error (EOPNOTSUPP, _, _) -> ()         (* Probably a Unix-domain socket *)

let connect net ~sw ~secret_key (addr, auth) =
  let eio_addr =
    match addr with
    | `Unix _ as x -> x
    | `TCP (host, port) ->
      match Eio.Net.getaddrinfo_stream net host ~service:(string_of_int port) with
      | [] -> Capnp_rpc.Debug.failf "No addresses found for host name %S" host
      | addr :: _ -> addr
  in
  Log.info (fun f -> f "Connecting to %a..." Eio.Net.Sockaddr.pp eio_addr);
  match Eio.Net.connect ~sw net eio_addr with
  | socket ->
    begin match addr with
      | `Unix _ -> ()
      | `TCP _ ->
        let socket = Eio_unix.Resource.fd_opt socket |> Option.get in
        Eio_unix.Fd.use_exn "keep-alive" socket @@ fun socket ->
        Unix.setsockopt socket Unix.SO_KEEPALIVE true;
        try_set_nodelay socket;
        Keepalive.try_set_idle socket 60
    end;
    Tls_wrapper.connect_as_client socket secret_key auth
  | exception ex ->
    Fiber.check ();
    error "@[<v2>Network connection for %a failed:@,%a@]" Location.pp addr Fmt.exn ex

let accept_connection ~secret_key flow =
  Eio_unix.Resource.fd_opt flow
  |> Option.iter (fun fd -> Eio_unix.Fd.use_exn "TCP_NODELAY" fd try_set_nodelay);
  Tls_wrapper.connect_as_server flow secret_key

let v t = (t :> [`Generic] Eio.Net.ty r)
