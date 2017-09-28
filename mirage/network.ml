open Lwt.Infix
module Log = Capnp_rpc.Debug.Log

let error fmt =
  fmt |> Fmt.kstrf @@ fun msg ->
  Error (`Msg msg)

module Location = struct
  type t = [
    | `TCP of string * int
  ]

  let tcp ~host ~port = `TCP (host, port)

  let pp f x = Capnp_rpc_lwt.Capnp_address.(Location.pp f (x :> Location.t))

  let equal = ( = )
end

module Make (Stack : Mirage_stack_lwt.V4) (Dns : Dns_resolver_mirage.S) = struct
  module Tls_wrapper = Capnp_rpc_lwt.Tls_wrapper.Make(Stack.TCPV4)

  module Address = struct
    module Full = Capnp_rpc_lwt.Capnp_address

    type t = Location.t * Capnp_rpc_lwt.Auth.Digest.t

    let digest t = Full.digest (t :> Full.t)
    let to_uri (t, id) = Full.to_uri ((t :> Full.t), id)
    let pp f t = Full.pp f (t :> Full.t)

    let parse_uri uri =
      match Full.parse_uri uri with
      | Error _ as e -> e
      | Ok ((`TCP _, _), _ as x) -> Ok x
      | Ok ((`Unix x, _), _) ->
        error "Unix-domain addresses are not available with Mirage networking (%S)" x

    let equal (addr, auth) (addr_b, auth_b) =
      Location.equal addr addr_b &&
      Capnp_rpc_lwt.Auth.Digest.equal auth auth_b
  end

  module Types = struct
    type provision_id
    type recipient_id
    type third_party_cap_id = [`Two_party_only]
    type join_key_part
  end

  let parse_third_party_cap_id _ = `Two_party_only

  type t = {
    stack : Stack.t;
    dns : Dns.t;
  }

  let addr_of_host dns host =
    Dns.gethostbyname dns host >|= function
    | [] -> error "Unknown host %S" host
    | addrs ->
      let rec loop = function
        | [] -> error "No IPv4 addresses found for %S" host
        | Ipaddr.V4 x :: _ -> Ok x
        | Ipaddr.V6 _ :: xs -> loop xs
      in
      loop addrs

  let ( >>*= ) x f =
    x >>= function
    | Error _ as e -> Lwt.return e
    | Ok y -> f y

  let connect t ~switch ~secret_key (addr, auth) =
    match addr with
    | `TCP (host, port) ->
      Logs.info (fun f -> f "Connecting to %s:%d..." host port);
      addr_of_host t.dns host >>*= fun addr ->
      let tcp = Stack.tcpv4 t.stack in
      Stack.TCPV4.create_connection tcp (addr, port) >>= function
      | Error e -> Lwt.return @@ error "Failed to connect to %S:%d: %a" host port Stack.TCPV4.pp_error e
      | Ok flow -> Tls_wrapper.connect_as_client ~switch flow secret_key auth

  let accept_connection ~switch ~secret_key flow =
    Tls_wrapper.connect_as_server ~switch flow secret_key
end
