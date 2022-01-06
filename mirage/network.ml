open Lwt.Infix
module Log = Capnp_rpc.Debug.Log

let error fmt =
  fmt |> Fmt.kstr @@ fun msg ->
  Error (`Msg msg)

module Location = struct
  type t = [
    | `TCP of string * int
  ]

  let tcp ~host ~port = `TCP (host, port)

  let pp f x = Capnp_rpc_net.Capnp_address.(Location.pp f (x :> Location.t))

  let equal = ( = )
end

module Make  (R : Mirage_random.S) (T : Mirage_time.S) (M : Mirage_clock.MCLOCK) (P : Mirage_clock.PCLOCK) (Stack : Tcpip.Stack.V4V6) = struct

  module Dns = Dns_client_mirage.Make(R)(T)(M)(P)(Stack)
  module Tls_wrapper = Capnp_rpc_net.Tls_wrapper.Make(Stack.TCP)

  module Address = struct
    module Full = Capnp_rpc_net.Capnp_address

    type t = Location.t * Capnp_rpc_net.Auth.Digest.t

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
      Capnp_rpc_net.Auth.Digest.equal auth auth_b
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
    match Ipaddr.of_string host with
    | Ok ip -> Lwt.return @@ Ok ip
    | Error (`Msg _) ->
      match Domain_name.of_string host with
      | Ok dn -> begin
        match Domain_name.host dn with
        | Ok h -> begin
           Dns.gethostbyname dns h >>= function
           | Ok addr -> Lwt.return_ok (Ipaddr.V4 addr)
           | Error (`Msg error_msg) -> Lwt.return @@ error "Unknown host %S : %s" host error_msg
         end
        | Error (`Msg error_msg) -> Lwt.return @@ error "Invalid hostname %S : %s" host error_msg
      end
      | Error (`Msg error_msg) -> Lwt.return @@ error "Bad domain name %S : %s" host error_msg

  let ( >>*= ) x f =
    x >>= function
    | Error _ as e -> Lwt.return e
    | Ok y -> f y

  let connect t ~switch ~secret_key (addr, auth) =
    match addr with
    | `TCP (host, port) ->
      Logs.info (fun f -> f "Connecting to %s:%d..." host port);
      addr_of_host t.dns host >>*= fun addr ->
      let tcp = Stack.tcp t.stack in
      Stack.TCP.create_connection tcp (addr, port) >>= function
      | Error e -> Lwt.return @@ error "Failed to connect to %S:%d: %a" host port Stack.TCP.pp_error e
      | Ok flow -> Tls_wrapper.connect_as_client ~switch flow secret_key auth

  let accept_connection ~switch ~secret_key flow =
    Tls_wrapper.connect_as_server ~switch flow secret_key
end
