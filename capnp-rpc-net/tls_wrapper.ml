module Log = Capnp_rpc.Debug.Log

open Lwt.Infix
open Auth

let error fmt =
  fmt |> Fmt.kstrf @@ fun msg ->
  Error (`Msg msg)

module Make (Underlying : Mirage_flow.S) = struct
  module Flow = struct
    include Tls_mirage.Make(Underlying)

    let read flow =
      read flow >|= function
      | Error (`Write `Closed) -> Ok `Eof (* This can happen, despite being a write error on a read! *)
      | x -> x

    let writev flow bufs =
      writev flow bufs >|= function
      | Error (`Write `Closed) -> Error `Closed
      | x -> x

    let write flow buf = writev flow [buf]
  end

  let plain_endpoint ~switch flow =
    Endpoint.of_flow ~switch ~peer_id:Auth.Digest.insecure (module Underlying) flow

  let connect_as_server ~switch flow secret_key =
    match secret_key with
    | None -> Lwt.return @@ Ok (plain_endpoint ~switch flow)
    | Some key ->
      Log.info (fun f -> f "Doing TLS server-side handshake...");
      let tls_config = Secret_key.tls_server_config key in
      Flow.server_of_flow tls_config flow >|= function
      | Error e -> error "TLS connection failed: %a" Flow.pp_write_error e
      | Ok flow ->
        match Flow.epoch flow with
        | Error () -> failwith "Unknown error getting TLS epoch data"
        | Ok data ->
          match data.Tls.Core.peer_certificate with
          | None -> error "No client certificate found"
          | Some client_cert ->
            let peer_id = Digest.of_certificate client_cert in
            Ok (Endpoint.of_flow ~switch ~peer_id (module Flow) flow)

  let connect_as_client ~switch flow secret_key auth =
    match Digest.authenticator auth with
    | None -> Lwt.return @@ Ok (plain_endpoint ~switch flow)
    | Some authenticator ->
      let tls_config = Secret_key.tls_client_config ~authenticator (Lazy.force secret_key) in
      Log.info (fun f -> f "Doing TLS client-side handshake...");
      Flow.client_of_flow tls_config flow >|= function
      | Error e -> error "TLS connection failed: %a" Flow.pp_write_error e
      | Ok flow -> Ok (Endpoint.of_flow ~switch ~peer_id:auth (module Flow) flow)
end
