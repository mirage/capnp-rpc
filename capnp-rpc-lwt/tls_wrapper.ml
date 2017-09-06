module Log = Capnp_rpc.Debug.Log

open Lwt.Infix
open Auth

let error fmt =
  fmt |> Fmt.kstrf @@ fun msg ->
  Error (`Msg msg)

module Make (Underlying : Mirage_flow_lwt.S) = struct
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
      let certificates = Secret_key.certificates key in
      let client_cert = ref None in
      let authenticator ?host:_ = function
        | [] -> `Fail `EmptyCertificateChain
        | client :: _ ->  (* First certificate is the client's own *)
          client_cert := Some client;
          `Ok None
      in
      let tls_config = Tls.Config.server ~certificates ~authenticator () in
      Flow.server_of_flow tls_config flow >|= function
      | Error e -> error "TLS connection failed: %a" Flow.pp_write_error e
      | Ok flow ->
        match !client_cert with
        | None -> failwith "No client certificate after auth!"
        | Some client_cert ->
          let peer_id = Digest.of_certificate client_cert in
          Ok (Endpoint.of_flow ~switch ~peer_id (module Flow) flow)

  let connect_as_client ~switch flow secret_key auth =
    match Digest.authenticator auth with
    | None -> Lwt.return @@ Ok (plain_endpoint ~switch flow)
    | Some authenticator ->
      let certificates = Secret_key.certificates (Lazy.force secret_key) in
      let tls_config = Tls.Config.client ~certificates ~authenticator () in
      Log.info (fun f -> f "Doing TLS client-side handshake...");
      Flow.client_of_flow tls_config flow >|= function
      | Error e -> error "TLS connection failed: %a" Flow.pp_write_error e
      | Ok flow -> Ok (Endpoint.of_flow ~switch ~peer_id:auth (module Flow) flow)
end
