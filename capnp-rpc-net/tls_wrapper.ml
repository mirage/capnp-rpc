module Log = Capnp_rpc.Debug.Log

open Auth

let error fmt =
  fmt |> Fmt.kstr @@ fun msg ->
  Error (`Msg msg)

let plain_endpoint ~sw flow =
  Endpoint.of_flow ~sw ~peer_id:Auth.Digest.insecure flow

let connect_as_server ~sw flow secret_key =
  match secret_key with
  | None -> Ok (plain_endpoint ~sw flow)
  | Some key ->
    Log.info (fun f -> f "Doing TLS server-side handshake...");
    let tls_config = Secret_key.tls_server_config key in
    match Tls_eio.server_of_flow tls_config flow with
    | exception (Failure msg) -> error "TLS connection failed: %s" msg
    | exception ex -> error "TLS connection failed: %a" Fmt.exn ex
    | flow ->
      match Tls_eio.epoch flow with
      | Error () -> failwith "Unknown error getting TLS epoch data"
      | Ok data ->
        match data.Tls.Core.peer_certificate with
        | None -> error "No client certificate found"
        | Some client_cert ->
          let peer_id = Digest.of_certificate client_cert in
          Ok (Endpoint.of_flow ~sw ~peer_id flow)

let connect_as_client ~sw flow secret_key auth =
  match Digest.authenticator auth with
  | None -> Ok (plain_endpoint ~sw flow)
  | Some authenticator ->
    let tls_config = Secret_key.tls_client_config ~authenticator (Lazy.force secret_key) in
    Log.info (fun f -> f "Doing TLS client-side handshake...");
    match Tls_eio.client_of_flow tls_config flow with
    | exception (Failure msg) -> error "TLS connection failed: %s" msg
    | exception ex -> error "TLS connection failed: %a" Fmt.exn ex
    | flow -> Ok (Endpoint.of_flow ~sw ~peer_id:auth flow)
