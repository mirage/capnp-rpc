open Capnp_core
open Lwt.Infix

module Log = Capnp_rpc.Debug.Log

let error fmt =
  fmt |> Fmt.kstrf @@ fun msg ->
  Error (`Msg msg)

module Make (Network : S.NETWORK) (Underlying : Mirage_flow_lwt.S) = struct
  module Flow = Tls_mirage.Make(Underlying)

  module Sturdy_ref = Sturdy_ref.Make (Network)
  module CapTP = CapTP_capnp.Make (Network)

  type t = {
    secret_key : Auth.Secret_key.t option;
    switch : Lwt_switch.t option;
    address : Network.Address.t option;
    mutable bootstrap : Core_types.cap option;
    mutable connections : CapTP.t list; (* todo: should be a map, once we have Vat IDs *)
  }

  let create ?switch ?secret_key ?bootstrap ?address () =
    let t = {
      secret_key;
      switch;
      address;
      bootstrap;
      connections = [];
    } in
    Lwt_switch.add_hook switch (fun () ->
        begin match t.bootstrap with
          | Some x -> Core_types.dec_ref x; t.bootstrap <- None
          | None -> ()
        end;
        let ex = Capnp_rpc.Exception.v ~ty:`Disconnected "Vat shut down" in
        Lwt_list.iter_p (fun c -> CapTP.disconnect c ex) t.connections >|= fun () ->
        t.connections <- []
      );
    t

  let connect t endpoint =
    let switch = Lwt_switch.create () in
    Lwt_switch.add_hook t.switch (fun () -> Lwt_switch.turn_off switch);
    let conn = CapTP.connect ~switch ?offer:t.bootstrap endpoint in
    t.connections <- conn :: t.connections;
    conn

  let public_fingerprint t =
    match t.secret_key with
    | None -> Auth.Digest.insecure
    | Some key -> Auth.Secret_key.digest key

  let bootstrap_ref t =
    match t.address with
    | None -> failwith "bootstrap_ref: vat was not configured with an address"
    | Some address ->
      let auth = public_fingerprint t in
      Sturdy_ref.v ~auth ~address ~service:`Bootstrap

  let pp_bootstrap_uri f t =
    if t.bootstrap = None then Fmt.string f "(vat has no bootstrap service)"
    else if t.address = None then Fmt.string f "(vat has no public address)"
    else Sturdy_ref.pp_with_secrets f (bootstrap_ref t)

  let plain_endpoint ~switch flow =
    Endpoint.of_flow ~switch (module Underlying) flow

  let connect_as_server ~switch t flow =
    match t.secret_key with
    | None -> Lwt.return @@ Ok (connect t @@ plain_endpoint ~switch flow)
    | Some key ->
      Log.info (fun f -> f "Doing TLS server-side handshake...");
      Flow.server_of_flow (Auth.Secret_key.tls_config key) flow >|= function
      | Error e -> error "TLS connection failed: %a" Flow.pp_write_error e
      | Ok flow -> Ok (connect t @@ Endpoint.of_flow ~switch (module Flow) flow)

  let connect_as_client ~switch t auth flow =
    match Auth.Digest.authenticator auth with
    | None -> Lwt.return @@ Ok (connect t @@ plain_endpoint ~switch flow)
    | Some authenticator ->
      let tls_config = Tls.Config.client ~authenticator () in
      Log.info (fun f -> f "Doing TLS client-side handshake...");
      Flow.client_of_flow tls_config flow >|= function
      | Error e -> error "TLS connection failed: %a" Flow.pp_write_error e
      | Ok flow -> Ok (connect t @@ Endpoint.of_flow ~switch (module Flow) flow)
end
