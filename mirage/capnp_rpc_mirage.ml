open Lwt.Infix

module Log = Capnp_rpc.Debug.Log

module Location = Network.Location

module Make (R : Mirage_random.S) (T : Mirage_time.S) (C : Mirage_clock.MCLOCK) (Stack : Mirage_stack.V4) = struct

  module Dns = Dns_client_mirage.Make(R)(T)(C)(Stack)
  module Network = Network.Make(R)(T)(C)(Stack)
  module Vat_config = Vat_config.Make(Network)
  module Vat_network = Capnp_rpc_net.Networking(Network)(Stack.TCPV4)

  type flow = Stack.TCPV4.flow

  module CapTP = Vat_network.CapTP
  module Vat = Vat_network.Vat

  let network ~dns stack = {Network.dns; stack}

  let handle_connection ?tags ~secret_key vat flow =
    let switch = Lwt_switch.create () in
    Network.accept_connection ~switch ~secret_key flow >>= function
    | Error (`Msg msg) ->
      Log.warn (fun f -> f ?tags "Rejecting new connection: %s" msg);
      Lwt.return_unit
    | Ok ep ->
      Vat.add_connection vat ~switch ~mode:`Accept ep >|= fun (_ : CapTP.t) ->
      ()

  let serve ?switch ?tags ?restore t config =
    let {Vat_config.secret_key = _; serve_tls; listen_address; public_address} = config in
    let vat =
      let auth = Vat_config.auth config in
      let secret_key = lazy (fst (Lazy.force config.secret_key)) in
      Vat.create ?switch ?tags ?restore ~address:(public_address, auth) ~secret_key t
    in
    match listen_address with
    | `TCP port ->
      Stack.listen_tcpv4 t.stack ~port (fun flow ->
          Log.info (fun f -> f ?tags "Accepting new connection");
          let secret_key = if serve_tls then Some (Vat_config.secret_key config) else None in
          Lwt.async (fun () -> handle_connection ?tags ~secret_key vat flow);
          Lwt.return_unit
        );
      Log.info (fun f -> f ?tags "Waiting for %s connections on %a"
                    (if serve_tls then "(encrypted)" else "UNENCRYPTED")
                    Vat_config.Listen_address.pp listen_address);
      Lwt.return vat

  let client_only_vat ?switch ?tags ?restore t =
    let secret_key = lazy (Capnp_rpc_net.Auth.Secret_key.generate ()) in
    Vat.create ?switch ?tags ?restore ~secret_key t
end
