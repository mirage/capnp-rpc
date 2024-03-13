open Eio.Std
open Capnp_rpc_lwt

let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let callback_fn msg =
  traceln "Callback got %S" msg

let run_client service =
  Capability.with_ref (Echo.Callback.local callback_fn) @@ fun callback ->
  Echo.heartbeat service "foo" callback

let secret_key = `Ephemeral
let listen_address = `TCP ("127.0.0.1", 7000)

let start_server ~sw ~clock net =
  let config = Capnp_rpc_unix.Vat_config.create ~secret_key listen_address in
  let service_id = Capnp_rpc_unix.Vat_config.derived_id config "main" in
  let restore = Capnp_rpc_net.Restorer.single service_id (Echo.local ~clock) in
  let vat = Capnp_rpc_unix.serve ~sw ~net ~restore config in
  Capnp_rpc_unix.Vat.sturdy_uri vat service_id

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let clock = if Sys.getenv_opt "CI" = None then env#clock else Fake_clock.v in
  let uri = start_server ~sw ~clock env#net in
  traceln "Connecting to echo service at: %a" Uri.pp_hum uri;
  let client_vat = Capnp_rpc_unix.client_only_vat ~sw env#net in
  let sr = Capnp_rpc_unix.Vat.import_exn client_vat uri in
  Sturdy_ref.with_cap_exn sr run_client;
  raise Exit
