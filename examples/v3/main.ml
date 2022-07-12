open Lwt.Infix
open Capnp_rpc_lwt

let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let callback_fn msg =
  Fmt.pr "Callback got %S@." msg

let run_client service =
  Capability.with_ref (Echo.Callback.local callback_fn) @@ fun callback ->
  Echo.heartbeat service "foo" callback

let secret_key = `Ephemeral
let listen_address = `TCP ("127.0.0.1", 7000)

let start_server () =
  let config = Capnp_rpc_unix.Vat_config.create ~secret_key listen_address in
  let service_id = Capnp_rpc_unix.Vat_config.derived_id config "main" in
  let restore = Capnp_rpc_net.Restorer.single service_id Echo.local in
  Capnp_rpc_unix.serve config ~restore >|= fun vat ->
  Capnp_rpc_unix.Vat.sturdy_uri vat service_id

let () =
  Lwt_main.run begin
    start_server () >>= fun uri ->
    Fmt.pr "Connecting to echo service at: %a@." Uri.pp_hum uri;
    let client_vat = Capnp_rpc_unix.client_only_vat () in
    let sr = Capnp_rpc_unix.Vat.import_exn client_vat uri in
    Sturdy_ref.with_cap_exn sr run_client
  end
