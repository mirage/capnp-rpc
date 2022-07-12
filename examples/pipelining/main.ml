open Lwt.Infix
open Capnp_rpc_lwt

let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let callback_fn msg =
  Fmt.pr "Callback got %S@." msg

(* $MDX part-begin=run-client *)
let run_client service =
  let logger = Echo.get_logger service in
  Echo.Callback.log logger "Message from client" >|= function
  | Ok () -> ()
  | Error (`Capnp err) ->
    Fmt.epr "Server's logger failed: %a" Capnp_rpc.Error.pp err
(* $MDX part-end *)

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
    Fmt.pr "[client] Connecting to echo service...@.";
    let client_vat = Capnp_rpc_unix.client_only_vat () in
    let sr = Capnp_rpc_unix.Vat.import_exn client_vat uri in
    Sturdy_ref.with_cap_exn sr run_client
  end
