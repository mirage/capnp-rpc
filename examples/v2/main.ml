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

let () =
  Eio_main.run @@ fun env ->
  let clock = if Sys.getenv_opt "CI" = None then env#clock else Fake_clock.v in
  let service = Echo.local ~clock in
  run_client service
