open Eio.Std
open Capnp_rpc.Std

let delay = if Sys.getenv_opt "CI" = None then 1.0 else 0.0

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
  let delay = Eio.Time.Timeout.seconds env#mono_clock delay in
  let service = Echo.local ~delay in
  run_client service
