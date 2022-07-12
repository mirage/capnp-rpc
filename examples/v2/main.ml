open Capnp_rpc_lwt

let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let callback_fn msg =
  Fmt.pr "Callback got %S@." msg

let run_client service =
  Capability.with_ref (Echo.Callback.local callback_fn) @@ fun callback ->
  Echo.heartbeat service "foo" callback

let () =
  Lwt_main.run begin
    let service = Echo.local in
    run_client service
  end
