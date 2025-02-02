open Eio.Std
open Capnp_rpc.Std

let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let callback_fn msg =
  traceln "Callback got %S" msg

let run_client service =
  Capability.with_ref (Echo.Callback.local callback_fn) @@ fun callback ->
  Echo.heartbeat service "foo" callback

let connect net uri =
  Switch.run @@ fun sw ->
  let client_vat = Capnp_rpc_unix.client_only_vat ~sw net in
  let sr = Capnp_rpc_unix.Vat.import_exn client_vat uri in
  Capnp_rpc_unix.with_cap_exn sr run_client

open Cmdliner

let ( $$ ) f x = Term.(const f $ x)

let connect_addr =
  let i = Arg.info [] ~docv:"ADDR" ~doc:"Address of server (capnp://...)" in
  Arg.(required @@ pos 0 (some Capnp_rpc_unix.sturdy_uri) None i)

let connect_cmd env =
  let doc = "run the client" in
  let info = Cmd.info "connect" ~doc in
  Cmd.v info (connect env#net $$ connect_addr)

let () =
  exit @@ Eio_main.run @@ fun env ->
  Cmd.eval (connect_cmd env)
