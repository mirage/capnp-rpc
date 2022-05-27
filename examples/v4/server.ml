open Lwt.Infix
open Capnp_rpc_net

let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let cap_file = "echo.cap"

let serve config =
  Lwt_main.run begin
    let service_id = Capnp_rpc_unix.Vat_config.derived_id config "main" in
    let restore = Restorer.single service_id Echo.local in
    Capnp_rpc_unix.serve config ~restore >>= fun vat ->
    match Capnp_rpc_unix.Cap_file.save_service vat service_id cap_file with
    | Error `Msg m -> failwith m
    | Ok () ->
      Fmt.pr "Server running. Connect using %S.@." cap_file;
      fst @@ Lwt.wait ()  (* Wait forever *)
  end

open Cmdliner

let serve_cmd =
  let doc = "run the server" in
  let info = Cmd.info "serve" ~doc in
  Cmd.v info Term.(const serve $ Capnp_rpc_unix.Vat_config.cmd)

let () =
  exit (Cmd.eval serve_cmd)
