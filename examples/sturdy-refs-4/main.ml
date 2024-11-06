open Lwt.Infix
open Capnp_rpc.Std

module Restorer = Capnp_rpc_net.Restorer

let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let or_fail = function
  | Ok x -> x
  | Error (`Msg m) -> failwith m

(* $MDX part-begin=server *)
let serve config =
  Lwt_main.run begin
    (* Create the on-disk store *)
    let make_sturdy = Capnp_rpc_unix.Vat_config.sturdy_uri config in
    let db, set_loader = Db.create ~make_sturdy "./store" in
    (* Create the restorer *)
    let services = Restorer.Table.of_loader (module Db) db in
    let restore = Restorer.of_table services in
    (* Add the root service *)
    let persist_new ~label =
      let id = Db.save_new db ~label in
      Capnp_rpc_net.Restorer.restore restore id
    in
    let root_id = Capnp_rpc_unix.Vat_config.derived_id config "root" in
    let root =
      let sr = Capnp_rpc_net.Restorer.Table.sturdy_ref services root_id in
      Logger.local ~persist_new sr "root"
    in
    Restorer.Table.add services root_id root;
    (* Tell the database how to restore saved loggers *)
    Lwt.wakeup set_loader (fun sr ~label -> Restorer.grant @@ Logger.local ~persist_new sr label);
    (* Run the server *)
    Capnp_rpc_unix.serve config ~restore >>= fun _vat ->
    let uri = Capnp_rpc_unix.Vat_config.sturdy_uri config root_id in
    Capnp_rpc_unix.Cap_file.save_uri uri "admin.cap" |> or_fail;
    print_endline "Wrote admin.cap";
    fst @@ Lwt.wait () (* Wait forever *)
  end
(* $MDX part-end *)

let log cap_file msg =
  Lwt_main.run begin
    let vat = Capnp_rpc_unix.client_only_vat () in
    let sr = Capnp_rpc_unix.Cap_file.load vat cap_file |> or_fail in
    Sturdy_ref.with_cap_exn sr @@ fun logger ->
    Logger.log logger msg
  end

let sub cap_file label =
  Lwt_main.run begin
    let sub_file = label ^ ".cap" in
    if Sys.file_exists sub_file then Fmt.failwith "%S already exists!" sub_file;
    let vat = Capnp_rpc_unix.client_only_vat () in
    let sr = Capnp_rpc_unix.Cap_file.load vat cap_file |> or_fail in
    Sturdy_ref.with_cap_exn sr @@ fun logger ->
    Capability.with_ref (Logger.sub logger label) @@ fun sub ->
    Capnp_rpc.Persistence.save_exn sub >>= fun uri ->
    Capnp_rpc_unix.Cap_file.save_uri uri sub_file |> or_fail;
    Printf.printf "Wrote %S\n%!" sub_file;
    Lwt.return_unit
  end

open Cmdliner

let serve_cmd =
  let doc = "run the server" in
  let info = Cmd.info "serve" ~doc in
  Cmd.v info Term.(const serve $ Capnp_rpc_unix.Vat_config.cmd)

let cap_file =
  let i = Arg.info [] ~docv:"PATH" ~doc:"logger.cap file" in
  Arg.(required @@ pos 0 (some file) None i)

let msg =
  let i = Arg.info [] ~docv:"MSG" ~doc:"Message to send" in
  Arg.(required @@ pos 1 (some string) None i)

let label =
  let i = Arg.info [] ~docv:"LABEL" ~doc:"Tag for new logger" in
  Arg.(required @@ pos 1 (some string) None i)

let log_cmd =
  let doc = "log a message" in
  let info = Cmd.info "log" ~doc in
  Cmd.v info Term.(const log $ cap_file $ msg)

let sub_cmd =
  let doc = "create a sub-logger" in
  let info = Cmd.info "sub" ~doc in
  Cmd.v info Term.(const sub $ cap_file $ label)

let cmds = [serve_cmd; sub_cmd; log_cmd]

let () =
  let doc = "a command-line interface for logger service" in
  let info = Cmd.info ~doc "logger-client" in
  exit (Cmd.eval @@ Cmd.group info cmds)
