open Eio.Std
open Capnp_rpc_lwt

module Restorer = Capnp_rpc_net.Restorer

let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let secret_key = `Ephemeral
let listen_address = `TCP ("127.0.0.1", 7000)

let or_fail = function
  | Ok x -> x
  | Error (`Msg m) -> failwith m

(* $MDX part-begin=main *)
let make_service ~config ~services name =
  let service = Logger.local name in
  let id = Capnp_rpc_unix.Vat_config.derived_id config name in
  Restorer.Table.add services id service;
  name, id

let start_server ~sw net =
  let config = Capnp_rpc_unix.Vat_config.create ~secret_key listen_address in
  let make_sturdy = Capnp_rpc_unix.Vat_config.sturdy_uri config in
  let services = Restorer.Table.create make_sturdy in
  let restore = Restorer.of_table services in
  let services = List.map (make_service ~config ~services) ["alice"; "bob"] in
  let vat = Capnp_rpc_unix.serve ~sw ~net ~restore config in
  services |> List.iter (fun (name, id) ->
      let cap_file = name ^ ".cap" in
      Capnp_rpc_unix.Cap_file.save_service vat id cap_file |> or_fail;
      Printf.printf "[server] saved %S\n%!" cap_file
    )

let run_client ~sw ~net cap_file msg =
  let vat = Capnp_rpc_unix.client_only_vat ~sw net in
  let sr = Capnp_rpc_unix.Cap_file.load vat cap_file |> or_fail in
  Printf.printf "[client] loaded %S\n%!" cap_file;
  Sturdy_ref.with_cap_exn sr @@ fun cap ->
  Logger.log cap msg

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let net = env#net in
  start_server ~sw net;
  run_client ~sw ~net "./alice.cap" "Message from Alice";
  run_client ~sw ~net "./bob.cap" "Message from Bob";
  raise Exit
(* $MDX part-end *)
