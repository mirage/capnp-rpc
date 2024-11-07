open Lwt.Infix
open Capnp_rpc.Std

module Restorer = Capnp_rpc_net.Restorer

let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let secret_key = `Ephemeral
let listen_address = `TCP ("127.0.0.1", 7000)

let or_fail = function
  | Ok x -> x
  | Error (`Msg m) -> failwith m

let start_server () =
  let config = Capnp_rpc_unix.Vat_config.create ~secret_key listen_address in
  let make_sturdy = Capnp_rpc_unix.Vat_config.sturdy_uri config in
  let services = Restorer.Table.create make_sturdy in
  let restore = Restorer.of_table services in
  let root_id = Capnp_rpc_unix.Vat_config.derived_id config "root" in
  let root = Logger.local "root" in
  Restorer.Table.add services root_id root;
  Capnp_rpc_unix.serve config ~restore >|= fun _vat ->
  Capnp_rpc_unix.Vat_config.sturdy_uri config root_id

(* $MDX part-begin=main *)
let () =
  Lwt_main.run begin
    start_server () >>= fun root_uri ->
    let vat = Capnp_rpc_unix.client_only_vat () in
    let root_sr = Capnp_rpc_unix.Vat.import vat root_uri |> or_fail in
    Sturdy_ref.with_cap_exn root_sr @@ fun root ->
    Logger.log root "Message from Admin" >>= fun () ->
    Capability.with_ref (Logger.sub root "alice") @@ fun for_alice ->
    Capability.with_ref (Logger.sub root "bob") @@ fun for_bob ->
    Logger.log for_alice "Message from Alice" >>= fun () ->
    Logger.log for_bob "Message from Bob"
  end
(* $MDX part-end *)
