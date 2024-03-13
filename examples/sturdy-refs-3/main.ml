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

let start_server ~sw net =
  let config = Capnp_rpc_unix.Vat_config.create ~secret_key listen_address in
  let make_sturdy = Capnp_rpc_unix.Vat_config.sturdy_uri config in
  let services = Restorer.Table.create make_sturdy in
  Switch.on_release sw (fun () -> Restorer.Table.clear services);
  let restore = Restorer.of_table services in
  (* $MDX part-begin=root *)
  let root_id = Capnp_rpc_unix.Vat_config.derived_id config "root" in
  let root =
    let sr = Capnp_rpc_net.Restorer.Table.sturdy_ref services root_id in
    Logger.local ~services sr "root"
  in
  (* $MDX part-end *)
  Restorer.Table.add services root_id root;
  let _vat = Capnp_rpc_unix.serve ~sw ~net ~restore config in
  Capnp_rpc_unix.Vat_config.sturdy_uri config root_id

let run_client ~sw ~net cap_file =
  let vat = Capnp_rpc_unix.client_only_vat ~sw net in
  let sr = Capnp_rpc_unix.Cap_file.load vat cap_file |> or_fail in
  Sturdy_ref.with_cap_exn sr @@ fun for_alice ->
  Logger.log for_alice "Message from Alice"

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let net = env#net in
  let root_uri = start_server ~sw net in
  let vat = Capnp_rpc_unix.client_only_vat ~sw net in
  let root_sr = Capnp_rpc_unix.Vat.import vat root_uri |> or_fail in
  Sturdy_ref.with_cap_exn root_sr @@ fun root ->
  Logger.log root "Message from Admin";
  (* $MDX part-begin=save *)
  (* The admin creates a logger for Alice and saves it: *)
  let uri = Capability.with_ref (Logger.sub root "alice") Persistence.save_exn in
  Capnp_rpc_unix.Cap_file.save_uri uri "alice.cap" |> or_fail;
  (* Alice uses it: *)
  run_client ~sw ~net "alice.cap";
  raise Exit
  (* $MDX part-end *)
