
open Lwt.Infix

open Capnp_rpc_lwt 

let () =
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ())

let rec create_for v = function
  | 0. -> []
  | n -> v n :: (create_for v (n-.1.))

let run_client service = 
  let n = 100000. in
  let ops = n |> create_for (fun i -> 
      fun () -> 
        Echo.ping service (Float.to_string i) >>= fun res ->
        Lwt.return (res = "echo:" ^ (Float.to_string i))
    ) in
  let st = Unix.gettimeofday () in
  Lwt_list.map_p (fun v -> v ()) ops >>= fun _ ->
  let ed = Unix.gettimeofday () in 
  let rate = n /. (ed -. st) in
  Logs.info (fun m -> m "rate = %f" rate );
  Lwt.return_unit

let secret_key = `Ephemeral
let listen_address = `TCP ("127.0.0.1", 7000)

let start_server () =
  let config = Capnp_rpc_unix.Vat_config.create ~secret_key ~serve_tls:false listen_address in
  let service_id = Capnp_rpc_unix.Vat_config.derived_id config "main" in
  let restore = Capnp_rpc_net.Restorer.single service_id Echo.local in
  Capnp_rpc_unix.serve config ~restore >|= fun vat ->
  Capnp_rpc_unix.Vat.sturdy_uri vat service_id

let () =
  Lwt_main.run begin
    start_server () >>= fun uri ->
    Fmt.pr "Connecting to echo service at: %a@." Uri.pp_hum uri;
    let client_vat = Capnp_rpc_unix.client_only_vat () in
    let sr = Capnp_rpc_unix.Vat.import_exn client_vat uri in
    Sturdy_ref.connect_exn sr >>= fun proxy -> 
    run_client proxy
  end
