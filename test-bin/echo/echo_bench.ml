open Eio.Std

open Capnp_rpc_lwt 

let () =
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ())

let run_client service = 
  let n = 100000 in
  let ops = List.init n (fun i -> 
      let payload = Int.to_string i in
      let desired_result = "echo:" ^ payload in
      fun () ->
        let res = Echo.ping service payload in
        assert (res = desired_result)
    ) in
  let st = Unix.gettimeofday () in
  ops |> Fiber.List.iter ~max_fibers:12 (fun v -> v ());
  let ed = Unix.gettimeofday () in 
  (Int.to_float n) /. (ed -. st)

let secret_key = `Ephemeral
let listen_address = `TCP ("127.0.0.1", 7000)

let start_server ~sw net =
  let config = Capnp_rpc_unix.Vat_config.create ~secret_key ~serve_tls:false listen_address in
  let service_id = Capnp_rpc_unix.Vat_config.derived_id config "main" in
  let restore = Capnp_rpc_net.Restorer.single service_id Echo.local in
  let vat = Capnp_rpc_unix.serve ~sw ~net ~restore config in
  Capnp_rpc_unix.Vat.sturdy_uri vat service_id

exception Done of float

let () =
  Eio_main.run @@ fun env ->
  try
    Switch.run ~name:"main" @@ fun sw ->
    let uri = start_server ~sw env#net in
    Fmt.pr "Connecting to echo service at: %a@." Uri.pp_hum uri;
    Switch.run ~name:"client" @@ fun sw ->
    let client_vat = Capnp_rpc_unix.client_only_vat ~sw env#net in
    let sr = Capnp_rpc_unix.Vat.import_exn client_vat uri in
    let rate = Sturdy_ref.with_cap_exn sr run_client in
    raise (Done rate)
  with Done rate ->
    Logs.info (fun m -> m "rate = %f" rate)
