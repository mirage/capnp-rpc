(* Run the calc service as a child process, connecting directly over a socketpair.
   Unlike a normal connection, there is no encryption or use of sturdy refs here. *)

open Eio.Std

module Calc = Testlib.Calc

let service_name = Capnp_rpc_net.Restorer.Id.public "my-service"
(* The name of the service that the child offers and the parent requests.
   It doesn't matter what this is, as long as they both use the same string. *)

module Logging = struct
  let reporter id =
    let report src _level ~over k msgf =
      let src = Logs.Src.name src in
      msgf @@ fun ?header:_ ?tags:_ fmt ->
      let print _ =
        over ();
        k ()
      in
      Fmt.kpf print Fmt.stdout ("%6s: %a: @[" ^^ fmt ^^ "@]@.")
        id Fmt.(styled `Magenta string) (Printf.sprintf "%11s" src)
    in
    { Logs.report = report }

  let init id =
    Fmt_tty.setup_std_outputs ();
    Logs.(set_level (Some Info));
    Logs.set_reporter (reporter id)
end

let run_connection conn =
  Fiber.both
    (* Normally the vat runs a leak handler to free resources that get GC'd
       with a non-zero reference count. We're not using a vat, so run it ourselves. *)
    Capnp_rpc.Leak_handler.run
    (fun () -> Capnp_rpc_unix.CapTP.run conn)

module Parent = struct
  let run socket =
    Logging.init "parent";
    Switch.run @@ fun sw -> 
    (* Run Cap'n Proto RPC protocol on [socket]: *)
    let p = Capnp_rpc_net.Endpoint.of_flow socket ~peer_id:Capnp_rpc_net.Auth.Digest.insecure in
    Logs.info (fun f -> f "Connecting to child process...");
    let conn = Capnp_rpc_unix.CapTP.connect ~sw ~restore:Capnp_rpc_net.Restorer.none p in
    Fiber.fork_daemon ~sw (fun () -> run_connection conn; `Stop_daemon);
    (* Get the child's service object: *)
    let calc = Capnp_rpc_unix.CapTP.bootstrap conn service_name in
    (* Use the service: *)
    Logs.app (fun f -> f "Sending request...");
    let remote_mul = Calc.getOperator calc `Multiply in
    let result = Calc.evaluate calc Calc.Expr.(Call (remote_mul, [Float 21.0; Float 2.0])) in
    let v = Calc.Value.read result in
    Logs.app (fun f -> f "Result: %f" v);
    Logs.app (fun f -> f "Shutting down...")
end

module Child = struct
  let run socket =
    Logging.init "child";
    Switch.run @@ fun sw -> 
    let socket = Eio_unix.Net.import_socket_stream ~sw ~close_unix:false socket in
    let service = Calc.local ~sw in
    let restore = Capnp_rpc_net.Restorer.single service_name service in
    (* Run Cap'n Proto RPC protocol on [socket]: *)
    let endpoint = Capnp_rpc_net.Endpoint.of_flow socket ~peer_id:Capnp_rpc_net.Auth.Digest.insecure in
    let conn = Capnp_rpc_unix.CapTP.connect ~sw ~restore endpoint in
    Logs.info (fun f -> f "Serving requests...");
    run_connection conn
end

let () =
  Eio_main.run @@ fun env ->
  let prog_mgr = env#process_mgr in
  match Sys.argv with
  | [| prog |] ->
    (* We are the parent. *)
    Switch.run @@ fun sw -> 
    let prog = if Filename.is_implicit prog then "./" ^ prog else prog in
    let p, c = Eio_unix.Net.socketpair_stream ~sw () in
    (* Run the child, passing the socket as its stdin. *)
    let _child = Eio.Process.spawn ~sw prog_mgr [prog; "--child"] ~stdin:c in
    Eio.Net.close c;
    Parent.run p;
    Logs.info (fun f -> f "Done")
  | [| _prog; "--child" |] ->
    (* We are the child. Our socket is on stdin. *)
    Child.run Unix.stdin
  | _ ->
    failwith "Run this command without arguments."
