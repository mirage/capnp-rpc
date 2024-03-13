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

module Parent = struct
  let run socket =
    Logging.init "parent";
    Switch.run @@ fun sw -> 
    (* Run Cap'n Proto RPC protocol on [socket]: *)
    let p = Eio_unix.Net.import_socket_stream ~sw ~close_unix:true socket
            |> Capnp_rpc_net.Endpoint.of_flow
              ~peer_id:Capnp_rpc_net.Auth.Digest.insecure
    in
    Logs.info (fun f -> f "Connecting to child process...");
    let conn = Capnp_rpc_unix.CapTP.connect ~sw ~restore:Capnp_rpc_net.Restorer.none p in
    Fiber.fork_daemon ~sw (fun () -> Capnp_rpc_unix.CapTP.listen conn; `Stop_daemon);
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
    let endpoint = Capnp_rpc_net.Endpoint.of_flow socket
        ~peer_id:Capnp_rpc_net.Auth.Digest.insecure
    in
    let conn = Capnp_rpc_unix.CapTP.connect ~sw ~restore endpoint in
    Logs.info (fun f -> f "Serving requests...");
    Capnp_rpc_unix.CapTP.listen conn
end

let find_our_path prog =
  if Sys.file_exists prog then prog
  else (
    (* Hack for running under "dune exec" *)
    let prog = "./_build/default/" ^ prog in
    if Sys.file_exists prog then prog
    else Fmt.failwith "Can't find path to own binary %S from %S" prog (Sys.getcwd ())
  )

let await_exit pid =
  Eio_unix.run_in_systhread @@ fun () ->
  let rec aux () =
    match Unix.waitpid [] pid with
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> aux ()
    | _pid, status -> status
  in
  aux ()

let () =
  Eio_main.run @@ fun _env ->
  match Sys.argv with
  | [| prog |] ->
    (* We are the parent. *)
    let prog = find_our_path prog in
    let p, c = Unix.(socketpair PF_UNIX SOCK_STREAM 0 ~cloexec:true) in
    Unix.clear_close_on_exec c;
    (* Run the child, passing the socket as its stdin. *)
    let child = Unix.create_process prog [| prog; "--child" |] c Unix.stdout Unix.stderr in
    Parent.run p;
    Logs.info (fun f -> f "Waiting for child to exit...");
    Unix.kill child Sys.sigkill;
    let _ : Unix.process_status = await_exit child in
    Logs.info (fun f -> f "Done")
  | [| _prog; "--child" |] ->
    (* We are the child. Our socket is on stdin. *)
    Child.run Unix.stdin
  | _ ->
    failwith "Run this command without arguments."
