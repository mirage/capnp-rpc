open Examples
open Lwt.Infix
open Capnp_rpc_lwt

module Test_utils = Testbed.Test_utils

(* Create a client/server pair, with the server providing [service].
   Return the client proxy to it.
   Everything gets shut down when the switch is turned off. *)
let run_server ~switch ~service () =
  let server_socket, client_socket = Unix.(socketpair PF_UNIX SOCK_STREAM 0) in
  let _server =
    CapTP.of_endpoint ~tags:Test_utils.server_tags ~switch ~offer:service (Endpoint.of_socket ~switch server_socket)
  in
  let client =
    CapTP.of_endpoint ~tags:Test_utils.client_tags ~switch (Endpoint.of_socket ~switch client_socket)
  in
  CapTP.bootstrap client

(* Generic Lwt running for Alcotest. *)
let run_lwt fn () =
  Logs.info (fun f -> f "Start test-case");
  let async_ex, async_waker = Lwt.wait () in
  let handle_exn ex =
    Logs.info (fun f -> f "Uncaught async exception: %a" Fmt.exn ex);
    if Lwt.state async_ex = Lwt.Sleep then
      Lwt.wakeup_exn async_waker ex
  in
  Lwt.async_exception_hook := handle_exn;
  Lwt_main.run (Lwt_switch.with_switch (fun sw -> Lwt.pick [fn sw; async_ex]))

let test_simple switch =
  let service = run_server ~switch ~service:(Echo.service ()) () in
  Echo.Client.ping service "ping" >>= fun reply ->
  Alcotest.(check string) "Ping response" "got:0:ping" reply;
  Capability.dec_ref service;
  Lwt.return ()

let test_parallel switch =
  let service = run_server ~switch ~service:(Echo.service ()) () in
  let reply1 = Echo.Client.ping service ~slow:true "ping1" in
  Echo.Client.ping service "ping2" >|= Alcotest.(check string) "Ping2 response" "got:1:ping2" >>= fun () ->
  assert (Lwt.state reply1 = Lwt.Sleep);
  Echo.Client.unblock service >>= fun () ->
  reply1 >|= Alcotest.(check string) "Ping1 response" "got:0:ping1" >>= fun () ->
  Lwt.return ()

let test_registry switch =
  let registry_impl = Registry.service () in
  let registry = run_server ~switch ~service:registry_impl () in
  let echo_service = Registry.Client.echo_service registry in
  Registry.Client.unblock registry >>= fun () ->
  Echo.Client.ping echo_service "ping" >|= Alcotest.(check string) "Ping response" "got:0:ping" >>= fun () ->
  Lwt.return ()

let test_embargo switch =
  let registry_impl = Registry.service () in
  let local_echo = Echo.service () in
  let registry = run_server ~switch ~service:registry_impl () in
  Registry.Client.set_echo_service registry local_echo >>= fun () ->
  let echo_service = Registry.Client.echo_service registry in
  let reply1 = Echo.Client.ping echo_service "ping" in
  Registry.Client.unblock registry >>= fun () ->
  reply1 >|= Alcotest.(check string) "Ping response" "got:0:ping" >>= fun () ->
  (* Flush, to ensure we resolve the echo_service's location. *)
  Echo.Client.ping echo_service "ping" >|= Alcotest.(check string) "Ping response" "got:1:ping" >>= fun () ->
  (* Test local connection. *)
  Echo.Client.ping echo_service "ping" >|= Alcotest.(check string) "Ping response" "got:2:ping" >>= fun () ->
  Lwt.return ()

let test_calculator switch =
  let c = Calc.client @@ run_server ~switch ~service:Calc.service () in
  (c#evaluate (`Float 1.))#read >|= Alcotest.(check float) "Simple calc" 1. >>= fun () ->
  let local_add = Calc.add in
  let expr = `Call (local_add, [`Float 1.; `Float 2.]) in
  (c#evaluate expr)#read >|= Alcotest.(check float) "Complex with local fn" 3. >>= fun () ->
  let remote_add = c#getOperator `Add in
  (Calc.fn_client remote_add)#call [5.; 3.] >|= Alcotest.(check float) "Check fn" 8. >>= fun () ->
  let expr = `Call (remote_add, [`Float 1.; `Float 2.]) in
  (c#evaluate expr)#read >|= Alcotest.(check float) "Complex with remote fn" 3. >>= fun () ->
  Lwt.return ()

let rpc_tests = [
  "Simple",     `Quick, run_lwt test_simple;
  "Parallel",   `Quick, run_lwt test_parallel;
  "Embargo",    `Quick, run_lwt test_embargo;
  "Registry",   `Quick, run_lwt test_registry;
  "Calculator", `Quick, run_lwt test_calculator;
]

let () =
  Alcotest.run ~and_exit:false "capnp-rpc" [
    "lwt", rpc_tests;
  ]
