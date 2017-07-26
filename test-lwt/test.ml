open Examples
open Lwt.Infix
open Capnp_rpc_lwt

module Test_utils = Testbed.Test_utils

type cs = {
  client : CapTP.t;
  server : CapTP.t;
}

(* Have the client ask the server for its bootstrap object, and return the
   resulting client-side proxy to it. *)
let get_bootstrap cs =
  CapTP.bootstrap cs.client

module Utils = struct
  [@@@ocaml.warning "-32"]

  let dump cs =
    Logs.info (fun f -> f ~tags:Test_utils.client_tags "%a" CapTP.dump cs.client);
    Logs.info (fun f -> f ~tags:Test_utils.server_tags "%a" CapTP.dump cs.server)
end

(* Create a client/server pair, with the server providing [service].
   Return the client proxy to it.
   Everything gets shut down when the switch is turned off. *)
let run_server ~switch ~service () =
  let server_socket, client_socket = Lwt_unix.(socketpair PF_UNIX SOCK_STREAM 0) in
  let server =
    Capnp_rpc_unix.endpoint_of_socket ~switch server_socket
    |> CapTP.connect ~tags:Test_utils.server_tags ~switch ~offer:service
  in
  let client =
    Capnp_rpc_unix.endpoint_of_socket ~switch client_socket
    |> CapTP.connect ~tags:Test_utils.client_tags ~switch
  in
  Capability.dec_ref service;
  { client; server }

(* Generic Lwt running for Alcotest. *)
let run_lwt fn () =
  let warnings_at_start = Logs.(err_count () + warn_count ()) in
  Logs.info (fun f -> f "Start test-case");
  let async_ex, async_waker = Lwt.wait () in
  let handle_exn ex =
    Logs.info (fun f -> f "Uncaught async exception: %a" Fmt.exn ex);
    if Lwt.state async_ex = Lwt.Sleep then
      Lwt.wakeup_exn async_waker ex
  in
  Lwt.async_exception_hook := handle_exn;
  Lwt_main.run begin
    Lwt_switch.with_switch (fun sw ->
        let finished = ref false in
        Lwt_switch.add_hook (Some sw) (fun () ->
            if not !finished then handle_exn (Failure "Switch turned off early");
            Lwt.return_unit
          );
        Lwt.pick [
          async_ex;
          fn sw >|= fun () -> finished := true;
        ] >|= fun () ->
        Gc.full_major ()
      )
  end;
  Lwt.wakeup_paused ();
  Gc.full_major ();
  Lwt.wakeup_paused ();
  Gc.full_major ();
  let warnings_at_end = Logs.(err_count () + warn_count ()) in
  Alcotest.(check int) "Check log for warnings" 0 (warnings_at_end - warnings_at_start)

let test_simple switch =
  let cs = run_server ~switch ~service:(Echo.service ()) () in
  let service = get_bootstrap cs in
  Echo.Client.ping service "ping" >>= fun reply ->
  Alcotest.(check string) "Ping response" "got:0:ping" reply;
  Capability.dec_ref service;
  Lwt.return ()

let test_parallel switch =
  let cs = run_server ~switch ~service:(Echo.service ()) () in
  let service = get_bootstrap cs in
  let reply1 = Echo.Client.ping service ~slow:true "ping1" in
  Echo.Client.ping service "ping2" >|= Alcotest.(check string) "Ping2 response" "got:1:ping2" >>= fun () ->
  assert (Lwt.state reply1 = Lwt.Sleep);
  Echo.Client.unblock service >>= fun () ->
  reply1 >|= Alcotest.(check string) "Ping1 response" "got:0:ping1" >>= fun () ->
  Capability.dec_ref service;
  Lwt.return ()

let test_registry switch =
  let registry_impl = Registry.service () in
  let cs = run_server ~switch ~service:registry_impl () in
  let registry = get_bootstrap cs in
  let echo_service = Registry.Client.echo_service registry in
  Registry.Client.unblock registry >>= fun () ->
  Echo.Client.ping echo_service "ping" >|= Alcotest.(check string) "Ping response" "got:0:ping" >>= fun () ->
  Capability.dec_ref registry;
  Capability.dec_ref echo_service;
  Lwt.return ()

let test_embargo switch =
  let registry_impl = Registry.service () in
  let local_echo = Echo.service () in
  let cs = run_server ~switch ~service:registry_impl () in
  let registry = get_bootstrap cs in
  Registry.Client.set_echo_service registry local_echo >>= fun () ->
  Capability.dec_ref local_echo;
  let echo_service = Registry.Client.echo_service registry in
  let reply1 = Echo.Client.ping echo_service "ping" in
  Registry.Client.unblock registry >>= fun () ->
  reply1 >|= Alcotest.(check string) "Ping response" "got:0:ping" >>= fun () ->
  (* Flush, to ensure we resolve the echo_service's location. *)
  Echo.Client.ping echo_service "ping" >|= Alcotest.(check string) "Ping response" "got:1:ping" >>= fun () ->
  (* Test local connection. *)
  Echo.Client.ping echo_service "ping" >|= Alcotest.(check string) "Ping response" "got:2:ping" >>= fun () ->
  Capability.dec_ref echo_service;
  Capability.dec_ref registry;
  Lwt.return ()

let test_resolve switch =
  let registry_impl = Registry.service () in
  let local_echo = Echo.service () in
  let cs = run_server ~switch ~service:registry_impl () in
  let registry = get_bootstrap cs in
  Registry.Client.set_echo_service registry local_echo >>= fun () ->
  Capability.dec_ref local_echo;
  let echo_service = Registry.Client.echo_service_promise registry in
  let reply1 = Echo.Client.ping echo_service "ping" in
  Registry.Client.unblock registry >>= fun () ->
  reply1 >|= Alcotest.(check string) "Ping response" "got:0:ping" >>= fun () ->
  (* Flush, to ensure we resolve the echo_service's location. *)
  Echo.Client.ping echo_service "ping" >|= Alcotest.(check string) "Ping response" "got:1:ping" >>= fun () ->
  (* Test local connection. *)
  Echo.Client.ping echo_service "ping" >|= Alcotest.(check string) "Ping response" "got:2:ping" >>= fun () ->
  Capability.dec_ref echo_service;
  Capability.dec_ref registry;
  Lwt.return ()

let test_cancel switch =
  let cs = run_server ~switch ~service:(Echo.service ()) () in
  let service = get_bootstrap cs in
  let reply1 = Echo.Client.ping service ~slow:true "ping1" in
  assert (Lwt.state reply1 = Lwt.Sleep);
  Lwt.cancel reply1;
  Lwt.try_bind
    (fun () -> reply1)
    (fun _ -> Alcotest.fail "Should have been cancelled!")
    (function
      | Lwt.Canceled -> Lwt.return ()
      | ex -> Lwt.fail ex
    )
  >>= fun () ->
  Echo.Client.unblock service >|= fun () ->
  Capability.dec_ref service

let test_calculator switch =
  let open Calc in
  let cs = run_server ~switch ~service:Calc.service () in
  let c = get_bootstrap cs in
  Client.evaluate c (Float 1.) |> Client.final_read >|= Alcotest.(check float) "Simple calc" 1. >>= fun () ->
  let local_add = Calc.add in
  let expr = Call (local_add, [Float 1.; Float 2.]) in
  Client.evaluate c expr |> Client.final_read >|= Alcotest.(check float) "Complex with local fn" 3. >>= fun () ->
  let remote_add = Calc.Client.getOperator c `Add in
  Calc.Client.call remote_add [5.; 3.] >|= Alcotest.(check float) "Check fn" 8. >>= fun () ->
  let expr = Call (remote_add, [Float 1.; Float 2.]) in
  Client.evaluate c expr |> Client.final_read >|= Alcotest.(check float) "Complex with remote fn" 3. >>= fun () ->
  Capability.dec_ref remote_add;
  Capability.dec_ref c;
  Lwt.return ()

let test_indexing switch =
  let registry_impl = Registry.service () in
  let cs = run_server ~switch ~service:registry_impl () in
  let registry = get_bootstrap cs in
  let echo_service, version = Registry.Client.complex registry in
  Echo.Client.ping echo_service "ping" >|= Alcotest.(check string) "Ping response" "got:0:ping" >>= fun () ->
  Registry.Version.read version >|= Alcotest.(check string) "Version response" "0.1" >>= fun () ->
  Capability.dec_ref registry;
  Capability.dec_ref echo_service;
  Capability.dec_ref version;
  Lwt.return ()

let rpc_tests = [
  "Simple",     `Quick, run_lwt test_simple;
  "Parallel",   `Quick, run_lwt test_parallel;
  "Embargo",    `Quick, run_lwt test_embargo;
  "Resolve",    `Quick, run_lwt test_resolve;
  "Registry",   `Quick, run_lwt test_registry;
  "Calculator", `Quick, run_lwt test_calculator;
  "Cancel",     `Quick, run_lwt test_cancel;
  "Indexing",   `Quick, run_lwt test_indexing;
]

let () =
  Alcotest.run ~and_exit:false "capnp-rpc" [
    "lwt", rpc_tests;
  ]
