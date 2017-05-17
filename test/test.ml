open Examples
open Lwt.Infix

let test_simple switch =
  let client = Echo.client @@ Test_utils.run_server ~switch ~service:Echo.service () in
  client#ping "ping" >>= fun reply ->
  Alcotest.(check string) "Ping response" "got:ping" reply;
  Lwt.return ()

let test_parallel switch =
  let client = Echo.client @@ Test_utils.run_server ~switch ~service:Echo.service () in
  let reply1 = client#ping ~slow:true "ping1" in
  client#ping "ping2" >|= Alcotest.(check string) "Ping2 response" "got:ping2" >>= fun () ->
  assert (Lwt.state reply1 = Lwt.Sleep);
  client#unblock () >>= fun () ->
  reply1 >|= Alcotest.(check string) "Ping1 response" "got:ping1" >>= fun () ->
  Lwt.return ()

let test_registry switch =
  let registry = Registry.client @@ Test_utils.run_server ~switch ~service:Registry.service () in
  let client = registry#echo_service in
  client#ping "ping" >|= Alcotest.(check string) "Ping response" "got:ping" >>= fun () ->
  Lwt.return ()

let test_calculator switch =
  let c = Calc.client @@ Test_utils.run_server ~switch ~service:Calc.service () in
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
  "Simple",     `Quick, Test_utils.run_lwt test_simple;
  "Parallel",   `Quick, Test_utils.run_lwt test_parallel;
  "Registry",   `Quick, Test_utils.run_lwt test_registry;
  "Calculator", `Quick, Test_utils.run_lwt test_calculator;
]

let () =
  Alcotest.run ~and_exit:false "capnp-rpc" [
    "RPC", rpc_tests;
  ]
