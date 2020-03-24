open Astring
open Examples
open Lwt.Infix
open Capnp_rpc_lwt
open Capnp_rpc_net

module Test_utils = Testbed.Test_utils

module Vat = Capnp_rpc_unix.Vat
module CapTP = Capnp_rpc_unix.CapTP
module Unix_flow = Capnp_rpc_unix.Unix_flow
module Tls_wrapper = Capnp_rpc_net.Tls_wrapper.Make(Unix_flow)
module Exception = Capnp_rpc.Exception

type cs = {
  client : Vat.t;
  server : Vat.t;
  client_key : Auth.Secret_key.t;
  server_key : Auth.Secret_key.t;
  serve_tls : bool;
  server_switch : Lwt_switch.t;
}

let ensure_removed path =
  try Unix.unlink path
  with Unix.Unix_error(Unix.ENOENT, _, _) -> ()

(* Have the client ask the server for its bootstrap object, and return the
   resulting client-side proxy to it. *)
let get_bootstrap cs =
  let id = Restorer.Id.public "" in
  let sr = Vat.sturdy_uri cs.server id |> Vat.import_exn cs.client in
  Sturdy_ref.connect_exn sr

module Utils = struct
  [@@@ocaml.warning "-32"]

  let dump cs =
    Logs.info (fun f -> f ~tags:Test_utils.client_tags "%a" Vat.dump cs.client);
    Logs.info (fun f -> f ~tags:Test_utils.server_tags "%a" Vat.dump cs.server)
end

let cap_equal_exn a b =
  match Capability.equal a b with
  | Ok x -> x
  | Error `Unsettled -> Alcotest.failf "Can't compare %a and %a: not settled!"
                          Capability.pp a
                          Capability.pp b

let cap = Alcotest.testable Capability.pp cap_equal_exn

let server_key = Auth.Secret_key.generate ()
let client_key = Auth.Secret_key.generate ()
let bad_key = Auth.Secret_key.generate ()

let server_pem = `PEM (Auth.Secret_key.to_pem_data server_key)

let make_vats ?(serve_tls=false) ~switch ~service () =
  let id = Restorer.Id.public "" in
  let restore = Restorer.single id service in
  let server_config =
    let socket_path = Filename.(concat (Filename.get_temp_dir_name ())) "capnp-rpc-test-server" in
    Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return @@ ensure_removed socket_path);
    Capnp_rpc_unix.Vat_config.create ~secret_key:server_pem ~serve_tls (`Unix socket_path)
  in
  let server_switch = Lwt_switch.create () in
  Capnp_rpc_unix.serve ~switch:server_switch ~tags:Test_utils.server_tags ~restore server_config >>= fun server ->
  Lwt_switch.add_hook (Some switch) (fun () -> Lwt_switch.turn_off server_switch);
  Lwt_switch.add_hook (Some switch) (fun () -> Capability.dec_ref service; Lwt.return_unit);
  Lwt.return {
    client = Vat.create ~switch ~tags:Test_utils.client_tags ~secret_key:(lazy client_key) ();
    server;
    client_key;
    server_key;
    serve_tls;
    server_switch;
  }

(* Generic Lwt running for Alcotest. *)
let run_lwt name ?(expected_warnings=0) fn =
  Alcotest_lwt.test_case name `Quick @@ fun sw () ->
  let warnings_at_start = Logs.(err_count () + warn_count ()) in
  Logs.info (fun f -> f "Start test-case");
  let finished = ref false in
  Lwt_switch.add_hook (Some sw) (fun () ->
      if not !finished then !Lwt.async_exception_hook (Failure "Switch turned off early");
      Lwt.return_unit
    );
  fn sw >>= fun () -> finished := true;
  Lwt_switch.turn_off sw >|= fun () ->
  Gc.full_major ();
  Lwt.wakeup_paused ();
  Gc.full_major ();
  Lwt.wakeup_paused ();
  Gc.full_major ();
  let warnings_at_end = Logs.(err_count () + warn_count ()) in
  Alcotest.(check int) "Check log for warnings" expected_warnings (warnings_at_end - warnings_at_start)

let test_simple switch ~serve_tls =
  make_vats ~switch ~serve_tls ~service:(Echo.local ()) () >>= fun cs ->
  get_bootstrap cs >>= fun service ->
  Echo.ping service "ping" >>= fun reply ->
  Alcotest.(check string) "Ping response" "got:0:ping" reply;
  Capability.dec_ref service;
  Lwt.return ()

let test_bad_crypto switch =
  make_vats ~switch ~serve_tls:true ~service:(Echo.local ()) () >>= fun cs ->
  let id = Restorer.Id.public "" in
  let uri = Vat.sturdy_uri cs.server id in
  let bad_digest = Auth.Secret_key.digest ~hash:`SHA256 bad_key in
  let uri = Auth.Digest.add_to_uri bad_digest uri in
  let sr = Capnp_rpc_unix.Vat.import_exn cs.client uri in
  let old_warnings = Logs.warn_count () in
  Sturdy_ref.connect sr >>= function
  | Ok _ -> Alcotest.fail "Wrong TLS key should have been rejected"
  | Error e ->
    let msg = Fmt.to_to_string Capnp_rpc.Exception.pp e in
    assert (String.is_prefix ~affix:"Failed: TLS connection failed: authentication failure" msg);
    (* Wait for server to log warning *)
    let rec wait () =
      if Logs.warn_count () = old_warnings then Lwt.pause () >>= wait
      else Lwt.return_unit
    in
    wait ()

let test_parallel switch =
  make_vats ~switch ~service:(Echo.local ()) () >>= fun cs ->
  get_bootstrap cs >>= fun service ->
  let reply1 = Echo.ping service ~slow:true "ping1" in
  Echo.ping service "ping2" >|= Alcotest.(check string) "Ping2 response" "got:1:ping2" >>= fun () ->
  assert (Lwt.state reply1 = Lwt.Sleep);
  Echo.unblock service >>= fun () ->
  reply1 >|= Alcotest.(check string) "Ping1 response" "got:0:ping1" >>= fun () ->
  Capability.dec_ref service;
  Lwt.return ()

let test_registry switch =
  let registry_impl = Registry.local () in
  make_vats ~switch ~service:registry_impl () >>= fun cs ->
  get_bootstrap cs >>= fun registry ->
  Capability.with_ref (Registry.echo_service registry) @@ fun echo_service ->
  Registry.unblock registry >>= fun () ->
  Echo.ping echo_service "ping" >|= Alcotest.(check string) "Ping response" "got:0:ping" >>= fun () ->
  Capability.dec_ref registry;
  Lwt.return ()

let test_embargo switch =
  let registry_impl = Registry.local () in
  let local_echo = Echo.local () in
  make_vats ~switch ~service:registry_impl () >>= fun cs ->
  get_bootstrap cs >>= fun registry ->
  Registry.set_echo_service registry local_echo >>= fun () ->
  Capability.dec_ref local_echo;
  let echo_service = Registry.echo_service registry in
  let reply1 = Echo.ping echo_service "ping" in
  Registry.unblock registry >>= fun () ->
  reply1 >|= Alcotest.(check string) "Ping response" "got:0:ping" >>= fun () ->
  (* Flush, to ensure we resolve the echo_service's location. *)
  Echo.ping echo_service "ping" >|= Alcotest.(check string) "Ping response" "got:1:ping" >>= fun () ->
  (* Test local connection. *)
  Echo.ping echo_service "ping" >|= Alcotest.(check string) "Ping response" "got:2:ping" >>= fun () ->
  Capability.dec_ref echo_service;
  Capability.dec_ref registry;
  Lwt.return ()

let test_resolve switch =
  let registry_impl = Registry.local () in
  let local_echo = Echo.local () in
  make_vats ~switch ~service:registry_impl () >>= fun cs ->
  get_bootstrap cs >>= fun registry ->
  Registry.set_echo_service registry local_echo >>= fun () ->
  Capability.dec_ref local_echo;
  let echo_service = Registry.echo_service_promise registry in
  let reply1 = Echo.ping echo_service "ping" in
  Registry.unblock registry >>= fun () ->
  reply1 >|= Alcotest.(check string) "Ping response" "got:0:ping" >>= fun () ->
  (* Flush, to ensure we resolve the echo_service's location. *)
  Echo.ping echo_service "ping" >|= Alcotest.(check string) "Ping response" "got:1:ping" >>= fun () ->
  (* Test local connection. *)
  Echo.ping echo_service "ping" >|= Alcotest.(check string) "Ping response" "got:2:ping" >>= fun () ->
  Capability.dec_ref echo_service;
  Capability.dec_ref registry;
  Lwt.return ()

let test_cancel switch =
  make_vats ~switch ~service:(Echo.local ()) () >>= fun cs ->
  get_bootstrap cs >>= fun service ->
  let reply1 = Echo.ping service ~slow:true "ping1" in
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
  Echo.unblock service >|= fun () ->
  Capability.dec_ref service

let float = Alcotest.testable Fmt.float (=)

let test_calculator switch =
  let open Calc in
  Capability.inc_ref Calc.local;
  make_vats ~switch ~service:Calc.local () >>= fun cs ->
  get_bootstrap cs >>= fun c ->
  Calc.evaluate c (Float 1.) |> Value.final_read >|= Alcotest.check float "Simple calc" 1. >>= fun () ->
  let local_add = Calc.Fn.add in
  let expr = Expr.(Call (local_add, [Float 1.; Float 2.])) in
  Calc.evaluate c expr |> Value.final_read >|= Alcotest.check float "Complex with local fn" 3. >>= fun () ->
  let remote_add = Calc.getOperator c `Add in
  Calc.Fn.call remote_add [5.; 3.] >|= Alcotest.check float "Check fn" 8. >>= fun () ->
  let expr = Expr.(Call (remote_add, [Float 1.; Float 2.])) in
  Calc.evaluate c expr |> Value.final_read >|= Alcotest.check float "Complex with remote fn" 3. >>= fun () ->
  Capability.dec_ref remote_add;
  Capability.dec_ref c;
  Lwt.return ()

let test_calculator2 switch =
  let open Calc in
  Capability.inc_ref Calc.local;
  make_vats ~switch ~service:Calc.local () >>= fun cs ->
  get_bootstrap cs >>= fun c ->
  let remote_add = Calc.getOperator c `Add in
  let remote_mul = Calc.getOperator c `Multiply in
  let expr = Expr.(Call (remote_mul, [Float 4.; Float 6.])) in
  let result = Calc.evaluate c expr in
  let expr = Expr.(Call (remote_add, [Prev result; Float 3.])) in
  let add3 = Calc.evaluate c expr |> Value.final_read in
  let expr = Expr.(Call (remote_add, [Prev result; Float 5.])) in
  let add5 = Calc.evaluate c expr |> Value.final_read in
  add3 >>= fun add3 ->
  add5 >>= fun add5 ->
  Alcotest.check float "First" 27.0 add3;
  Alcotest.check float "Second" 29.0 add5;
  Capability.dec_ref result;
  Capability.dec_ref remote_add;
  Capability.dec_ref remote_mul;
  Capability.dec_ref c;
  Lwt.return ()

let test_indexing switch =
  let registry_impl = Registry.local () in
  make_vats ~switch ~service:registry_impl () >>= fun cs ->
  get_bootstrap cs >>= fun registry ->
  let echo_service, version = Registry.complex registry in
  Echo.ping echo_service "ping" >|= Alcotest.(check string) "Ping response" "got:0:ping" >>= fun () ->
  Registry.Version.read version >|= Alcotest.(check string) "Version response" "0.1" >>= fun () ->
  Capability.dec_ref registry;
  Capability.dec_ref echo_service;
  Capability.dec_ref version;
  Lwt.return ()

let cmd_result t =
  let pp f = function
    | `Error _ -> Fmt.string f "error"
    | `Help -> Fmt.string f "help"
    | `Version -> Fmt.string f "version"
    | `Ok x -> Alcotest.pp t f x
  in
  let equal a b =
    match a, b with
    | `Ok a, `Ok b -> Alcotest.equal t a b
    | _ -> a = b
  in
  Alcotest.testable pp equal

let vat_config = Alcotest.testable Capnp_rpc_unix.Vat_config.pp Capnp_rpc_unix.Vat_config.equal

let config_result = cmd_result vat_config

let test_options () =
  let term = (Capnp_rpc_unix.Vat_config.cmd, Cmdliner.Term.info "main") in
  let config = Cmdliner.Term.eval
      ~argv:[| "main"; "--capnp-secret-key-file=key.pem"; "--capnp-listen-address"; "unix:/run/socket" |] term in
  let expected = `Ok (Capnp_rpc_unix.Vat_config.create
                        ~secret_key:(`File "key.pem")
                        (`Unix "/run/socket")
                     ) in
  Alcotest.check config_result "Unix, same address" expected config;
  let expected = `Ok (Capnp_rpc_unix.Vat_config.create
                       ~secret_key:(`File "key.pem")
                       ~public_address:(`TCP ("1.2.3.4", 7001))
                       (`TCP ("0.0.0.0", 7000))
                     ) in
  Cmdliner.Term.eval ~argv:[| "main";
                              "--capnp-secret-key-file=key.pem";
                              "--capnp-public-address"; "tcp:1.2.3.4:7001";
                              "--capnp-listen-address"; "tcp:0.0.0.0:7000" |] term
  |> Alcotest.check config_result "Using TCP" expected

let expect_ok = function
  | Error (`Msg m) -> Alcotest.fail m
  | Ok x -> x

let test_sturdy_uri () =
  let module Address = Capnp_rpc_unix.Network.Address in
  let address = (module Address : Alcotest.TESTABLE with type t = Address.t) in
  let sturdy_ref = Alcotest.pair address Alcotest.string in
  let check msg expected_uri sr =
    let uri = Address.to_uri sr in
    Alcotest.(check string) msg expected_uri (Uri.to_string uri);
    let sr2 = Address.parse_uri uri |> expect_ok in
    Alcotest.check sturdy_ref msg sr sr2
  in
  let sr = (`Unix "/sock", Auth.Digest.insecure), "" in
  check "Insecure Unix" "capnp://insecure@/sock/" sr;
  let sr = (`TCP ("localhost", 7000), Auth.Digest.insecure), "" in
  check "Insecure TCP" "capnp://insecure@localhost:7000" sr;
  let test_uri = Uri.of_string "capnp://sha-256:s16WV4JeGusAL_nTjvICiQOFqm3LqYrDj3K-HXdMi8s@/" in
  let auth = Auth.Digest.from_uri test_uri |> expect_ok in
  let sr = (`TCP ("localhost", 7000), auth), "main" in
  check "Secure TCP" "capnp://sha-256:s16WV4JeGusAL_nTjvICiQOFqm3LqYrDj3K-HXdMi8s@localhost:7000/bWFpbg" sr;
  let sr = (`Unix "/sock", auth), "main" in
  check "Secure Unix" "capnp://sha-256:s16WV4JeGusAL_nTjvICiQOFqm3LqYrDj3K-HXdMi8s@/sock/bWFpbg" sr

let test_sturdy_self switch =
  let service = Echo.local () in
  Capability.inc_ref service;
  make_vats ~switch ~serve_tls:true ~service () >>= fun cs ->
  let id = Restorer.Id.public "" in
  let sr = Vat.sturdy_uri cs.server id |> Vat.import_exn cs.server in
  Sturdy_ref.connect_exn sr >>= fun service2 ->
  Alcotest.check cap "Restore from same vat" service service2;
  Capability.dec_ref service2;
  Capability.dec_ref service;
  Lwt.return ()

let expect_non_exn = function
  | Ok x -> x
  | Error ex -> Alcotest.failf "expect_non_exn: %a" Capnp_rpc.Exception.pp ex

let except = Alcotest.testable Capnp_rpc.Exception.pp (=)

let test_table_restorer _switch =
  let make_sturdy id = Uri.make ~path:(Restorer.Id.to_string id) () in
  let table = Restorer.Table.create make_sturdy in
  let echo_id = Restorer.Id.public "echo" in
  let registry_id = Restorer.Id.public "registry" in
  let broken_id = Restorer.Id.public "broken" in
  let unknown_id = Restorer.Id.public "unknown" in
  Restorer.Table.add table echo_id @@ Echo.local ();
  Restorer.Table.add table registry_id @@ Registry.local ();
  Restorer.Table.add table broken_id @@ Capability.broken (Capnp_rpc.Exception.v "broken");
  let r = Restorer.of_table table in
  Restorer.restore r echo_id >|= expect_non_exn >>= fun a1 ->
  Echo.ping a1 "ping" >>= fun reply ->
  Alcotest.(check string) "Ping response" "got:0:ping" reply;
  Restorer.restore r echo_id >|= expect_non_exn >>= fun a2 ->
  Alcotest.check cap "Same cap" a1 a2;
  Restorer.restore r registry_id >|= expect_non_exn >>= fun r1 ->
  assert (a1 <> r1);
  Restorer.restore r broken_id >|= expect_non_exn >>= fun x ->
  let expected = Some (Capnp_rpc.Exception.v "broken") in
  Alcotest.(check (option except)) "Broken response" expected (Capability.problem x);
  Restorer.restore r unknown_id >>= fun x ->
  let expected = Error (Capnp_rpc.Exception.v "Unknown persistent service ID") in
  Alcotest.(check (result reject except)) "Missing mapping" expected x;
  Capability.dec_ref a1;
  Capability.dec_ref a2;
  Capability.dec_ref r1;
  Restorer.Table.remove table echo_id;
  Restorer.Table.clear table;
  Lwt.return ()

module Loader = struct
  type t = string -> Restorer.resolution Lwt.t

  let hash _ = `SHA256
  let make_sturdy _ id = Uri.make ~path:(Restorer.Id.to_string id) ()
  let load t _sr digest = t digest
end

let test_fn_restorer _switch =
  let cap = Alcotest.testable Capability.pp (=) in
  let a = Restorer.Id.public "a" in
  let b = Restorer.Id.public "b" in
  let c = Restorer.Id.public "c" in
  let current_c = ref (Restorer.reject (Exception.v "Broken C")) in
  let delay = Lwt_condition.create () in
  let digest = Restorer.Id.digest (Loader.hash ()) in
  let load d =
    if d = digest a then Lwt.return @@ Restorer.grant @@ Echo.local ()
    else if d = digest b then Lwt_condition.wait delay >|= fun () -> Restorer.grant @@ Echo.local ()
    else if d = digest c then Lwt_condition.wait delay >|= fun () -> !current_c
    else Lwt.return @@ Restorer.unknown_service_id
  in
  let table = Restorer.Table.of_loader (module Loader) load in
  let restorer = Restorer.of_table table in
  let restore x = Restorer.restore restorer x in
  (* Check that restoring the same ID twice caches the capability. *)
  restore a >|= expect_non_exn >>= fun a1 ->
  restore a >|= expect_non_exn >>= fun a2 ->
  Alcotest.check cap "Restore cached" a1 a2;
  Capability.dec_ref a1;
  Capability.dec_ref a2;
  (* But if it's released, the next lookup loads a fresh one. *)
  restore a >|= expect_non_exn >>= fun a3 ->
  if a1 = a3 then Alcotest.fail "Returned released cap!";
  Capability.dec_ref a3;
  (* Doing two lookups in parallel only does one load. *)
  let b1 = restore b in
  let b2 = restore b in
  assert (Lwt.state b1 = Lwt.Sleep);
  Lwt_condition.broadcast delay ();
  b1 >|= expect_non_exn >>= fun b1 ->
  b2 >|= expect_non_exn >>= fun b2 ->
  Alcotest.check cap "Restore delayed cached" b1 b2;
  Restorer.Table.clear table;   (* (should have no effect) *)
  Capability.dec_ref b1;
  Capability.dec_ref b2;
  (* Failed lookups aren't cached. *)
  let c1 = restore c in
  Lwt_condition.broadcast delay ();
  c1 >>= fun c1 ->
  let reject = Alcotest.result cap except in
  Alcotest.check reject "C initially fails" (Error (Exception.v "Broken C")) c1;
  let c2 = restore c in
  let c_service = Echo.local () in
  current_c := Restorer.grant c_service;
  Lwt_condition.broadcast delay ();
  c2 >|= expect_non_exn >>= fun c2 ->
  Alcotest.check cap "C now works" c_service c2;
  Capability.dec_ref c2;
  (* Two users; one frees the cap immediately *)
  let b1 =
    restore b >|= expect_non_exn >|= fun b1 ->
    Capability.dec_ref b1;
    b1
  in
  let b2 = restore b in
  Lwt_condition.broadcast delay ();
  b1 >>= fun b1 ->
  b2 >|= expect_non_exn >>= fun b2 ->
  Alcotest.check cap "Cap not freed" b1 b2;
  Capability.dec_ref b2;
  Lwt.return_unit

let test_broken switch =
  make_vats ~switch ~service:(Echo.local ()) () >>= fun cs ->
  get_bootstrap cs >>= fun service ->
  Echo.ping service "ping" >|= Alcotest.(check string) "Ping response" "got:0:ping" >>= fun () ->
  let problem, set_problem = Lwt.wait () in
  Capability.when_broken (fun x -> Lwt.wakeup set_problem x) service;
  Alcotest.check (Alcotest.option except) "Still OK" None @@ Capability.problem service;
  assert (Lwt.state problem = Lwt.Sleep);
  Logs.info (fun f -> f "Turning off server...");
  Lwt_switch.turn_off cs.server_switch >>= fun () ->
  problem >>= fun problem ->
  let expected = Exception.v ~ty:`Disconnected "Vat shut down" in
  Alcotest.check except "Broken callback ran" expected problem;
  assert (Capability.problem service <> None);
  Lwt.catch
    (fun () -> Echo.ping service "ping" >|= fun _ -> Alcotest.fail "Should have failed!")
    (fun _ -> Lwt.return ())
  >|= fun () ->
  Capability.dec_ref service

(* [when_broken] follows promises. *)
let test_broken2 () =
  let promise, resolver = Capability.promise () in
  let problem = ref None in
  Capability.when_broken (fun x -> problem := Some x) promise;
  let p2, r2 = Capability.promise () in
  Capability.resolve_ok resolver p2;
  Alcotest.check (Alcotest.option except) "No problem yet" None !problem;
  let ex = Exception.v "Test" in
  Capability.resolve_ok r2 (Capability.broken ex);
  Alcotest.check (Alcotest.option except) "Now broken" (Some ex) !problem;
  ()

let test_broken3 () =
  let ex = Exception.v "Test" in
  let c = Capability.broken ex in
  let problem = ref None in
  Capability.when_broken (fun x -> problem := Some x) c;
  Alcotest.check (Alcotest.option except) "Broken immediately" (Some ex) !problem

let test_broken4 () =
  let promise, _resolver = Capability.promise () in
  let problem = ref None in
  Capability.when_broken (fun x -> problem := Some x) promise;
  Capability.dec_ref promise;
  Alcotest.check (Alcotest.option except) "Released, not called" None !problem

let test_parallel_connect switch =
  make_vats ~switch ~serve_tls:true ~service:(Echo.local ()) () >>= fun cs ->
  let service = get_bootstrap cs in
  let service2 = get_bootstrap cs in
  service >>= fun service ->
  service2 >>= fun service2 ->
  Capability.wait_until_settled service >>= fun () ->
  Capability.wait_until_settled service2 >>= fun () ->
  Alcotest.check cap "Shared connection" service service2;
  Capability.dec_ref service;
  Capability.dec_ref service2;
  Lwt.return_unit

let test_parallel_fails switch =
  make_vats ~switch ~serve_tls:true ~service:(Echo.local ()) () >>= fun cs ->
  let service = get_bootstrap cs in
  let service2 = get_bootstrap cs in
  service >>= fun service ->
  service2 >>= fun service2 ->
  Lwt_switch.turn_off cs.server_switch >>= fun () ->
  Capability.wait_until_settled service >>= fun () ->
  Capability.wait_until_settled service2 >>= fun () ->
  Alcotest.check cap "Shared failure" service service2;
  Capability.dec_ref service;
  Capability.dec_ref service2;
  (* Restart server (ignore new client) *)
  Lwt.pause () >>= fun () ->
  make_vats ~switch ~serve_tls:true ~service:(Echo.local ()) () >>= fun _cs2 ->
  get_bootstrap cs >>= fun service ->
  Echo.ping service "ping" >|= Alcotest.(check string) "Ping response" "got:0:ping" >>= fun () ->
  Capability.dec_ref service;
  Lwt.return_unit

let test_crossed_calls switch =
  (* Would be good to control the ordering here, to test the various cases.
     Currently, it's not certain which path is actually tested. *)
  let id = Restorer.Id.public "" in
  let make_vat ~secret_key ~tags addr =
    let service = Echo.local () in
    let restore = Restorer.(single id) service in
    let config =
      let secret_key = `PEM (Auth.Secret_key.to_pem_data secret_key) in
      let name = Fmt.strf "capnp-rpc-test-%s" addr in
      let socket_path = Filename.(concat (Filename.get_temp_dir_name ())) name in
      Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return @@ ensure_removed socket_path);
      Capnp_rpc_unix.Vat_config.create ~secret_key (`Unix socket_path)
    in
    Capnp_rpc_unix.serve ~switch ~tags ~restore config >>= fun vat ->
    Lwt_switch.add_hook (Some switch) (fun () -> Capability.dec_ref service; Lwt.return_unit);
    Lwt.return vat
  in
  make_vat ~secret_key:client_key ~tags:Test_utils.client_tags "client" >>= fun client ->
  make_vat ~secret_key:server_key ~tags:Test_utils.server_tags "server" >>= fun server ->
  let sr_to_client = Capnp_rpc_unix.Vat.sturdy_uri client id |> Vat.import_exn server in
  let sr_to_server = Capnp_rpc_unix.Vat.sturdy_uri server id |> Vat.import_exn client in
  let to_client = Sturdy_ref.connect_exn sr_to_client in
  let to_server = Sturdy_ref.connect_exn sr_to_server in
  to_client >>= fun to_client ->
  to_server >>= fun to_server ->
  Logs.info (fun f -> f ~tags:Test_utils.client_tags "%a" Capnp_rpc_unix.Vat.dump client);
  Logs.info (fun f -> f ~tags:Test_utils.server_tags "%a" Capnp_rpc_unix.Vat.dump server);
  let s_got = Echo.ping_result to_client "ping" in
  let c_got = Echo.ping_result to_server "ping" in
  s_got >>= fun s_got ->
  c_got >>= fun c_got ->
  begin match c_got, s_got with
    | Ok x, Ok y -> Lwt.return (x, y)
    | Ok x, Error _ ->
      (* Server got an error. Try client again. *)
      Sturdy_ref.connect_exn sr_to_client >>= fun to_client ->
      Capability.with_ref to_client @@ fun to_client ->
      Echo.ping to_client "ping" >|= fun s_got -> (x, s_got)
    | Error _, Ok y ->
      (* Client got an error. Try server again. *)
      Sturdy_ref.connect_exn sr_to_server >>= fun to_server ->
      Capability.with_ref to_server @@ fun to_server ->
      Echo.ping to_server "ping" >|= fun c_got -> (c_got, y)
    | Error (`Capnp e1), Error (`Capnp e2) ->
      Fmt.failwith "@[<v>Both connections failed!@,%a@,%a@]"
        Capnp_rpc.Error.pp e1
        Capnp_rpc.Error.pp e2
  end >>= fun (c_got, s_got) ->
  Alcotest.(check string) "Client's ping response" "got:0:ping" c_got;
  Alcotest.(check string) "Server's ping response" "got:0:ping" s_got;
  Capability.dec_ref to_client;
  Capability.dec_ref to_server;
  Lwt.return_unit

(* Run test_crossed_calls several times to try to trigger the various behaviours. *)
let test_crossed_calls _switch =
  let rec aux i =
    if i = 0 then Lwt.return_unit
    else (
      Lwt_switch.with_switch test_crossed_calls >>= fun () ->
      aux (i - 1)
    )
  in
  aux 10

let test_store switch =
  (* Persistent server configuration *)
  let db = Store.DB.create () in
  let socket_path = Filename.(concat (Filename.get_temp_dir_name ())) "capnp-rpc-test-server" in
  Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return @@ ensure_removed socket_path);
  let config = Capnp_rpc_unix.Vat_config.create ~secret_key:server_pem (`Unix socket_path) in
  let main_id = Restorer.Id.generate () in
  let start_server ~switch () =
    let make_sturdy = Capnp_rpc_unix.Vat_config.sturdy_uri config in
    let table = Store.File.table ~make_sturdy db in
    Lwt_switch.add_hook (Some switch) (fun () -> Restorer.Table.clear table; Lwt.return_unit);
    let restore = Restorer.of_table table in
    let service = Store.local ~restore db in
    Restorer.Table.add table main_id service;
    Capnp_rpc_unix.serve ~switch ~restore ~tags:Test_utils.server_tags config
  in
  (* Start server *)
  let server_switch = Lwt_switch.create () in
  start_server ~switch:server_switch () >>= fun server ->
  let store_uri = Capnp_rpc_unix.Vat.sturdy_uri server main_id in
  (* Set up client *)
  let client = Capnp_rpc_unix.client_only_vat ~tags:Test_utils.client_tags ~switch () in
  let sr = Capnp_rpc_unix.Vat.import_exn client store_uri in
  Sturdy_ref.connect_exn sr >>= fun store ->
  (* Try creating a file *)
  let file = Store.create_file store in
  Store.File.set file "Hello" >>= fun () ->
  Persistence.save_exn file >>= fun file_sr ->
  let file_sr = Vat.import_exn client file_sr in (* todo: get rid of this step *)
  (* Shut down server *)
  Lwt.async (fun () -> Lwt_switch.turn_off server_switch);
  let broken, set_broken = Lwt.wait () in
  Capability.when_broken (Lwt.wakeup set_broken) file;
  broken >>= fun _ex ->
  assert (Capability.problem file <> None);
  (* Restart server *)
  start_server ~switch () >>= fun _server ->
  (* Reconnect client *)
  Sturdy_ref.connect_exn file_sr >>= fun file ->
  Store.File.get file >>= fun data ->
  Alcotest.(check string) "Read file" "Hello" data;
  (* Clean up *)
  Capability.dec_ref file;
  Capability.dec_ref store;
  Lwt.return_unit

let run name fn = Alcotest_lwt.test_case_sync name `Quick fn

let rpc_tests = [
  run_lwt "Simple"              (test_simple ~serve_tls:false);
  run_lwt "Crypto"              (test_simple ~serve_tls:true);
  run_lwt "Bad crypto"          test_bad_crypto ~expected_warnings:1;
  run_lwt "Parallel"            test_parallel;
  run_lwt "Embargo"             test_embargo;
  run_lwt "Resolve"             test_resolve;
  run_lwt "Registry"            test_registry;
  run_lwt "Calculator"          test_calculator;
  run_lwt "Calculator 2"        test_calculator2;
  run_lwt "Cancel"              test_cancel;
  run_lwt "Indexing"            test_indexing;
  run     "Options"             test_options;
  run     "Sturdy URI"          test_sturdy_uri;
  run_lwt "Sturdy self"         test_sturdy_self;
  run_lwt "Table restorer"      test_table_restorer;
  run_lwt "Fn restorer"         test_fn_restorer;
  run_lwt "Broken ref"          test_broken;
  run     "Broken ref 2"        test_broken2;
  run     "Broken ref 3"        test_broken3;
  run     "Broken ref 4"        test_broken4;
  run_lwt "Parallel connect"    test_parallel_connect;
  run_lwt "Parallel fails"      test_parallel_fails;
  run_lwt "Crossed calls"       test_crossed_calls;
  run_lwt "Store"               test_store;
]

let () =
  Alcotest_lwt.run ~and_exit:false "capnp-rpc" [
    "lwt", rpc_tests;
  ] |> Lwt_main.run
