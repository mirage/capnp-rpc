open Eio.Std
open Astring
open Testlib
open Capnp_rpc_lwt
open Capnp_rpc_net

module Test_utils = Testbed.Test_utils

module Vat = Capnp_rpc_unix.Vat
module CapTP = Capnp_rpc_unix.CapTP
module Tls_wrapper = Capnp_rpc_net.Tls_wrapper
module Exception = Capnp_rpc.Exception

exception Simulated_failure

let ( let/ ) x f = f (x ())
let ( and/ ) x y () = Fiber.pair x y

let _debug () =
  Logs.Src.set_level Capnp_rpc.Debug.src (Some Logs.Debug)

type cs = {
  client : Vat.t;
  server : Vat.t;
  client_key : Auth.Secret_key.t;
  server_key : Auth.Secret_key.t;
  serve_tls : bool;
}

let next_port = ref 8000

let get_test_address name =
  match Sys.os_type with
  | "Win32" ->
    (* No Unix-domain sockets on Windows *)
    let port = !next_port in
    incr next_port;
    `TCP ("127.0.0.1", port)
  | _ ->
    `Unix (Filename.(concat (Filename.get_temp_dir_name ())) name)

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

let () = Logs.(set_level (Some Logs.Warning))
let server_key = Auth.Secret_key.generate ()
let client_key = Auth.Secret_key.generate ()
let bad_key = Auth.Secret_key.generate ()
let () = Logs.(set_level (Some Logs.Info))

let server_pem = `PEM (Auth.Secret_key.to_pem_data server_key)

let make_vats_full ?(serve_tls=false) ?server_sw ~sw ~net ~restore () =
  let server =
    let sw = Option.value server_sw ~default:sw in
    let server_config =
      let addr = get_test_address "capnp-rpc-test-server" in
      Capnp_rpc_unix.Vat_config.create ~secret_key:server_pem ~serve_tls addr
    in
    Capnp_rpc_unix.serve ~sw ~net ~tags:Test_utils.server_tags ~restore server_config
  in
  {
    client = Vat.create ~sw ~tags:Test_utils.client_tags ~secret_key:(lazy client_key) net;
    server;
    client_key;
    server_key;
    serve_tls;
  }

let with_vats ?serve_tls ?server_sw ~net ~service fn =
  try
    Switch.run @@ fun sw ->
    let id = Restorer.Id.public "" in
    let restore = Restorer.single id service in
    Switch.on_release sw (fun () -> Capability.dec_ref service);
    fn @@ make_vats_full ?serve_tls ?server_sw ~sw ~net ~restore ();
    Logs.info (fun f -> f "Test finished; shutting down vats...");
    raise Exit  (* Stop vats *)
  with Exit -> ()

(* Generic running for Alcotest. *)
let run_eio ~net name ?(expected_warnings=0) fn =
  Alcotest.test_case name `Quick @@ fun () ->
  let warnings_at_start = Logs.(err_count () + warn_count ()) in
  Logs.info (fun f -> f "Start test-case");
  fn ~net;
  Gc.full_major ();
  let warnings_at_end = Logs.(err_count () + warn_count ()) in
  Alcotest.(check int) "Check log for warnings" expected_warnings (warnings_at_end - warnings_at_start)

let test_simple ~net ~serve_tls =
  with_vats ~net ~serve_tls ~service:(Echo.local ()) @@ fun cs ->
  let service = get_bootstrap cs in
  let reply = Echo.ping service "ping" in
  Alcotest.(check string) "Ping response" "got:0:ping" reply;
  Capability.dec_ref service

let test_bad_crypto ~net =
  with_vats ~net ~serve_tls:true ~service:(Echo.local ()) @@ fun cs ->
  let id = Restorer.Id.public "" in
  let uri = Vat.sturdy_uri cs.server id in
  let bad_digest = Auth.Secret_key.digest ~hash:`SHA256 bad_key in
  let uri = Auth.Digest.add_to_uri bad_digest uri in
  let sr = Capnp_rpc_unix.Vat.import_exn cs.client uri in
  let old_warnings = Logs.warn_count () in
  match Sturdy_ref.connect sr with
  | Ok _ -> Alcotest.fail "Wrong TLS key should have been rejected"
  | Error e ->
    let msg = Fmt.to_to_string Capnp_rpc.Exception.pp e in
    assert (String.is_prefix ~affix:"Failed: TLS connection failed: TLS failure: authentication failure" msg);
    Logs.info (fun f -> f "Wait for server to log warning...");
    while Logs.warn_count () = old_warnings do
      Fiber.yield ()
    done

let test_parallel ~net =
  with_vats ~net ~service:(Echo.local ()) @@ fun cs ->
  Switch.run @@ fun sw ->
  let service = get_bootstrap cs in
  let reply1 = Fiber.fork_promise ~sw (fun () -> Echo.ping service ~slow:true "ping1") in
  Echo.ping service "ping2" |> Alcotest.(check string) "Ping2 response" "got:1:ping2";
  assert (Promise.peek reply1 = None);
  Echo.unblock service;
  Promise.await_exn reply1 |> Alcotest.(check string) "Ping1 response" "got:0:ping1";
  Capability.dec_ref service

let test_registry ~net =
  Switch.run @@ fun sw ->
  let registry_impl = Registry.local ~sw () in
  with_vats ~net ~service:registry_impl @@ fun cs ->
  let registry = get_bootstrap cs in
  Capability.with_ref (Registry.echo_service registry) @@ fun echo_service ->
  Registry.unblock registry;
  Echo.ping echo_service "ping" |> Alcotest.(check string) "Ping response" "got:0:ping";
  Capability.dec_ref registry

let test_embargo ~net =
  Switch.run @@ fun sw ->
  let registry_impl = Registry.local ~sw () in
  let local_echo = Echo.local () in
  with_vats ~net ~service:registry_impl @@ fun cs ->
  let registry = get_bootstrap cs in
  Registry.set_echo_service registry local_echo;
  Capability.dec_ref local_echo;
  let echo_service = Registry.echo_service registry in
  let reply1 = Fiber.fork_promise ~sw (fun () -> Echo.ping echo_service "ping") in
  Registry.unblock registry;
  Promise.await_exn reply1 |> Alcotest.(check string) "Ping response" "got:0:ping";
  (* Flush, to ensure we resolve the echo_service's location. *)
  Echo.ping echo_service "ping" |> Alcotest.(check string) "Ping response" "got:1:ping";
  (* Test local connection. *)
  Echo.ping echo_service "ping" |> Alcotest.(check string) "Ping response" "got:2:ping";
  Capability.dec_ref echo_service;
  Capability.dec_ref registry

let test_resolve ~net =
  Switch.run @@ fun sw ->
  let registry_impl = Registry.local ~sw () in
  let local_echo = Echo.local () in
  with_vats ~net ~service:registry_impl @@ fun cs ->
  let registry = get_bootstrap cs in
  Registry.set_echo_service registry local_echo;
  Capability.dec_ref local_echo;
  let echo_service = Registry.echo_service_promise registry in
  let reply1 = Fiber.fork_promise ~sw (fun () -> Echo.ping echo_service "ping") in
  Registry.unblock registry;
  Promise.await_exn reply1 |> Alcotest.(check string) "Ping response" "got:0:ping";
  (* Flush, to ensure we resolve the echo_service's location. *)
  Echo.ping echo_service "ping" |> Alcotest.(check string) "Ping response" "got:1:ping";
  (* Test local connection. *)
  Echo.ping echo_service "ping" |> Alcotest.(check string) "Ping response" "got:2:ping";
  Capability.dec_ref echo_service;
  Capability.dec_ref registry

(* todo: we stop waiting and we send a finish message, but we don't currently
   abort the service operation. *)
let test_cancel ~net =
  with_vats ~net ~service:(Echo.local ()) @@ fun cs ->
  let service = get_bootstrap cs in
  Fiber.first
    (fun () ->
      ignore (Echo.ping service ~slow:true "ping1" : string);
      assert false
    )
    (fun () ->
      Echo.ping service "ping" |> Alcotest.(check string) "Ping response" "got:1:ping"
    );
  Echo.unblock service;
  Echo.ping service "ping" |> Alcotest.(check string) "Ping response" "got:2:ping";
  Capability.dec_ref service

let float = Alcotest.testable Fmt.float (=)

let test_calculator ~net =
  let open Calc in
  Switch.run @@ fun sw ->
  let service = Calc.local ~sw in
  with_vats ~net ~service @@ fun cs ->
  let c = get_bootstrap cs in
  Calc.evaluate c (Float 1.) |> Value.final_read |> Alcotest.check float "Simple calc" 1.;
  let local_add = Calc.Fn.add in
  let expr = Expr.(Call (local_add, [Float 1.; Float 2.])) in
  Calc.evaluate c expr |> Value.final_read |> Alcotest.check float "Complex with local fn" 3.;
  let remote_add = Calc.getOperator c `Add in
  Calc.Fn.call remote_add [5.; 3.] |> Alcotest.check float "Check fn" 8.;
  let expr = Expr.(Call (remote_add, [Float 1.; Float 2.])) in
  Calc.evaluate c expr |> Value.final_read |> Alcotest.check float "Complex with remote fn" 3.;
  Capability.dec_ref remote_add;
  Capability.dec_ref c

let test_calculator2 ~net =
  let open Calc in
  Switch.run @@ fun sw ->
  let service = Calc.local ~sw in
  with_vats ~net ~service @@ fun cs ->
  let c = get_bootstrap cs in
  let remote_add = Calc.getOperator c `Add in
  let remote_mul = Calc.getOperator c `Multiply in
  let expr = Expr.(Call (remote_mul, [Float 4.; Float 6.])) in
  let result = Calc.evaluate c expr in
  let/ add3 () =
    let expr = Expr.(Call (remote_add, [Prev result; Float 3.])) in
    Calc.evaluate c expr |> Value.final_read
  and/ add5 () =
    let expr = Expr.(Call (remote_add, [Prev result; Float 5.])) in
    Calc.evaluate c expr |> Value.final_read
  in
  Alcotest.check float "First" 27.0 add3;
  Alcotest.check float "Second" 29.0 add5;
  Capability.dec_ref result;
  Capability.dec_ref remote_add;
  Capability.dec_ref remote_mul;
  Capability.dec_ref c

let test_indexing ~net =
  Switch.run @@ fun sw ->
  let registry_impl = Registry.local ~sw () in
  with_vats ~net ~service:registry_impl @@ fun cs ->
  let registry = get_bootstrap cs in
  let echo_service, version = Registry.complex registry in
  Echo.ping echo_service "ping" |> Alcotest.(check string) "Ping response" "got:0:ping";
  Registry.Version.read version |> Alcotest.(check string) "Version response" "0.1";
  Capability.dec_ref registry;
  Capability.dec_ref echo_service;
  Capability.dec_ref version

let cmd_result t =
  let pp f (x : ('a Cmdliner.Cmd.eval_ok, Cmdliner.Cmd.eval_error) result) =
    match x with
    | Ok (`Help) -> Fmt.string f "help"
    | Ok (`Version) -> Fmt.string f "version"
    | Ok (`Ok x) -> Alcotest.pp t f x
    | _ -> Fmt.string f "error"
  in
  let equal a b =
    match a, b with
    | Ok (`Ok a), Ok (`Ok b) -> Alcotest.equal t a b
    | _ -> a = b
  in
  Alcotest.testable pp equal

let vat_config = Alcotest.testable Capnp_rpc_unix.Vat_config.pp Capnp_rpc_unix.Vat_config.equal

let config_result = cmd_result vat_config

let test_options () =
  let term = Cmdliner.Cmd.(v (info "main") Capnp_rpc_unix.Vat_config.cmd) in
  let config = Cmdliner.Cmd.eval_value
      ~argv:[| "main"; "--capnp-secret-key-file=key.pem"; "--capnp-listen-address"; "unix:/run/socket" |] term in
  let expected =
    Result.ok (`Ok (Capnp_rpc_unix.Vat_config.create
                      ~secret_key:(`File "key.pem")
                      (`Unix "/run/socket")))
  in
  Alcotest.check config_result "Unix, same address" expected config;
  let expected =
    Result.ok (`Ok (Capnp_rpc_unix.Vat_config.create
                      ~secret_key:(`File "key.pem")
                      ~public_address:(`TCP ("1.2.3.4", 7001))
                      (`TCP ("0.0.0.0", 7000))))
  in
  Cmdliner.Cmd.eval_value ~argv:[| "main";
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

let test_sturdy_self ~net =
  let service = Echo.local () in
  Capability.inc_ref service;
  with_vats ~net ~serve_tls:true ~service @@ fun cs ->
  let id = Restorer.Id.public "" in
  let sr = Vat.sturdy_uri cs.server id |> Vat.import_exn cs.server in
  let service2 = Sturdy_ref.connect_exn sr in
  Alcotest.check cap "Restore from same vat" service service2;
  Capability.dec_ref service2;
  Capability.dec_ref service

let expect_non_exn = function
  | Ok x -> x
  | Error ex -> Alcotest.failf "expect_non_exn: %a" Capnp_rpc.Exception.pp ex

let except = Alcotest.testable Capnp_rpc.Exception.pp (=)
let except_ty = Alcotest.testable Capnp_rpc.Exception.pp_ty (=)

let test_table_restorer ~net:_ =
  Switch.run @@ fun sw ->
  let make_sturdy id = Uri.make ~path:(Restorer.Id.to_string id) () in
  let table = Restorer.Table.create make_sturdy in
  let echo_id = Restorer.Id.public "echo" in
  let registry_id = Restorer.Id.public "registry" in
  let broken_id = Restorer.Id.public "broken" in
  let unknown_id = Restorer.Id.public "unknown" in
  Restorer.Table.add table echo_id @@ Echo.local ();
  Restorer.Table.add table registry_id @@ Registry.local ~sw ();
  Restorer.Table.add table broken_id @@ Capability.broken (Capnp_rpc.Exception.v "broken");
  let r = Restorer.of_table table in
  let a1 = Restorer.restore r echo_id |> expect_non_exn in
  let reply = Echo.ping a1 "ping" in
  Alcotest.(check string) "Ping response" "got:0:ping" reply;
  let a2 = Restorer.restore r echo_id |> expect_non_exn in
  Alcotest.check cap "Same cap" a1 a2;
  let r1 = Restorer.restore r registry_id |> expect_non_exn in
  assert (a1 <> r1);
  let x = Restorer.restore r broken_id |> expect_non_exn in
  let expected = Some (Capnp_rpc.Exception.v "broken") in
  Alcotest.(check (option except)) "Broken response" expected (Capability.problem x);
  let x = Restorer.restore r unknown_id in
  let expected = Error (Capnp_rpc.Exception.v "Unknown persistent service ID") in
  Alcotest.(check (result reject except)) "Missing mapping" expected x;
  Capability.dec_ref a1;
  Capability.dec_ref a2;
  Capability.dec_ref r1;
  Restorer.Table.remove table echo_id;
  Restorer.Table.clear table

module Loader = struct
  type t = string -> Restorer.resolution

  let hash _ = `SHA256
  let make_sturdy _ id = Uri.make ~path:(Restorer.Id.to_string id) ()
  let load t _sr digest = t digest
end

let test_fn_restorer ~net:_ =
  Switch.run @@ fun sw ->
  let cap = Alcotest.testable Capability.pp (=) in
  let a = Restorer.Id.public "a" in
  let b = Restorer.Id.public "b" in
  let c = Restorer.Id.public "c" in
  let current_c = ref (Restorer.reject (Exception.v "Broken C")) in
  let delay = Eio.Condition.create () in
  let digest = Restorer.Id.digest (Loader.hash ()) in
  let load d =
    if d = digest a then Restorer.grant @@ Echo.local ()
    else if d = digest b then (Eio.Condition.await_no_mutex delay; Restorer.grant @@ Echo.local ())
    else if d = digest c then (Eio.Condition.await_no_mutex delay; !current_c)
    else Restorer.unknown_service_id
  in
  let table = Restorer.Table.of_loader ~sw (module Loader) load in
  let restorer = Restorer.of_table table in
  let restore x = Restorer.restore restorer x in
  (* Check that restoring the same ID twice caches the capability. *)
  let a1 = restore a |> expect_non_exn in
  let a2 = restore a |> expect_non_exn in
  Alcotest.check cap "Restore cached" a1 a2;
  Capability.dec_ref a1;
  Capability.dec_ref a2;
  (* But if it's released, the next lookup loads a fresh one. *)
  let a3 = restore a |> expect_non_exn in
  if a1 = a3 then Alcotest.fail "Returned released cap!";
  Capability.dec_ref a3;
  (* Doing two lookups in parallel only does one load. *)
  let b1 = Fiber.fork_promise ~sw (fun () -> restore b) in
  let b2 = Fiber.fork_promise ~sw (fun () -> restore b) in
  assert (Promise.peek b1 = None);
  Eio.Condition.broadcast delay;
  let b1 = Promise.await_exn b1 |> expect_non_exn in
  let b2 = Promise.await_exn b2 |> expect_non_exn in
  Alcotest.check cap "Restore delayed cached" b1 b2;
  Restorer.Table.clear table;   (* (should have no effect) *)
  Capability.dec_ref b1;
  Capability.dec_ref b2;
  (* Failed lookups aren't cached. *)
  let c1 = Fiber.fork_promise ~sw (fun () -> restore c) in
  Eio.Condition.broadcast delay;
  let c1 = Promise.await_exn c1 in
  let reject = Alcotest.result cap except in
  Alcotest.check reject "C initially fails" (Error (Exception.v "Broken C")) c1;
  let c2 = Fiber.fork_promise ~sw (fun () -> restore c) in
  let c_service = Echo.local () in
  current_c := Restorer.grant c_service;
  Eio.Condition.broadcast delay;
  let c2 = Promise.await_exn c2 |> expect_non_exn in
  Alcotest.check cap "C now works" c_service c2;
  Capability.dec_ref c2;
  (* Two users; one frees the cap immediately *)
  let b1 =
    Fiber.fork_promise ~sw @@ fun () ->
    restore b |> expect_non_exn |> fun b1 ->
    Capability.dec_ref b1;
    b1
  in
  let b2 = Fiber.fork_promise ~sw (fun () -> restore b) in
  Eio.Condition.broadcast delay;
  let b1 = Promise.await_exn b1 in
  let b2 = Promise.await_exn b2 |> expect_non_exn in
  Alcotest.check cap "Cap not freed" b1 b2;
  Capability.dec_ref b2

let test_broken ~net =
  try
    Switch.run (fun server_sw ->
        with_vats ~server_sw ~net ~service:(Echo.local ()) @@ fun cs ->
        let service = get_bootstrap cs in
        Echo.ping service "ping" |> Alcotest.(check string) "Ping response" "got:0:ping";
        let problem, set_problem = Promise.create () in
        Capability.when_broken (fun x -> Promise.resolve set_problem x) service;
        Alcotest.check (Alcotest.option except) "Still OK" None @@ Capability.problem service;
        assert (Promise.peek problem = None);
        Logs.info (fun f -> f "Turning off server...");
        Switch.fail server_sw Simulated_failure;
        let problem = Promise.await problem in
        Alcotest.check except_ty "Broken callback ran" `Disconnected problem.ty;
        assert (Capability.problem service <> None);
        try
          ignore (Echo.ping service "ping" : string);
          Alcotest.fail "Should have failed!"
        with Failure _ ->
          Capability.dec_ref service
      )
  with Simulated_failure -> ()

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

let test_parallel_connect ~net =
  with_vats ~net ~serve_tls:true ~service:(Echo.local ()) @@ fun cs ->
  let/ service () = get_bootstrap cs
  and/ service2 () = get_bootstrap cs in
  Capability.await_settled_exn service;
  Capability.await_settled_exn service2;
  Alcotest.check cap "Shared connection" service service2;
  Capability.dec_ref service;
  Capability.dec_ref service2

let test_parallel_fails ~net =
  try
    Switch.run (fun server_sw ->
        with_vats ~net ~server_sw ~serve_tls:true ~service:(Echo.local ()) @@ fun cs ->
        let/ service () = get_bootstrap cs
        and/ service2 () = get_bootstrap cs in
        Switch.fail server_sw Simulated_failure;
        let p, r = Promise.create () in
        Capability.when_broken (Promise.resolve r) service2;
        ignore (Promise.await p : Exception.t);
        Alcotest.check cap "Shared failure" service service2;
        Capability.dec_ref service;
        Capability.dec_ref service2;
        (* Restart server (ignore new client) *)
        Fiber.yield ();
        with_vats ~net ~serve_tls:true ~service:(Echo.local ()) @@ fun cs ->
        let service = get_bootstrap cs in
        Echo.ping service "ping" |> Alcotest.(check string) "Ping response" "got:0:ping";
        Capability.dec_ref service
      )
  with Simulated_failure -> ()

let test_crossed_calls ~net =
  (* Would be good to control the ordering here, to test the various cases.
     Currently, it's not certain which path is actually tested. *)
  try
    Switch.run @@ fun sw ->
    let id = Restorer.Id.public "" in
    let make_vat ~secret_key ~tags addr =
      let service = Echo.local () in
      let restore = Restorer.(single id) service in
      let config =
        let secret_key = `PEM (Auth.Secret_key.to_pem_data secret_key) in
        let name = Fmt.str "capnp-rpc-test-%s" addr in
        Capnp_rpc_unix.Vat_config.create ~secret_key (get_test_address name)
      in
      let vat = Capnp_rpc_unix.serve ~net ~sw ~tags ~restore config in
      Switch.on_release sw (fun () -> Capability.dec_ref service);
      vat
    in
    let client = make_vat ~secret_key:client_key ~tags:Test_utils.client_tags "client" in
    let server = make_vat ~secret_key:server_key ~tags:Test_utils.server_tags "server" in
    let sr_to_client = Capnp_rpc_unix.Vat.sturdy_uri client id |> Vat.import_exn server in
    let sr_to_server = Capnp_rpc_unix.Vat.sturdy_uri server id |> Vat.import_exn client in
    let/ to_client () = Sturdy_ref.connect_exn sr_to_client
    and/ to_server () = Sturdy_ref.connect_exn sr_to_server in
    Logs.info (fun f -> f ~tags:Test_utils.client_tags "%a" Capnp_rpc_unix.Vat.dump client);
    Logs.info (fun f -> f ~tags:Test_utils.server_tags "%a" Capnp_rpc_unix.Vat.dump server);
    let/ s_got () = Echo.ping_result to_client "ping"
    and/ c_got () = Echo.ping_result to_server "ping" in
    let c_got, s_got =
      match c_got, s_got with
      | Ok x, Ok y -> (x, y)
      | Ok x, Error _ ->
        (* Server got an error. Try client again. *)
        let to_client = Sturdy_ref.connect_exn sr_to_client in
        Capability.with_ref to_client @@ fun to_client ->
        Echo.ping to_client "ping" |> fun s_got -> (x, s_got)
      | Error _, Ok y ->
        (* Client got an error. Try server again. *)
        let to_server = Sturdy_ref.connect_exn sr_to_server in
        Capability.with_ref to_server @@ fun to_server ->
        Echo.ping to_server "ping" |> fun c_got -> (c_got, y)
      | Error (`Capnp e1), Error (`Capnp e2) ->
        Fmt.failwith "@[<v>Both connections failed!@,%a@,%a@]"
          Capnp_rpc.Error.pp e1
          Capnp_rpc.Error.pp e2
    in
    Alcotest.(check string) "Client's ping response" "got:0:ping" c_got;
    Alcotest.(check string) "Server's ping response" "got:0:ping" s_got;
    Capability.dec_ref to_client;
    Capability.dec_ref to_server;
    raise Simulated_failure
  with Simulated_failure -> ()

(* Run test_crossed_calls several times to try to trigger the various behaviours. *)
let test_crossed_calls ~net =
  for _ = 1 to 10 do
    test_crossed_calls ~net
  done

let test_store ~net =
  try
    Switch.run @@ fun sw ->
    (* Persistent server configuration *)
    let db = Store.DB.create () in
    let config =
      let addr = get_test_address "capnp-rpc-test-server" in
      Capnp_rpc_unix.Vat_config.create ~secret_key:server_pem addr
    in
    let main_id = Restorer.Id.generate () in
    let start_server ~sw () =
      let make_sturdy = Capnp_rpc_unix.Vat_config.sturdy_uri config in
      let table = Store.File.table ~sw ~make_sturdy db in
      Switch.on_release sw (fun () -> Restorer.Table.clear table);
      let restore = Restorer.of_table table in
      let service = Store.local ~restore db in
      Restorer.Table.add table main_id service;
      Capnp_rpc_unix.serve ~sw ~net ~restore ~tags:Test_utils.server_tags config
    in
    (* Start server *)
    let file, file_sr =
      Switch.run (fun server_switch ->
          let server = start_server ~sw:server_switch () in
          let store_uri = Capnp_rpc_unix.Vat.sturdy_uri server main_id in
          (* Set up client *)
          let client = Capnp_rpc_unix.client_only_vat ~tags:Test_utils.client_tags ~sw net in
          let sr = Capnp_rpc_unix.Vat.import_exn client store_uri in
          Sturdy_ref.with_cap_exn sr @@ fun store ->
          (* Try creating a file *)
          let file = Store.create_file store in
          Store.File.set file "Hello";
          let file_sr = Persistence.save_exn file in
          let file_sr = Vat.import_exn client file_sr in (* todo: get rid of this step *)
          (* Shut down server *)
          Switch.fail server_switch Simulated_failure;
          file, file_sr
        )
    in
    let broken, set_broken = Promise.create () in
    Capability.when_broken (Promise.resolve set_broken) file;
    ignore (Promise.await broken : Exception.t);
    assert (Capability.problem file <> None);
    (* Restart server *)
    let _server = start_server ~sw () in
    (* Reconnect client *)
    Sturdy_ref.with_cap_exn file_sr @@ fun file ->
    let data = Store.File.get file in
    Alcotest.(check string) "Read file" "Hello" data
  with Simulated_failure -> ()

let ( / ) = Eio.Path.( / )

let rmtree dir =
  Eio.Path.read_dir dir
  |> List.iter (fun leaf ->
      let path = dir / leaf in
      traceln "rm %a" Eio.Path.pp path;
      Eio.Path.unlink path
    );
  traceln "rmdir %a" Eio.Path.pp dir;
  Eio.Path.rmdir dir;
  traceln "Removed"

let with_temp_dir path fn =
  Eio.Path.mkdir path ~perm:0o700;
  Fun.protect (fun () -> Eio.Path.with_open_dir path fn)
    ~finally:(fun () -> rmtree path)

let test_file_store ~dir ~net:_ =
  with_temp_dir (dir / "capnp-tests") @@ fun tmpdir ->
  let module S = Capnp_rpc_unix.File_store in
  let s = S.create tmpdir in
  Alcotest.(check (option reject)) "Missing file" None @@ S.load s ~digest:"missing";
  let module Builder = Testlib.Api.Builder.Simple in
  let module Reader = Testlib.Api.Reader.Simple in
  let data =
    let b = Builder.init_root () in
    Builder.text_set b "Test";
    Builder.to_reader b
  in
  S.save s ~digest:"!/.." data;
  Alcotest.(check (option string)) "Restored" (Some "Test") @@ Option.map Reader.text_get (S.load s ~digest:"!/..")

let capnp_error = Alcotest.of_pp Capnp_rpc.Exception.pp

let test_await_settled ~net:_ =
  (* Ok *)
  Switch.run @@ fun sw ->
  let p, r = Capability.promise () in
  let check = Fiber.fork_promise ~sw (fun () -> Capability.await_settled p) in
  Capability.resolve_ok r @@ Echo.local ();
  let check = Promise.await_exn check in
  Alcotest.(check (result unit capnp_error)) "Check await success" (Ok ()) check;
  Capability.dec_ref p;
  (* Error *)
  let p, r = Capability.promise () in
  let check = Fiber.fork_promise ~sw (fun () -> Capability.await_settled p) in
  let err = Capnp_rpc.Exception.v "Test" in
  Capability.resolve_exn r err;
  let check = Promise.await_exn check in
  Alcotest.(check (result unit capnp_error)) "Check await failure" (Error err) check

(* The client disconnects before the server has finished loading the bootstrap object. *)
let test_late_bootstrap ~net =
  try
    Switch.run @@ fun server_sw ->
    Switch.run @@ fun client_switch ->
    let connected, set_connected = Promise.create () in
    let service, set_service = Promise.create () in
    let module Loader = struct
      type t = unit
      let hash () = `SHA256
      let make_sturdy () _id = assert false
      let load () _sr _name =
        Promise.resolve set_connected ();
        Promise.await service
    end in
    let table = Capnp_rpc_net.Restorer.Table.of_loader ~sw:server_sw (module Loader) () in
    let restore = Restorer.of_table table in
    let cs = make_vats_full ~sw:client_switch ~server_sw ~restore ~net () in
    let service = get_bootstrap cs in
    Promise.await connected;
    Eio.Cancel.protect @@ fun () ->
    Switch.fail client_switch Simulated_failure;
    Promise.resolve set_service @@ Capnp_rpc_net.Restorer.grant @@ Echo.local ();
    let service = Capability.await_settled service |> Result.get_error in
    Logs.info (fun f -> f "client got: %a" Capnp_rpc.Exception.pp service);
    assert (service.Capnp_rpc.Exception.ty = `Disconnected);
    (* The restorer yields once before returning the cap,
       so we wait too, to ensure it's done. *)
    Fiber.yield ()
  with Simulated_failure -> ()

let run name fn = Alcotest.test_case name `Quick fn

let rpc_tests ~net ~dir =
  let net = Capnp_rpc_unix.Network.v net in
  let run_eio = run_eio ~net in
  [
    run_eio "Simple"              (test_simple ~serve_tls:false);
    run_eio "Crypto"              (test_simple ~serve_tls:true);
    run_eio "Bad crypto"          test_bad_crypto ~expected_warnings:1;
    run_eio "Parallel"            test_parallel;
    run_eio "Embargo"             test_embargo;
    run_eio "Resolve"             test_resolve;
    run_eio "Registry"            test_registry;
    run_eio "Calculator"          test_calculator;
    run_eio "Calculator 2"        test_calculator2;
    run_eio "Cancel"              test_cancel;
    run_eio "Indexing"            test_indexing;
    run     "Options"             test_options;
    run     "Sturdy URI"          test_sturdy_uri;
    run_eio "Sturdy self"         test_sturdy_self;
    run_eio "Table restorer"      test_table_restorer;
    run_eio "Fn restorer"         test_fn_restorer;
    run_eio "Broken ref"          test_broken;
    run     "Broken ref 2"        test_broken2;
    run     "Broken ref 3"        test_broken3;
    run     "Broken ref 4"        test_broken4;
    run_eio "Parallel connect"    test_parallel_connect;
    run_eio "Parallel fails"      test_parallel_fails;
    run_eio "Crossed calls"       test_crossed_calls;
    run_eio "Store"               test_store;
    run_eio "File store"          (test_file_store ~dir);
    run_eio "Await settled"       test_await_settled;
    run_eio "Late bootstrap"      test_late_bootstrap;
  ]

let () =
  Eio_main.run @@ fun env ->
  (* Eio_unix.Ctf.with_tracing "/tmp/trace.ctf" @@ fun () -> *)
  Alcotest.run ~and_exit:false "capnp-rpc" [
    "eio", rpc_tests ~net:env#net ~dir:env#cwd;
  ]
