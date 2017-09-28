open Lwt.Infix
open Capnp_rpc_lwt
open Examples

module Time = struct
  type 'a io = 'a Lwt.t
  let sleep_ns ns = Lwt_unix.sleep (Duration.to_f ns)

  type t = unit

  let period_ns () = None
  let elapsed_ns () = 0L
  let disconnect () = Lwt.return_unit
end

module Random = struct
  type g = unit
  type buffer = Cstruct.t

  let generate ?g n = ignore g; Cstruct.create n
end

module Stack = struct
  module B = Basic_backend.Make
  module V = Vnetif.Make(B)
  module E = Ethif.Make(V)
  module A = Arpv4.Make(E)(Time)(Time)
  module I = Static_ipv4.Make(E)(A)
  module U = Udp.Make(I)(Random)
  module T = Tcp.Flow.Make(I)(Time)(Time)(Random)
  module Icmp = Icmpv4.Make(I)
  include Tcpip_stack_direct.Make(Time)(Random)(V)(E)(A)(I)(Icmp)(U)(T)

  let create_network () = B.create ~use_async_readers:true ~yield:Lwt_unix.yield ()

  let create_interface backend ip =
    V.connect backend >>= fun v ->
    E.connect v >>= fun e ->
    A.connect e () >>= fun a ->
    I.connect ~ip ~network:Ipaddr.V4.Prefix.private_10 e a >>= fun i ->
    U.connect i >>= fun u ->
    T.connect i () >>= fun t ->
    Icmp.connect i >>= fun icmp ->
    let config = { Mirage_stack_lwt.name = "test-stack"; interface = v } in
    connect config e a i icmp u t
end
module Mirage = Capnp_rpc_mirage.Make(Stack)(Dns_resolver_mirage.Static)
module Vat = Mirage.Vat

type cs = {
  client : Vat.t;
  server : Vat.t;
  client_key : Auth.Secret_key.t;
  server_key : Auth.Secret_key.t;
  serve_tls : bool;
  server_switch : Lwt_switch.t;
}

(* Have the client ask the server for its bootstrap object, and return the
   resulting client-side proxy to it. *)
let get_bootstrap cs =
  let id = Restorer.Id.public "" in
  let sr = Vat.sturdy_uri cs.server id |> Vat.import_exn cs.client in
  Sturdy_ref.connect_exn sr

let dns =
  let names = Hashtbl.create 7 in
  let rev = Hashtbl.create 7 in
  let add name ip =
    Hashtbl.add names name (Ipaddr.of_string_exn ip);
    Hashtbl.add rev (Ipaddr.V4.of_string_exn ip) name
  in
  add "server" "10.0.0.1";
  add "client" "10.0.0.2";
  let dns_entries = {Dns_resolver_mirage.names; rev} in
  Dns_resolver_mirage.Static.create dns_entries

let create_iface network ip =
  Stack.create_interface network (Ipaddr.V4.of_string_exn ip) >|= fun stack ->
  Mirage.network ~dns stack

let () = Nocrypto_entropy_unix.initialize ()
let server_key = Auth.Secret_key.generate ()
let client_key = Auth.Secret_key.generate ()

let server_pem = `PEM (Auth.Secret_key.to_pem_data server_key)

let make_vats ?(serve_tls=false) ~switch ~service () =
  let id = Restorer.Id.public "" in
  let restore = Restorer.single id service in
  let server_config =
    Mirage.Vat_config.create ~secret_key:server_pem ~serve_tls ~public_address:(`TCP ("server", 7000)) (`TCP 7000)
  in
  let net = Stack.create_network () in
  create_iface net "10.0.0.1" >>= fun server_net ->
  create_iface net "10.0.0.2" >>= fun client_net ->
  let server_switch = Lwt_switch.create () in
  Mirage.serve ~switch:server_switch ~tags:Testbed.Test_utils.server_tags ~restore server_net server_config >>= fun server ->
  Lwt_switch.add_hook (Some switch) (fun () -> Lwt_switch.turn_off server_switch);
  Lwt_switch.add_hook (Some switch) (fun () -> Capability.dec_ref service; Lwt.return_unit);
  Lwt.return {
    client = Vat.create ~switch ~tags:Testbed.Test_utils.client_tags ~secret_key:(lazy client_key) client_net;
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

let rpc_tests = [
  run_lwt "Simple" (test_simple ~serve_tls:false);
  run_lwt "TLS"    (test_simple ~serve_tls:true);
]

let () =
  Alcotest.run ~and_exit:false "capnp-rpc" [
    "mirage", rpc_tests;
  ]
