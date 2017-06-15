module Core_types = Testbed.Capnp_direct.Core_types
module Test_utils = Testbed.Test_utils
module Services = Testbed.Services
module CS = Testbed.Connection.Make ( )    (* A client-server pair *)
module RO_array = Capnp_rpc.RO_array

let empty = RO_array.empty

let error = Alcotest.of_pp Capnp_rpc.Error.pp
let pp_cap f p = p#pp f
let cap : Core_types.cap Alcotest.testable = Alcotest.of_pp pp_cap
let ro_array x = Alcotest.testable (RO_array.pp (Alcotest.pp x)) (RO_array.equal (Alcotest.equal x))
let response_promise = Alcotest.(option (result (pair string (ro_array cap)) error))

let test_simple_connection () =
  let open CS in
  let c, s = create ~client_tags:Test_utils.client_tags ~server_tags:Test_utils.server_tags (Services.echo_service ()) in
  let servce_promise = C.bootstrap c in
  S.handle_msg s ~expect:"bootstrap";
  C.handle_msg c ~expect:"return:(boot)";
  S.handle_msg s ~expect:"finish";
  let q = servce_promise#call "my-content" empty in
  S.handle_msg s ~expect:"call:my-content";
  C.handle_msg c ~expect:"return:got:my-content";
  Alcotest.(check response_promise) "Client got call response" (Some (Ok ("got:my-content", empty))) q#response;
  servce_promise#dec_ref;
  CS.flush c s;
  CS.check_finished c s

let init_pair ~bootstrap_service =
  let open CS in
  let c, s = create ~client_tags:Test_utils.client_tags ~server_tags:Test_utils.server_tags bootstrap_service in
  let bs = C.bootstrap c in
  S.handle_msg s ~expect:"bootstrap";
  C.handle_msg c ~expect:"return:(boot)";
  S.handle_msg s ~expect:"finish";
  c, s, bs

(* The server gets an object and then sends it back. When the object arrives back
   at the client, it must be the original (local) object, not a proxy. *)
let test_return () =
  let open CS in
  let c, s, bs = init_pair ~bootstrap_service:(Services.echo_service ()) in
  (* Pass callback *)
  let slot = ref ("empty", empty) in
  let local = Services.swap_service slot in
  let q = bs#call "c1" (RO_array.of_list [local]) in
  (* Server echos args back *)
  S.handle_msg s ~expect:"call:c1";
  C.handle_msg c ~expect:"return:got:c1";
  Alcotest.(check response_promise) "Client got response" (Some (Ok ("got:c1", RO_array.of_list [local]))) q#response;
  bs#dec_ref;
  S.handle_msg s ~expect:"finish";
  S.handle_msg s ~expect:"release";
  C.handle_msg c ~expect:"release";
  CS.check_finished c s

let test_return_error () =
  let c, s, bs = init_pair ~bootstrap_service:(Core_types.broken_cap "test-error") in
  (* Pass callback *)
  let slot = ref ("empty", empty) in
  let local = Services.swap_service slot in
  let q = bs#call "call" (RO_array.of_list [local]) in
  (* Server echos args back *)
  CS.flush c s;
  Alcotest.(check response_promise) "Client got response" (Some (Error (`Exception "test-error"))) q#response;
  q#finish;
  bs#dec_ref;
  CS.flush c s;
  CS.check_finished c s

let call target msg caps =
  List.iter (fun c -> c#inc_ref) caps;
  target#call msg (RO_array.of_list caps)

let test_share_cap () =
  let open CS in
  let c, s, bs = init_pair ~bootstrap_service:(Services.echo_service ()) in
  let q = call bs "msg" [bs; bs] in
  bs#dec_ref;
  S.handle_msg s ~expect:"call:msg";
  S.handle_msg s ~expect:"release";       (* Server drops [bs] export *)
  (* Server re-exports [bs] as result of echo *)
  C.handle_msg c ~expect:"return:got:msg";
  q#finish;
  CS.flush c s;
  CS.check_finished c s

(* The server gets an object and then sends it back. Messages pipelined to
   the object must arrive before ones sent directly. *)
let test_local_embargo () =
  let open CS in
  let c, s, bs = init_pair ~bootstrap_service:(Services.echo_service ()) in
  let local = Services.logger () in
  let q = call bs "Get service" [(local :> Core_types.cap)] in
  let service = q#cap 0 in
  let _ = service#call "Message-1" empty in
  S.handle_msg s ~expect:"call:Get service";
  C.handle_msg c ~expect:"return:got:Get service";
  (* We've received the bootstrap reply, so we know that [service] is local,
     but the pipelined message we sent to it via [s] hasn't arrived yet. *)
  let _ = service#call "Message-2" empty in
  S.handle_msg s ~expect:"call:Message-1";
  C.handle_msg c ~expect:"call:Message-1";            (* Gets pipelined message back *)
  S.handle_msg s ~expect:"disembargo-request";
  C.handle_msg c ~expect:"disembargo-reply";
  Alcotest.(check string) "Pipelined arrived first" "Message-1" local#pop;
  Alcotest.(check string) "Embargoed arrived second" "Message-2" local#pop;
  (* Clean up *)
  q#finish;
  bs#dec_ref;
  service#dec_ref;
  CS.flush c s;
  CS.check_finished c s

let cap x = (x :> Core_types.cap)

(* As above, but this time it resolves to a promised answer. *)
let test_local_embargo_2 () =
  let open CS in
  let server_main = Services.manual () in
  let c, s, bs = init_pair ~bootstrap_service:(cap server_main) in
  let local = Services.logger () in
  let local_reg = Services.manual () in    (* A registry that provides access to [local]. *)
  let q1 = call bs "q1" [cap local_reg] in (* Give the server our registry and get back [local]. *)
  let service = q1#cap 0 in                (* Service is a promise for local *)
  q1#finish;
  let _ = service#call "Message-1" empty in             (* First message to service *)
  S.handle_msg s ~expect:"call:q1";
  let (_, q1_args, a1) = server_main#pop in
  let proxy_to_local_reg = RO_array.get q1_args 0 in
  (* The server will now make a call on the client registry, and then tell the client
     to use the (unknown) result of that for [service]. *)
  let q2 = call proxy_to_local_reg "q2" [] in
  proxy_to_local_reg#dec_ref;
  let proxy_to_local = q2#cap 0 in
  a1#resolve (Ok ("a1", RO_array.of_list [proxy_to_local]));
  (* [proxy_to_local] is now owned by [a1]. *)
  q2#finish;
  C.handle_msg c ~expect:"call:q2";
  let (_, _q2_args, a2) = local_reg#pop in
  C.handle_msg c ~expect:"release";
  C.handle_msg c ~expect:"return:a1";
  (* The client now knows that [a1/0] is a local promise, but it can't use it directly yet because
     of the pipelined messages. It sends a disembargo request down the old [q1/0] path and waits for
     it to arrive back at the local promise. *)
  a2#resolve (Ok ("a2", RO_array.of_list [cap local]));
  (* Message-2 must be embargoed so that it arrives after Message-1. *)
  let _ = service#call "Message-2" empty in
  S.handle_msg s ~expect:"call:Message-1";
  C.handle_msg c ~expect:"call:Message-1";            (* Gets pipelined message back *)
  S.handle_msg s ~expect:"disembargo-request";
  C.handle_msg c ~expect:"disembargo-reply";
  Alcotest.(check string) "Pipelined arrived first" "Message-1" local#pop;
  Alcotest.(check string) "Embargoed arrived second" "Message-2" local#pop;
  (* Clean up *)
  bs#dec_ref;
  service#dec_ref;
  CS.flush c s;
  CS.check_finished c s

(* The field must still be useable after the struct is released. *)
let test_fields () =
  let open CS in
  let c, s = create ~client_tags:Test_utils.client_tags ~server_tags:Test_utils.server_tags (Services.echo_service ()) in
  let f0 = C.bootstrap c in
  let q1 = call f0 "c1" [] in
  S.handle_msg s ~expect:"bootstrap";
  C.handle_msg c ~expect:"return:(boot)";      (* [bs] resolves *)
  S.handle_msg s ~expect:"call:c1";
  S.handle_msg s ~expect:"finish";
  C.handle_msg c ~expect:"return:got:c1";
  Alcotest.(check response_promise) "Echo response" (Some (Ok ("got:c1", empty))) q1#response;
  q1#finish;
  let q2 = call f0 "c2" [] in
  CS.flush c s;
  Alcotest.(check response_promise) "Echo response 2" (Some (Ok ("got:c2", empty))) q2#response;
  f0#dec_ref;
  CS.flush c s;
  CS.check_finished c s

let test_cancel () =
  let open CS in
  let service = Services.manual () in
  let c, s = create ~client_tags:Test_utils.client_tags ~server_tags:Test_utils.server_tags
      (service :> Core_types.cap) in
  let f0 = C.bootstrap c in
  let q1 = call f0 "c1" [] in
  let prom = q1#cap 0 in
  q1#finish;    (* Client doesn't cancel q1 because we're using prom *)
  let _q2 = call prom "p1" [] in
  S.handle_msg s ~expect:"bootstrap";
  C.handle_msg c ~expect:"return:(boot)";      (* [bs] resolves *)
  S.handle_msg s ~expect:"call:c1";
  S.handle_msg s ~expect:"call:p1";
  S.handle_msg s ~expect:"finish";      (* bootstrap *)
  let (_, _, a1) = service#pop in
  a1#resolve (Ok ("a1", empty));
  C.handle_msg c ~expect:"return:Invalid cap index 0 in []";
  C.handle_msg c ~expect:"return:a1";
  f0#dec_ref;
  CS.flush c s;
  CS.check_finished c s

let test_duplicates () =
  let open CS in
  let service = Services.manual () in
  let c, s = create ~client_tags:Test_utils.client_tags ~server_tags:Test_utils.server_tags
      (service :> Core_types.cap) in
  let f0 = C.bootstrap c in
  let q1 = call f0 "c1" [] in
  f0#dec_ref;
  let x1 = q1#cap 0 in
  let x2 = q1#cap 0 in
  q1#finish;
  assert (x1 = x2);
  x1#dec_ref;
  x2#dec_ref;
  S.handle_msg s ~expect:"bootstrap";
  C.handle_msg c ~expect:"return:(boot)";       (* [bs] resolves *)
  S.handle_msg s ~expect:"call:c1";
  S.handle_msg s ~expect:"finish";              (* bootstrap *)
  let (_, _, a1) = service#pop in
  a1#resolve (Ok ("a1", empty));
  C.handle_msg c ~expect:"return:a1";
  S.handle_msg s ~expect:"release";             (* bootstrap *)
  S.handle_msg s ~expect:"finish";              (* c1 *)
  CS.check_finished c s

let tests = [
  "Return",     `Quick, test_return;
  "Return error", `Quick, test_return_error;
  "Connection", `Quick, test_simple_connection;
  "Local embargo", `Quick, test_local_embargo;
  "Local embargo 2", `Quick, test_local_embargo_2;
  "Shared cap", `Quick, test_share_cap;
  "Fields", `Quick, test_fields;
  "Cancel", `Quick, test_cancel;
  "Duplicates", `Quick, test_duplicates;
]

let () =
  Alcotest.run ~and_exit:false "capnp-rpc" [
    "core", tests;
  ]
