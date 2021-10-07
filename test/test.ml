open Astring

module Core_types = Testbed.Capnp_direct.Core_types
module Request = Testbed.Capnp_direct.String_content.Request
module Response = Testbed.Capnp_direct.String_content.Response
module Test_utils = Testbed.Test_utils
module Services = Testbed.Services
module CS = Testbed.Connection.Pair ( )    (* A client-server pair *)
module RO_array = Capnp_rpc.RO_array
module Error = Capnp_rpc.Error
module Exception = Capnp_rpc.Exception
module Local_struct_promise = Testbed.Capnp_direct.Local_struct_promise
module Cap_proxy = Testbed.Capnp_direct.Cap_proxy

module C = CS.C
module S = CS.S

let inc_ref = Core_types.inc_ref
let dec_ref = Core_types.dec_ref
let with_inc_ref x = inc_ref x; (x :> Core_types.cap)

let response_equal a b =
  let a_caps = Core_types.Response_payload.snapshot_caps a in
  let b_caps = Core_types.Response_payload.snapshot_caps b in
  Response.data a = Response.data b &&
  RO_array.equal (=) a_caps b_caps

let error = Alcotest.of_pp Capnp_rpc.Error.pp
let response = Alcotest.testable Core_types.Response_payload.pp response_equal
let response_promise = Alcotest.(option (result response error))

let exn = Alcotest.of_pp Capnp_rpc.Exception.pp

let call target msg caps =
  let caps = List.map (fun x -> (x :> Core_types.cap)) caps in
  List.iter Core_types.inc_ref caps;
  let results, resolver = Local_struct_promise.make () in
  let msg =
    Testbed.Capnp_direct.String_content.Request.v msg
    |> Core_types.Request_payload.with_caps (RO_array.of_list caps)
  in
  target#call resolver msg;
  results

let call_for_cap target msg caps =
  let q = call target msg caps in
  let cap = q#cap 0 in
  dec_ref q;
  cap

(* Takes ownership of caps *)
let resolve_ok (ans:#Core_types.struct_resolver) msg caps =
  let caps = List.map (fun x -> (x :> Core_types.cap)) caps in
  let msg =
    Testbed.Capnp_direct.String_content.Request.v msg
    |> Core_types.Response_payload.with_caps (RO_array.of_list caps)
  in
  Core_types.resolve_ok ans msg

let test_simple_connection () =
  let c, s = CS.create ~client_tags:Test_utils.client_tags ~server_tags:Test_utils.server_tags (Services.echo_service ()) in
  let servce_promise = C.bootstrap c in
  S.handle_msg s ~expect:"bootstrap";
  C.handle_msg c ~expect:"return:(boot)";
  S.handle_msg s ~expect:"finish";
  let q = call servce_promise "my-content" [] in
  S.handle_msg s ~expect:"call:my-content";
  C.handle_msg c ~expect:"return:got:my-content";
  let expected = Request.v "got:my-content" in
  Alcotest.(check response_promise) "Client got call response" (Some (Ok expected)) q#response;
  dec_ref q;
  dec_ref servce_promise;
  CS.flush c s;
  CS.check_finished c s

let init_pair ~bootstrap_service =
  let c, s = CS.create ~client_tags:Test_utils.client_tags ~server_tags:Test_utils.server_tags bootstrap_service in
  let bs = C.bootstrap c in
  S.handle_msg s ~expect:"bootstrap";
  C.handle_msg c ~expect:"return:(boot)";
  S.handle_msg s ~expect:"finish";
  c, s, bs

(* The server gets an object and then sends it back. When the object arrives back
   at the client, it must be the original (local) object, not a proxy. *)
let test_return () =
  let c, s, bs = init_pair ~bootstrap_service:(Services.echo_service ()) in
  (* Pass callback *)
  let slot = ref (Request.v "empty") in
  let local = Services.swap_service slot in
  let q = call bs "c1" [local] in
  dec_ref local;
  (* Server echos args back *)
  S.handle_msg s ~expect:"call:c1";
  C.handle_msg c ~expect:"return:got:c1";
  let expected = Response.v "got:c1"
                 |> Core_types.Response_payload.with_caps (RO_array.of_list [(local :> Core_types.cap)])
  in
  Alcotest.(check response_promise) "Client got response" (Some (Ok expected)) q#response;
  dec_ref bs;
  S.handle_msg s ~expect:"finish";
  S.handle_msg s ~expect:"release";
  C.handle_msg c ~expect:"release";
  dec_ref q;
  CS.check_finished c s

let test_return_error () =
  let c, s, bs = init_pair ~bootstrap_service:(Core_types.broken_cap (Exception.v "test-error")) in
  (* Pass callback *)
  let slot = ref (Request.v "empty") in
  let local = Services.swap_service slot in
  let q = call bs "call" [local] in
  dec_ref local;
  (* Server echos args back *)
  CS.flush c s;
  Alcotest.(check response_promise) "Client got response" (Some (Error (Error.exn "test-error"))) q#response;
  dec_ref q;
  dec_ref bs;
  CS.flush c s;
  CS.check_finished c s

let test_share_cap () =
  let c, s, bs = init_pair ~bootstrap_service:(Services.echo_service ()) in
  let q = call bs "msg" [bs; bs] in
  dec_ref bs;
  S.handle_msg s ~expect:"call:msg";
  S.handle_msg s ~expect:"release";       (* Server drops [bs] export *)
  (* Server re-exports [bs] as result of echo *)
  C.handle_msg c ~expect:"return:got:msg";
  dec_ref q;
  CS.flush c s;
  CS.check_finished c s

(* The server gets an object and then sends it back. Messages pipelined to
   the object must arrive before ones sent directly. *)
let test_local_embargo () =
  let c, s, bs = init_pair ~bootstrap_service:(Services.echo_service ()) in
  let local = Services.logger () in
  let q = call bs "Get service" [local] in
  let service = q#cap 0 in
  let m1 = call service "Message-1" [] in
  S.handle_msg s ~expect:"call:Get service";
  C.handle_msg c ~expect:"return:got:Get service";
  dec_ref q;
  (* We've received the bootstrap reply, so we know that [service] is local,
     but the pipelined message we sent to it via [s] hasn't arrived yet. *)
  let m2 = call service "Message-2" [] in
  S.handle_msg s ~expect:"call:Message-1";
  C.handle_msg c ~expect:"call:Message-1";            (* Gets pipelined message back *)
  S.handle_msg s ~expect:"disembargo-request";
  C.handle_msg c ~expect:"return:take-from-other";    (* Get results of Message-1 directly *)
  C.handle_msg c ~expect:"disembargo-reply";
  Alcotest.(check string) "Pipelined arrived first" "Message-1" local#pop;
  Alcotest.(check string) "Embargoed arrived second" "Message-2" local#pop;
  (* Clean up *)
  dec_ref m1;
  dec_ref m2;
  dec_ref local;
  dec_ref bs;
  dec_ref service;
  CS.flush c s;
  CS.check_finished c s

(* As above, but this time it resolves to a promised answer. *)
let test_local_embargo_2 () =
  let server_main = Services.manual () in
  let c, s, bs = init_pair ~bootstrap_service:server_main in
  let local = Services.logger () in
  let local_reg = Services.manual () in    (* A registry that provides access to [local]. *)
  let q1 = call bs "q1" [local_reg] in (* Give the server our registry and get back [local]. *)
  let service = q1#cap 0 in                (* Service is a promise for local *)
  dec_ref q1;
  let m1 = call service "Message-1" [] in             (* First message to service *)
  S.handle_msg s ~expect:"call:q1";
  let proxy_to_local_reg, a1 = server_main#pop1 "q1" in
  (* The server will now make a call on the client registry, and then tell the client
     to use the (unknown) result of that for [service]. *)
  let q2 = call proxy_to_local_reg "q2" [] in
  dec_ref proxy_to_local_reg;
  let proxy_to_local = q2#cap 0 in
  resolve_ok a1 "a1" [proxy_to_local];
  (* [proxy_to_local] is now owned by [a1]. *)
  dec_ref q2;
  C.handle_msg c ~expect:"call:q2";
  let a2 = local_reg#pop0 "q2" in
  C.handle_msg c ~expect:"release";
  C.handle_msg c ~expect:"return:a1";
  (* The client now knows that [a1/0] is a local promise, but it can't use it directly yet because
     of the pipelined messages. It sends a disembargo request down the old [q1/0] path and waits for
     it to arrive back at the local promise. *)
  resolve_ok a2 "a2" [local];
  (* Message-2 must be embargoed so that it arrives after Message-1. *)
  let m2 = call service "Message-2" [] in
  S.handle_msg s ~expect:"call:Message-1";
  C.handle_msg c ~expect:"call:Message-1";            (* Gets pipelined message back *)
  S.handle_msg s ~expect:"disembargo-request";
  C.handle_msg c ~expect:"return:take-from-other";    (* Get results of Message-1 directly *)
  C.handle_msg c ~expect:"disembargo-reply";
  Alcotest.(check string) "Pipelined arrived first" "Message-1" local#pop;
  Alcotest.(check string) "Embargoed arrived second" "Message-2" local#pop;
  (* Clean up *)
  dec_ref m1;
  dec_ref m2;
  dec_ref bs;
  dec_ref service;
  dec_ref local_reg;
  CS.flush c s;
  CS.check_finished c s

(* Embargo on a resolve message *)
let test_local_embargo_3 () =
  let service = Services.manual () in
  let c, s, bs = init_pair ~bootstrap_service:service in
  let local = Services.logger () in
  let q1 = call bs "q1" [local] in
  S.handle_msg s ~expect:"call:q1";
  let proxy_to_logger, a1 = service#pop1 "q1" in
  let promise = Cap_proxy.local_promise () in
  resolve_ok a1 "a1" [promise];
  C.handle_msg c ~expect:"return:a1";
  let service = q1#cap 0 in
  let m1 = call service "Message-1" [] in
  promise#resolve proxy_to_logger;
  C.handle_msg c ~expect:"resolve";
  (* We've received the resolve message, so we know that [service] is local,
     but the pipelined message we sent to it via [s] hasn't arrived yet. *)
  let m2 = call service "Message-2" [] in
  S.handle_msg s ~expect:"finish";
  S.handle_msg s ~expect:"call:Message-1";
  C.handle_msg c ~expect:"call:Message-1";            (* Gets pipelined message back *)
  S.handle_msg s ~expect:"disembargo-request";
  C.handle_msg c ~expect:"return:take-from-other";    (* Get results of Message-1 directly *)
  C.handle_msg c ~expect:"disembargo-reply";
  Alcotest.(check string) "Pipelined arrived first" "Message-1" local#pop;
  Alcotest.(check string) "Embargoed arrived second" "Message-2" local#pop;
  (* Clean up *)
  dec_ref m1;
  dec_ref m2;
  dec_ref local;
  dec_ref q1;
  dec_ref bs;
  dec_ref service;
  CS.flush c s;
  CS.check_finished c s

(* Embargo a local answer that doesn't have the specified cap. *)
let test_local_embargo_4 () =
  let service = Services.manual () in
  let c, s, bs = init_pair ~bootstrap_service:service in
  let local = Services.echo_service () in
  let q1 = call bs "q1" [local] in
  let broken = q1#cap 0 in
  let qp = call broken "pipeline" [] in
  S.handle_msg s ~expect:"call:q1";
  let proxy_to_local, a1 = service#pop1 "q1" in
  let q2 = call proxy_to_local "q2" [] in
  resolve_ok a1 "a1" [q2#cap 0];
  dec_ref q2;
  C.handle_msg c ~expect:"call:q2";
  C.handle_msg c ~expect:"return:a1";
  (* At this point, the client knows that [broken] is its own answer to [q2], which is an error.
     It therefore does not try to disembargo it. *)
  Alcotest.(check string) "Error not embargoed"
    "Failed: Invalid capability index!"
   (Fmt.str "%t" broken#shortest#pp);
  (* Clean up *)
  dec_ref qp;
  dec_ref local;
  dec_ref proxy_to_local;
  dec_ref q1;
  dec_ref bs;
  CS.flush c s;
  CS.check_finished c s

(* A remote answer resolves to a remote promise, which doesn't require an embargo.
   However, when that promise resolves to a local service, we *do* need an embargo
   (because we pipelined over the answer), even though we didn't pipeline over the
   import. *)
let test_local_embargo_5 () =
  let service = Services.manual () in
  let c, s, bs = init_pair ~bootstrap_service:service in
  let local = Services.logger () in
  let q1 = call bs "q1" [local] in
  let test = q1#cap 0 in
  let m1 = call test "Message-1" [] in
  S.handle_msg s ~expect:"call:q1";
  let proxy_to_local, a1 = service#pop1 "q1" in
  let server_promise = Cap_proxy.local_promise () in
  resolve_ok a1 "a1" [server_promise];
  C.handle_msg c ~expect:"return:a1";
  (* [test] is now known to be at [service]; no embargo needed.
     The server now resolves it to a client service. *)
  server_promise#resolve proxy_to_local;
  C.handle_msg c ~expect:"resolve";
  let m2 = call test "Message-2" [] in
  CS.flush c s;
  Alcotest.(check string) "Pipelined arrived first" "Message-1" local#pop;
  Alcotest.(check string) "Embargoed arrived second" "Message-2" local#pop;
  CS.flush c s;
  (* Clean up *)
  dec_ref m1;
  dec_ref m2;
  dec_ref local;
  dec_ref test;
  dec_ref q1;
  dec_ref bs;
  CS.flush c s;
  CS.check_finished c s

(* We pipeline a message to a question, and then discover that it resolves
   to a local answer, which points to a capability at the peer. As the peer
   is already bouncing the pipelined message back to us, we need to embargo
   the new cap until the server's question is finished. *)
let test_local_embargo_6 () =
  let service = Services.manual () in
  let c, s, bs = init_pair ~bootstrap_service:service in
  let local = Services.manual () in
  (* Client calls the server, giving it [local]. *)
  let target = call_for_cap bs "q1" [local] in
  let m1 = call target "Message-1" [] in
  S.handle_msg s ~expect:"call:q1";
  let proxy_to_local, a1 = service#pop1 "q1" in
  (* Server makes a call on [local] and uses that promise to answer [q1]. *)
  let q2 = call proxy_to_local "q2" [] in
  resolve_ok a1 "a1" [q2#cap 0];
  C.handle_msg c ~expect:"call:q2";
  S.handle_msg s ~expect:"call:Message-1";      (* Forwards pipelined call back to the client *)
  (* Client resolves a2 to [bs]. *)
  let a2 = local#pop0 "q2" in
  resolve_ok a2 "a2" [bs];
  (* Server gets response to q2, that [q2#cap 0] is [bs].
     Although we don't actually care about this, it still embargoes it: *)
  S.handle_msg s ~expect:"return:a2";
  (* Client gets results from q1 - need to embargo it until we've forwarded the pipelined message
     back to the server. *)
  C.handle_msg c ~expect:"return:a1";
  Logs.info (fun f -> f "target = %t" target#pp);
  let m2 = call target "Message-2" [] in         (* Client tries to send message-2, but it gets embargoed *)
  dec_ref target;
  S.handle_msg s ~expect:"disembargo-request";
  S.handle_msg s ~expect:"finish";              (* Finish for q1 *)
  C.handle_msg c ~expect:"call:Message-1";      (* Pipelined message-1 arrives at client *)
  C.handle_msg c ~expect:"return:take-from-other";
  C.handle_msg c ~expect:"disembargo-request";  (* (the server is doing its own embargo on q2) *)
  S.handle_msg s ~expect:"call:Message-1";
  S.handle_msg s ~expect:"finish";
  S.handle_msg s ~expect:"disembargo-reply";    (* (the server is doing its own embargo on q2) *)
  C.handle_msg c ~expect:"disembargo-reply";
  S.handle_msg s ~expect:"call:Message-2";
  let am1 = service#pop0 "Message-1" in
  let am2 = service#pop0 "Message-2" in
  resolve_ok am1 "m1" [];
  resolve_ok am2 "m2" [];
  dec_ref m1;
  dec_ref m2;
  dec_ref q2;
  dec_ref proxy_to_local;
  dec_ref local;
  CS.flush c s;
  CS.check_finished c s

(* The client tries to disembargo via a switchable. *)
let test_local_embargo_7 () =
  let service = Services.manual () in
  let c, s, bs = init_pair ~bootstrap_service:service in
  let local = Services.manual () in
  (* Client calls the server, giving it [local]. *)
  let q1 = call bs "q1" [local] in
  let target = q1#cap 0 in
  dec_ref q1;
  let m1 = call target "Message-1" [] in
  S.handle_msg s ~expect:"call:q1";
  let proxy_to_local, a1 = service#pop1 "q1" in
  (* Server makes a call on [local] and uses that promise to answer [q1]. *)
  let q2 = call proxy_to_local "q2" [] in
  resolve_ok a1 "a1" [q2#cap 0];
  dec_ref q2;
  C.handle_msg c ~expect:"call:q2";
  S.handle_msg s ~expect:"call:Message-1";      (* Forwards pipelined call back to the client *)
  (* Client resolves a2 to a local promise. *)
  let client_promise = Cap_proxy.local_promise () in
  let a2 = local#pop0 "q2" in
  resolve_ok a2 "a2" [with_inc_ref client_promise];
  (* Client gets answer to a1 and sends disembargo. *)
  C.handle_msg c ~expect:"return:a1";
  let m2 = call target "Message-2" [] in
  S.handle_msg s ~expect:"return:a2";
  (* At this point, the server's answer to q1 is a switchable, because it expects the client
     to resolve the promise at some point in the future. *)
  S.handle_msg s ~expect:"disembargo-request";
  C.handle_msg c ~expect:"call:Message-1";      (* Pipelined message-1 arrives at client *)
  C.handle_msg c ~expect:"return:take-from-other";
  C.handle_msg c ~expect:"disembargo-reply";
  let client_logger = Services.logger () in
  inc_ref client_logger;
  client_promise#resolve (client_logger :> Core_types.cap);
  dec_ref client_promise;
  CS.flush c s;
  Alcotest.(check string) "Pipelined arrived first" "Message-1" client_logger#pop;
  Alcotest.(check string) "Embargoed arrived second" "Message-2" client_logger#pop;
  dec_ref m1;
  dec_ref m2;
  dec_ref client_logger;
  dec_ref proxy_to_local;
  dec_ref local;
  dec_ref bs;
  dec_ref target;
  CS.flush c s;
  CS.check_finished c s

let test_local_embargo_8 () =
  let service = Services.manual () in
  let c, s, bs = init_pair ~bootstrap_service:service in
  let local = Services.manual () in
  (* Client calls the server, giving it [local]. *)
  let q1 = call bs "q1" [local] in
  let target = q1#cap 0 in
  dec_ref q1;
  S.handle_msg s ~expect:"call:q1";
  let proxy_to_local, a1 = service#pop1 "q1" in
  (* Server makes a call on [local] and uses that promise to answer [q1]. *)
  let q2 = call proxy_to_local "q2" [] in
  (* Client resolves a2 to a local promise. *)
  C.handle_msg c ~expect:"call:q2";
  let a2 = local#pop0 "q2" in
  let local_promise = Cap_proxy.local_promise () in
  resolve_ok a2 "a2" [local_promise];
  (* The server then answers q1 with that [local_promise]. *)
  S.handle_msg s ~expect:"return:a2";
  resolve_ok a1 "a1" [q2#cap 0];
  dec_ref q2;
  C.handle_msg c ~expect:"finish";
  (* The client resolves the local promise to a remote one *)
  let q3 = call bs "q3" [] in
  let remote_promise = q3#cap 0 in
  let m1 = call target "Message-1" [] in
  local_promise#resolve remote_promise;
  S.handle_msg s ~expect:"call:q3";
  S.handle_msg s ~expect:"call:Message-1";      (* Forwards pipelined call back to the client *)
  S.handle_msg s ~expect:"resolve";
  (* Client gets answer to a1 and sends disembargo. *)
  C.handle_msg c ~expect:"return:a1";
  (* We now know that [target] is [remote_promise], but we need to embargo it until Message-1
     arrives back at the client. *)
  let m2 = call target "Message-2" [] in
  C.handle_msg c ~expect:"call:Message-1";      (* Forwards pipelined call back to the server again *)
  S.handle_msg s ~expect:"disembargo-request";
  S.handle_msg s ~expect:"finish";
  S.handle_msg s ~expect:"call:Message-1";
  C.handle_msg c ~expect:"return:take-from-other"; (* Reply to client's first Message-1 *)
  S.handle_msg s ~expect:"finish";
  C.handle_msg c ~expect:"disembargo-request";  (* Server is also doing its own embargo *)
  C.handle_msg c ~expect:"disembargo-reply";    (* Client now disembargoes Message-2 *)
  S.handle_msg s ~expect:"disembargo-reply";
  C.handle_msg c ~expect:"release";
  C.handle_msg c ~expect:"finish";
  S.handle_msg s ~expect:"call:Message-2";
  let logger = Services.logger () in
  let a3 = service#pop0 "q3" in
  inc_ref logger;
  resolve_ok a3 "a3" [logger];
  Alcotest.(check string) "Pipelined arrived first" "Message-1" logger#pop;
  Alcotest.(check string) "Embargoed arrived second" "Message-2" logger#pop;
  dec_ref m1;
  dec_ref m2;
  dec_ref q3;
  dec_ref target;
  dec_ref proxy_to_local;
  dec_ref logger;
  dec_ref bs;
  dec_ref local;
  CS.flush c s;
  CS.check_finished c s

(* m1 and m2 are sent in order on the same reference, [pts2].
   They must arrive in order too. *)
let _test_local_embargo_9 () =
  let client_bs = Services.manual () in
  let service_bs = Services.manual () in
  let c, s = CS.create
      ~client_tags:Test_utils.client_tags ~client_bs:(with_inc_ref client_bs)
      ~server_tags:Test_utils.server_tags (with_inc_ref service_bs) in
  (* The client gets the server's bootstrap. *)
  let service = C.bootstrap c in
  S.handle_msg s ~expect:"bootstrap";
  C.handle_msg c ~expect:"return:(boot)";
  S.handle_msg s ~expect:"finish";
  (* The server gets the client's bootstrap. *)
  let ptc0 = S.bootstrap s in                   (* The first proxy-to-client *)
  C.handle_msg c ~expect:"bootstrap";
  S.handle_msg s ~expect:"return:(boot)";
  C.handle_msg c ~expect:"finish";
  (* The client calls the server. *)
  let pts1 = call_for_cap service "service.ptc0" [] in (* will become [ptc0] *)
  let pts2 = call_for_cap service "service.ptc1" [] in (* will become [ptc1] *)
  S.handle_msg s ~expect:"call:service.ptc0";
  S.handle_msg s ~expect:"call:service.ptc1";
  (* The server calls the client. *)
  let ptc1 = call_for_cap ptc0 "client.self" [] in (* [ptc1] will become [ptc0] *)
  C.handle_msg c ~expect:"call:client.self";
  (* The client handles the server's request by returning [pts1], which will become [ptc0]. *)
  let ptc0_resolver = client_bs#pop0 "client.self" in
  resolve_ok ptc0_resolver "reply" [pts1];
  (* The server handles the client's requests by returning [ptc0] (the client's bootstrap)
     and [ptc1], which will resolve to the client's bootstrap later. *)
  let pts0_resolver = service_bs#pop0 "service.ptc0" in
  resolve_ok pts0_resolver "ptc0" [ptc0];
  let pts1_resolver = service_bs#pop0 "service.ptc1" in
  resolve_ok pts1_resolver "ptc1" [with_inc_ref ptc1];
  (* The client pipelines a message to the server: *)
  let m1 = call pts2 "m1" [] in
  (* The client gets replies to its questions: *)
  C.handle_msg c ~expect:"return:ptc0";         (* Resolves pts1 to client_bs (only used for pipelining) *)
  C.handle_msg c ~expect:"return:ptc1";         (* Resolves pts2 to embargoed(pts1) (embargoed because of [m1]) *)
  (* The client knows [ptc1] is local, but has embargoed it.
     [m1] must arrive back at the client before the disembargo. *)
  let m2 = call pts2 "m2" [] in
  S.handle_msg s ~expect:"return:reply";
  S.handle_msg s ~expect:"call:m1";             (* Server forwards m1 back to client *)
  C.handle_msg c ~expect:"call:m1";             (* Client forwards m1 back to server *)
  S.handle_msg s ~expect:"disembargo-request";
  C.handle_msg c ~expect:"return:take-from-other";
  C.handle_msg c ~expect:"disembargo-reply";
  (* Client does a second disembargo *)
  S.handle_msg s ~expect:"finish";
  C.handle_msg c ~expect:"finish";
  S.handle_msg s ~expect:"call:m1";             (* Server forwards m1 back to client again *)
  C.handle_msg c ~expect:"call:m1";             (* m1 finally arrives *)
  S.handle_msg s ~expect:"finish";
  S.handle_msg s ~expect:"disembargo-request";
  C.handle_msg c ~expect:"return:take-from-other";
  C.handle_msg c ~expect:"finish";
  C.handle_msg c ~expect:"disembargo-reply";
  (* At this point, the client knows [m1] must have arrived by now and delivers m2. *)
  let am1 = client_bs#pop0 "m1" in
  let am2 = client_bs#pop0 "m2" in
  resolve_ok am1 "am1" [];
  resolve_ok am2 "am2" [];
  dec_ref pts2;
  dec_ref ptc1;
  dec_ref client_bs;
  dec_ref service_bs;
  dec_ref service;
  dec_ref m1;
  dec_ref m2;
  CS.flush c s;
  CS.check_finished c s

(* We still need embargoes with self-results-to=yourself. *)
let test_local_embargo_10 () =
  let service_1 = Services.manual () in         (* At the client *)
  let c, s = CS.create
    ~client_tags:Test_utils.client_tags
    ~server_tags:Test_utils.server_tags (Services.echo_service ())
  in
  let proxy_to_echo = C.bootstrap c in
  CS.flush c s;
  (* The client asks for a service, which will resolve to [service_1].
     It pipelines it a message [q1], and then pipelines [m1] on the result of that.
     The server will forward [q1] back to the client and tell it to take the answer
     from that. Because the client already sent [m1] over the result, it must
     embargo it and wait before sending [m2]. *)
  let q0 = call proxy_to_echo "echo" [service_1] in
  let bs = q0#cap 0 in
  dec_ref q0;
  (* bs is a promise for the client's own [service_1]. *)
  let q1 = call bs "q1" [] in
  let target = q1#cap 0 in
  let m1 = call target "M-1" [] in
  S.handle_msg s ~expect:"call:echo";
  S.handle_msg s ~expect:"call:q1";
  S.handle_msg s ~expect:"call:M-1";
  C.handle_msg c ~expect:"return:got:echo";
  S.handle_msg s ~expect:"disembargo-request";          (* Client disembargoing bootstrap *)
  C.handle_msg c ~expect:"call:q1";
  let aq1 = service_1#pop0 "q1" in
  resolve_ok aq1 "aq1" [with_inc_ref service_1];
  C.handle_msg c ~expect:"return:take-from-other";      (* Return for client's q1 - use aq1 *)
  (* At this point, the client knows that [target] is [service_1], but must embargo it until
     it knows that "M-1" has been delivered. *)
  let m2 = call target "M-2" [] in
  C.handle_msg c ~expect:"call:M-1";                    (* Pipelined call arrives back *)
  C.handle_msg c ~expect:"return:take-from-other";      (* Return for M-1 *)
  C.handle_msg c ~expect:"disembargo-reply";            (* Disembargo of [bs]. *)
  S.handle_msg s ~expect:"finish";                      (* Bootstrap *)
  S.handle_msg s ~expect:"return:sent-elsewhere";       (* For forwarded q1 *)
  S.handle_msg s ~expect:"disembargo-request";
  C.handle_msg c ~expect:"release";
  C.handle_msg c ~expect:"disembargo-reply";
  let am1 = service_1#pop0 "M-1" in
  let am2 = service_1#pop0 "M-2" in
  resolve_ok am1 "am1" [];
  resolve_ok am2 "am2" [];
  dec_ref q1;
  dec_ref m1;
  dec_ref m2;
  dec_ref target;
  dec_ref bs;
  dec_ref proxy_to_echo;
  dec_ref service_1;
  CS.flush c s;
  CS.check_finished c s

let test_local_embargo_11 () =
  let client_bs = Services.manual () in
  let server_bs = Services.manual () in
  let c, s = CS.create
    ~client_tags:Test_utils.client_tags ~client_bs
    ~server_tags:Test_utils.server_tags server_bs
  in
  let to_server_bs = C.bootstrap c in
  let to_client_bs = S.bootstrap s in
  CS.flush c s;
  let q1 = call_for_cap to_server_bs "q1" [] in
  let to_server_bs_2 = C.bootstrap c in
  let q2 = call_for_cap to_client_bs "q2" [] in
  S.handle_msg s ~expect:"call:q1";
  resolve_ok (server_bs#pop0 "q1") "a1" [with_inc_ref q2];
  let q3 = call q1 "q3" [] in
  C.handle_msg c ~expect:"call:q2";
  resolve_ok (client_bs#pop0 "q2") "a2" [with_inc_ref to_server_bs_2];
  S.handle_msg s ~expect:"bootstrap";
  (* Client gets a1, resolving q1 to the already-answered q2's cap 0.
     As q3 was pipelined over q1 and q2's cap 0 is currently a remote-promise
     (q2), it embargoes q1. *)
  C.handle_msg c ~expect:"return:a1";
  (* to_server_bs_2 resolves. No embargo is needed: *)
  C.handle_msg c ~expect:"return:(boot)";
  S.handle_msg s ~expect:"call:q3";             (* Pipelined q3 arrives, forwarded to q2 *)
  C.handle_msg c ~expect:"call:q3";             (* q3 back at client, sent to bootstrap call *)
  C.handle_msg c ~expect:"return:take-from-other"; (* use forwarded call for answer to q3 *)
  S.handle_msg s ~expect:"return:a2";           (* q2 = server_bs, embargoed due to q3 forwarding *)
  (* note: probably shouldn't mark paths as dirty when just forwarding, but should still work *)
  C.handle_msg c ~expect:"disembargo-request";
  S.handle_msg s ~expect:"disembargo-request";
  C.handle_msg c ~expect:"disembargo-reply";    (* Second embargo not needed, as remote *)
  dec_ref to_server_bs;
  dec_ref to_server_bs_2;
  dec_ref to_client_bs;
  dec_ref q1;
  dec_ref q2;
  dec_ref q3;
  CS.flush c s;
  CS.check_finished c s

let test_local_embargo_12 () =
  let server_bs = Services.manual () in
  let c, s, bs = init_pair ~bootstrap_service:server_bs in
  (* Client calls [bs], passing a local promise as the argument. *)
  let x, xr = Local_struct_promise.make () in   (* x will become broken *)
  let x0 = x#cap 0 in
  let q1 = call bs "q1" [x0] in
  (* Client resolves local promise to the (about to fail) promise q1:2 *)
  resolve_ok xr "x" [with_inc_ref (q1#cap 2)];
  (* Client pipelines q2 over q1:0 *)
  let q2 = call (q1#cap 0) "q2" [] in
  (* Server handles q1. The result is a promise for the result of a call to q1:2,
     which will fail. *)
  S.handle_msg s ~expect:"call:q1";
  let to_x0, a1 = server_bs#pop1 "q1" in
  let y = call_for_cap to_x0 "q3" [] in
  resolve_ok a1 "a1" [y];
  C.handle_msg c ~expect:"call:q3";     (* q3 arrives at client and is forwarded to q1 (to=yourself) *)
  S.handle_msg s ~expect:"resolve";     (* [to_x0 = q3#2]. *)
  C.handle_msg c ~expect:"return:a1";   (* [q1 = q3#0] *)
  (* Client has pipelined over q1, so embargoes it. *)
  S.handle_msg s ~expect:"call:q2";     (* Server forwards q2 back along q3, replies with take-from-other(q4) *)
  C.handle_msg c ~expect:"release";     (* Server will use q3 instead of i0 for to_x0 *)
  C.handle_msg c ~expect:"call:q2";     (* Client forwards q2 to q1#2 *)
  S.handle_msg s ~expect:"call:q3";     (* q3 arrives at a1#2, which doesn't exist *)
  (* Server replies with results-sent-elsewhere. Is this correct? It's really an error. *)
  C.handle_msg c ~expect:"return:take-from-other";   (* Take from q2=q4 *)
  CS.dump c s;
  S.handle_msg s ~expect:"return:take-from-other";   (* Take from broken answer *)
  C.handle_msg c ~expect:"return:sent-elsewhere";
  S.handle_msg s ~expect:"disembargo-request";       (* Client wants to clear q1 to disembargo x *)
  C.handle_msg c ~expect:"disembargo-reply";
  (* As [x] is now broken, no further disembargoes should be sent. *)
  dec_ref q1;
  dec_ref q2;
  dec_ref bs;
  dec_ref x;
  dec_ref x0;
  dec_ref to_x0;
  CS.flush c s;
  CS.check_finished c s

let test_local_embargo_13 () =
  let client_bs = Services.manual () in
  let server_bs = Services.manual () in
  let c, s = CS.create
    ~client_tags:Test_utils.client_tags ~client_bs
    ~server_tags:Test_utils.server_tags server_bs
  in
  let to_client = S.bootstrap s in
  let to_server = C.bootstrap c in
  CS.flush c s;
  let broken = Core_types.broken_cap (Capnp_rpc.Exception.v "broken") in   (* (at server) *)
  (* Server calls client, passing a broken cap.
     Due to a protocol limitation, we first send this as an export and then break it. *)
  let q1 = call_for_cap to_client "q1" [broken] in
  (* The client calls the server, and pipelines over the result *)
  let q2 = call_for_cap to_server "q2" [] in
  let q3 = call q2 "q3" [] in
  (* Client gets exported (soon to be broken) cap and echoes it back. *)
  C.handle_msg c ~expect:"call:q1";
  let to_broken, a1 = client_bs#pop1 "q1" in
  resolve_ok a1 "a1" [to_broken];
  (* Server replies to q2 with q1. *)
  S.handle_msg s ~expect:"call:q2";
  let a2 = server_bs#pop0 "q2" in
  resolve_ok a2 "a2" [q1];
  C.handle_msg c ~expect:"resolve";     (* Client discovers to_broken is broken *)
  S.handle_msg s ~expect:"call:q3";     (* Server gets q3, forwards to q1 *)
  C.handle_msg c ~expect:"return:a2";   (* Client gets q2 = q1, embargoes due to q3 *)
  C.handle_msg c ~expect:"call:q3";     (* Client forwards q3 to broken *)
  (* Not sure if we need to forward q3 here, since we know the target is broken. *)
  (* When q2 embargo is done, client must not do a second embargo, since q2 is now broken. *)
  dec_ref q2;
  dec_ref q3;
  dec_ref to_server;
  dec_ref to_client;
  CS.flush c s;
  CS.check_finished c s

let test_local_embargo_14 () =
  let client_bs = Services.manual () in (* Bootstrap for vat 0 *)
  let server_bs = Services.manual () in (* Bootstrap for vat 1 *)
  let c, s = CS.create
    ~client_tags:Test_utils.client_tags ~client_bs
    ~server_tags:Test_utils.server_tags server_bs
  in
  let to_server = C.bootstrap c in
  let to_client = S.bootstrap s in
  CS.flush c s;
  let client_via_q1 = call_for_cap to_server "q1" [] in
  S.handle_msg s ~expect:"call:q1";
  resolve_ok (server_bs#pop0 "q1") "a1" [to_client];
  let q2 = call client_via_q1 "q2" [] in
  let server_via_q2 = q2#cap 0 in
  let broken = q2#cap 2 in
  C.handle_msg c ~expect:"return:a1";
  (* We sent q2 down q1, so the client will embargo client_via_q1 *)
  S.handle_msg s ~expect:"call:q2";     (* Forwards q2 back to client as q2' *)
  C.handle_msg c ~expect:"call:q2";     (* client_bs gets q2'. *)
  let q3 = call broken "q3" [] in
  C.handle_msg c ~expect:"return:take-from-other";      (* Client learns q2 = q2' *)
  (* Client embargoes q2, due to q3 *)
  let m1 = call server_via_q2 "m1" [] in
  Logs.info (fun f -> f "server_via_q2 = %t" server_via_q2#pp);
  resolve_ok (client_bs#pop0 "q2") "a2" [to_server];
  Logs.info (fun f -> f "server_via_q2 = %t" server_via_q2#pp);
  let m2 = call server_via_q2 "m2" [] in
  CS.flush c s;
  let _ = server_bs#pop0 "m1" in
  let _ = server_bs#pop0 "m2" in
  dec_ref client_via_q1;
  dec_ref server_via_q2;
  dec_ref broken;
  dec_ref q2;
  dec_ref q3;
  dec_ref m1;
  dec_ref m2;
  CS.flush c s;
  CS.check_finished c s

let test_local_embargo_15 () =
  let client_bs = Services.manual () in
  let server_bs = Services.manual () in
  let c, s = CS.create
    ~client_tags:Test_utils.client_tags ~client_bs
    ~server_tags:Test_utils.server_tags server_bs
  in
  let to_server = C.bootstrap c in
  let to_client = S.bootstrap s in
  let x1 = call_for_cap to_server "q1" [] in
  CS.flush c s;
  let x2 = call_for_cap to_client "q2" [] in
  let x3 = call_for_cap to_client "q3" [] in
  CS.flush c s;
  resolve_ok (server_bs#pop0 "q1") "reply" [with_inc_ref x3];
  resolve_ok (client_bs#pop0 "q2") "reply" [with_inc_ref x1];
  let m1 = call x2 "m1" [] in
  S.handle_msg s ~expect:"return:reply";        (* q2 = x3 *)
  let m2 = call x2 "m2" [] in
  let local_promise = Cap_proxy.local_promise () in
  resolve_ok (client_bs#pop0 "q3") "reply" [with_inc_ref local_promise];
  local_promise#resolve (with_inc_ref to_server);
  dec_ref local_promise;
  CS.flush c s;
  let am1 = server_bs#pop0 "m1" in
  let am2 = server_bs#pop0 "m2" in
  resolve_ok am1 "am1" [];
  resolve_ok am2 "am2" [];
  dec_ref m1;
  dec_ref m2;
  dec_ref x1;
  dec_ref x2;
  dec_ref x3;
  dec_ref to_server;
  dec_ref to_client;
  CS.flush c s;
  CS.check_finished c s

(* The field must still be useable after the struct is released. *)
let test_fields () =
  let c, s = CS.create ~client_tags:Test_utils.client_tags ~server_tags:Test_utils.server_tags (Services.echo_service ()) in
  let f0 = C.bootstrap c in
  let q1 = call f0 "c1" [] in
  S.handle_msg s ~expect:"bootstrap";
  C.handle_msg c ~expect:"return:(boot)";      (* [bs] resolves *)
  S.handle_msg s ~expect:"call:c1";
  S.handle_msg s ~expect:"finish";
  C.handle_msg c ~expect:"return:got:c1";
  Alcotest.(check response_promise) "Echo response" (Some (Ok (Response.v "got:c1"))) q1#response;
  dec_ref q1;
  let q2 = call f0 "c2" [] in
  CS.flush c s;
  Alcotest.(check response_promise) "Echo response 2" (Some (Ok (Response.v "got:c2"))) q2#response;
  dec_ref q2;
  dec_ref f0;
  CS.flush c s;
  CS.check_finished c s

let test_cancel () =
  let service = Services.manual () in
  let c, s = CS.create ~client_tags:Test_utils.client_tags ~server_tags:Test_utils.server_tags
      (service :> Core_types.cap) in
  let f0 = C.bootstrap c in
  let q1 = call f0 "c1" [] in
  let prom = q1#cap 0 in
  dec_ref q1;    (* Client doesn't cancel q1 because we're using prom *)
  let _q2 = call prom "p1" [] in
  S.handle_msg s ~expect:"bootstrap";
  C.handle_msg c ~expect:"return:(boot)";      (* [bs] resolves *)
  S.handle_msg s ~expect:"call:c1";
  S.handle_msg s ~expect:"call:p1";
  S.handle_msg s ~expect:"finish";      (* bootstrap *)
  let a1 = service#pop0 "c1" in
  resolve_ok a1 "a1" [];
  C.handle_msg c ~expect:"return:Invalid capability index!";
  C.handle_msg c ~expect:"return:a1";
  dec_ref f0;
  CS.flush c s;
  CS.check_finished c s

(* Actually sends a cancel *)
let test_cancel_2 () =
  let service = Services.manual () in
  let c, s, bs = init_pair ~bootstrap_service:service in
  let q1 = call bs "c1" [] in
  dec_ref q1;    (* Client cancels *)
  S.handle_msg s ~expect:"call:c1";
  S.handle_msg s ~expect:"finish";      (* cancel *)
  let a1 = service#pop0 "c1" in
  let echo = Services.echo_service () in
  resolve_ok a1 "a1" [echo];
  C.handle_msg c ~expect:"return:(cancelled)";
  dec_ref bs;
  CS.flush c s;
  CS.check_finished c s

(* Don't forget to release the returned cap if the question was cancelled. *)
let test_cancel_3 () =
  let service = Services.manual () in
  let c, s = CS.create
    ~client_tags:Test_utils.client_tags
    ~server_tags:Test_utils.server_tags service
  in
  let proxy_to_service = C.bootstrap c in
  let q1 = call proxy_to_service "q1" [] in
  S.handle_msg s ~expect:"bootstrap";
  S.handle_msg s ~expect:"call:q1";
  resolve_ok (service#pop0 "q1") "reply" [Core_types.null];
  C.handle_msg c ~expect:"return:(boot)";
  dec_ref q1;
  C.handle_msg c ~expect:"return:reply";
  dec_ref proxy_to_service;
  CS.flush c s;
  CS.check_finished c s

(* Asking for the same field twice gives the same object. *)
let test_duplicates () =
  let service = Services.manual () in
  let c, s = CS.create ~client_tags:Test_utils.client_tags ~server_tags:Test_utils.server_tags
      (service :> Core_types.cap) in
  let f0 = C.bootstrap c in
  let q1 = call f0 "c1" [] in
  dec_ref f0;
  let x1 = q1#cap 0 in
  let x2 = q1#cap 0 in
  dec_ref q1;
  assert (x1 = x2);
  dec_ref x1;
  dec_ref x2;
  S.handle_msg s ~expect:"bootstrap";
  C.handle_msg c ~expect:"return:(boot)";       (* [bs] resolves *)
  S.handle_msg s ~expect:"call:c1";
  S.handle_msg s ~expect:"finish";              (* bootstrap question *)
  S.handle_msg s ~expect:"release";             (* bootstrap cap *)
  let a1 = service#pop0 "c1" in
  resolve_ok a1 "a1" [];
  C.handle_msg c ~expect:"return:a1";
  S.handle_msg s ~expect:"finish";              (* c1 *)
  CS.check_finished c s

(* Exporting a cap twice reuses the existing export. *)
let test_single_export () =
  let service = Services.manual () in
  let c, s, bs = init_pair ~bootstrap_service:service in
  let local = Services.echo_service () in
  let q1 = call bs "q1" [local; local] in
  let q2 = call bs "q2" [local] in
  Alcotest.(check int) "One export" 1 (C.stats c).n_exports;
  S.handle_msg s ~expect:"call:q1";
  S.handle_msg s ~expect:"call:q2";
  dec_ref q1;
  dec_ref q2;
  let ignore msg =
    let got, a = service#pop_n msg in
    RO_array.iter dec_ref got;
    resolve_ok a "a" []
  in
  ignore "q1";
  ignore "q2";
  dec_ref local;
  dec_ref bs;
  CS.flush c s;
  CS.check_finished c s

(* Exporting a field of a remote promise sends a promised answer desc. *)
let test_shorten_field () =
  let service = Services.manual () in
  let logger = Services.logger () in
  let c, s, bs = init_pair ~bootstrap_service:service in
  let q1 = call bs "q1" [] in
  let proxy_to_logger = q1#cap 0 in
  let q2 = call bs "q2" [proxy_to_logger] in
  S.handle_msg s ~expect:"call:q1";
  let a1 = service#pop0 "q1" in
  resolve_ok a1 "a1" [logger];
  S.handle_msg s ~expect:"call:q2";
  let direct_to_logger, a2 = service#pop1 "q2" in
  assert (direct_to_logger#shortest = (logger :> Core_types.cap));
  resolve_ok a2 "a2" [];
  dec_ref direct_to_logger;
  dec_ref bs;
  dec_ref proxy_to_logger;
  dec_ref q1;
  dec_ref q2;
  CS.flush c s;
  CS.check_finished c s

let ensure_is_cycle_error (x:#Core_types.struct_ref) : unit =
  match x#response with
  | Some (Error (`Exception ex))
    when (String.is_prefix ~affix:"Attempt to create a cycle detected:" ex.Exception.reason) -> ()
  | _ -> Alcotest.fail (Fmt.str "Not a cycle error: %t" x#pp)

let ensure_is_cycle_error_cap cap =
  match cap#problem with
  | Some ex when (String.is_prefix ~affix:"<cycle: " ex.Exception.reason) -> ()
  | _ -> Alcotest.fail (Fmt.str "Not a cycle error: %t" cap#pp)

let test_cycle () =
  (* Cap cycles *)
  let module P = Testbed.Capnp_direct.Cap_proxy in
  let p1 = P.local_promise () in
  let p2 = P.local_promise () in
  p1#resolve (p2 :> Core_types.cap);
  p2#resolve (p1 :> Core_types.cap);
  ensure_is_cycle_error (call p2 "test" []);
  (* Connect struct to its own field *)
  let p1, p1r = Local_struct_promise.make () in
  let c = p1#cap 0 in
  inc_ref c;
  resolve_ok p1r "msg" [c];
  ensure_is_cycle_error_cap c;
  dec_ref c;
  dec_ref p1;
  (* Connect struct to itself *)
  let p1, p1r = Local_struct_promise.make () in
  p1r#resolve p1;
  ensure_is_cycle_error p1;
  dec_ref p1

(* Resolve a promise with an answer that includes the result of a pipelined
   call on the promise itself. *)
let test_cycle_2 () =
  let s1, s1r = Local_struct_promise.make () in
  let s2 = call (s1#cap 0) "get-s2" [] in
  resolve_ok s1r "a7" [s2#cap 0];
  ensure_is_cycle_error_cap (s1#cap 0);
  dec_ref s2;
  dec_ref s1

(* It's not a cycle if one field resolves to another. *)
let test_cycle_3 () =
  let echo = Services.echo_service () in
  let a1, a1r = Local_struct_promise.make () in
  resolve_ok a1r "a1" [a1#cap 1; (echo :> Core_types.cap)];
  let target = a1#cap 1 in
  let q2 = call target "q2" [] in
  Alcotest.(check response_promise) "Field 1 OK"
    (Some (Ok (Response.v "got:q2")))
    q2#response;
  dec_ref q2;
  dec_ref target;
  dec_ref a1

(* Check ref-counting when resolving loops. *)
let test_cycle_4 () =
  let echo = Services.echo_service () in
  let a1, a1r = Local_struct_promise.make () in
  let f0 = a1#cap 0 in
  resolve_ok a1r "a1" [a1#cap 1; (echo :> Core_types.cap)];
  dec_ref f0;
  dec_ref a1;
  Logs.info (fun f -> f "echo = %t" echo#pp);
  Alcotest.(check bool) "Echo released" true echo#released

(* A field depends on the struct. *)
let test_cycle_5 () =
  let a, ar = Local_struct_promise.make () in
  let b, br = Local_struct_promise.make () in
  let c, cr = Local_struct_promise.make () in
  Alcotest.(check (result unit reject)) "Not a cycle" (Ok ()) @@ br#set_blocker (c :> Core_types.base_ref);
  Alcotest.(check (result unit reject)) "Not a cycle" (Ok ()) @@ cr#set_blocker (a :> Core_types.base_ref);
  let b0 = b#cap 0 in
  let reply =
    Response.v "reply"
    |> Core_types.Response_payload.with_caps (RO_array.of_list [b0])
  in
  let x = Core_types.return reply in
  ar#resolve x;
  Logs.info (fun f -> f "a = %t" a#pp);
  ensure_is_cycle_error_cap (a#cap 0);
  dec_ref a

(* A blocker depends on itself. *)
let test_cycle_6 () =
  let a, ar = Local_struct_promise.make () in
  let a0 = a#cap 0 in
  a0#call ar (Request.v "loop");
  Logs.info (fun f -> f "a0 = %t" a#pp)

(* The server returns an answer containing a promise. Later, it resolves the promise
   to a resource at the client. The client must be able to invoke the service locally. *)
let test_resolve () =
  let service = Services.manual () in
  let client_logger = Services.logger () in
  let c, s, proxy_to_service = init_pair ~bootstrap_service:service in
  (* The client makes a call and gets a reply, but the reply contains a promise. *)
  let q1 = call proxy_to_service "q1" [client_logger] in
  dec_ref proxy_to_service;
  S.handle_msg s ~expect:"call:q1";
  let proxy_to_logger, a1 = service#pop1 "q1" in
  let promise = Cap_proxy.local_promise () in
  inc_ref promise;
  resolve_ok a1 "a1" [promise];
  C.handle_msg c ~expect:"return:a1";
  (* The server now resolves the promise *)
  promise#resolve proxy_to_logger;
  dec_ref promise;
  CS.flush c s;
  (* The client can now use the logger directly *)
  let x = q1#cap 0 in
  let q2 = call x "test-message" [] in
  Alcotest.(check string) "Got message directly" "test-message" client_logger#pop;
  dec_ref x;
  dec_ref q1;
  dec_ref q2;
  dec_ref client_logger;
  CS.flush c s;
  CS.check_finished c s

(* The server resolves an export after the client has released it.
   The client releases the new target. *)
let test_resolve_2 () =
  let service = Services.manual () in
  let client_logger = Services.logger () in
  let c, s, proxy_to_service = init_pair ~bootstrap_service:service in
  (* The client makes a call and gets a reply, but the reply contains a promise. *)
  let q1 = call proxy_to_service "q1" [client_logger] in
  dec_ref client_logger;
  dec_ref proxy_to_service;
  S.handle_msg s ~expect:"call:q1";
  let proxy_to_logger, a1 = service#pop1 "q1" in
  let promise = Cap_proxy.local_promise () in
  resolve_ok a1 "a1" [promise];
  C.handle_msg c ~expect:"return:a1";
  (* The client doesn't care about the result and releases it *)
  dec_ref q1;
  (* The server now resolves the promise. The client must release the new target. *)
  promise#resolve proxy_to_logger;
  CS.flush c s;
  CS.check_finished c s

(* The server returns a promise, but by the time it resolves the server
   has removed the export. It must not send a resolve message. *)
let test_resolve_3 () =
  let service = Services.manual () in
  let c, s, proxy_to_service = init_pair ~bootstrap_service:service in
  (* Make a call, get a promise, and release it *)
  let q1 = call proxy_to_service "q1" [] in
  S.handle_msg s ~expect:"call:q1";
  let a1 = service#pop0 "q1" in
  let a1_promise = Cap_proxy.local_promise () in
  inc_ref a1_promise;
  resolve_ok a1 "a1" [a1_promise];
  C.handle_msg c ~expect:"return:a1";
  dec_ref q1;
  S.handle_msg s ~expect:"finish";
  S.handle_msg s ~expect:"release";
  (* Make another call, get a settled export this time. *)
  let q2 = call proxy_to_service "q2" [] in
  S.handle_msg s ~expect:"call:q2";
  CS.flush c s;
  let a2 = service#pop0 "q2" in
  let echo = Services.echo_service () in
  inc_ref echo;
  resolve_ok a2 "a2" [echo];
  C.handle_msg c ~expect:"return:a2";
  (* Service now resolves first answer *)
  a1_promise#resolve (echo :> Core_types.cap);
  dec_ref a1_promise;
  dec_ref proxy_to_service;
  CS.flush c s;
  dec_ref q2;
  CS.flush c s;
  CS.check_finished c s

(* Resolving a remote's export to another export, which we haven't seen yet.
   We must add the new import to the table before looking it up to set the
   disembargo target. *)
let test_resolve_4 () =
  let service = Services.manual () in
  let c, s = CS.create
    ~client_tags:Test_utils.client_tags
    ~server_tags:Test_utils.server_tags service
  in
  let to_server = C.bootstrap c in
  let x = Cap_proxy.local_promise () in
  let q1 = call to_server "q1" [x] in
  x#resolve (Services.manual () :> Core_types.cap);
  CS.flush c s;
  let to_x, a1 = service#pop1 "q1" in
  resolve_ok a1 "a1" [];
  dec_ref to_x;
  dec_ref q1;
  dec_ref x;
  dec_ref to_server;
  CS.flush c s;
  CS.check_finished c s

(* Finishing a question releases multiple imports *)
let test_resolve_5 () =
  let service = Services.manual () in
  let c, s = CS.create
    ~client_tags:Test_utils.client_tags
    ~server_tags:Test_utils.server_tags service
  in
  let promise = Cap_proxy.local_promise () in
  let to_service = C.bootstrap c in
  let q1 = call to_service "q1" [promise] in
  S.handle_msg s ~expect:"bootstrap";
  S.handle_msg s ~expect:"call:q1";
  let to_promise, a1 = service#pop1 "q1" in
  resolve_ok a1 "a1" [to_promise];
  C.handle_msg c ~expect:"return:(boot)";
  promise#resolve (Services.manual () :> Core_types.cap);
  C.handle_msg c ~expect:"return:a1";
  S.handle_msg s ~expect:"finish";      (* Bootstrap *)
  S.handle_msg s ~expect:"resolve";
  S.handle_msg s ~expect:"finish";
  dec_ref q1;
  dec_ref to_service;
  dec_ref promise;
  CS.flush c s;
  CS.check_finished c s

(* When a proxy is released it must be removed from the import,
   which may need to hang around for forwarding. *)
let test_resolve_6 () =
  let client_bs = Services.manual () in
  let server_bs = Services.manual () in
  let c, s = CS.create
    ~client_tags:Test_utils.client_tags ~client_bs
    ~server_tags:Test_utils.server_tags server_bs
  in
  let to_server = C.bootstrap c in
  let to_client = S.bootstrap s in
  CS.flush c s;
  let x = call_for_cap to_server "q1" [] in
  let y = call_for_cap to_client "q2" [] in
  S.handle_msg s ~expect:"call:q1";
  resolve_ok (server_bs#pop0 "q1") "a1" [to_client; Core_types.null];
  C.handle_msg c ~expect:"call:q2";
  resolve_ok (client_bs#pop0 "q2") "a2" [x];
  C.handle_msg c ~expect:"return:a1";
  C.handle_msg c ~expect:"resolve";
  S.handle_msg s ~expect:"return:a2";
  C.handle_msg c ~expect:"finish";
  S.handle_msg s ~expect:"finish";
  S.handle_msg s ~expect:"release";
  dec_ref y;
  dec_ref to_server;
  CS.flush c s;
  CS.check_finished c s

let test_resolve_7 () =
  let service = Services.manual () in
  let c, s, bs = init_pair ~bootstrap_service:service in
  let promise, resolver = Local_struct_promise.make () in
  let bs_promise = promise#cap 0 in               (* Local promise *)
  let x1 = call_for_cap bs_promise "q1" [] in     (* Never resolves *)
  let x2 = call_for_cap bs_promise "q2" [] in     (* Will resolve to null *)
  let q3 = call x1 "q3" [x2] in
  let q4 = call bs "q4" [x2] in
  (* Resolving [bs_promise] to [bs] sends q1, q3 and q2 over the network.
     [x2] is marked as blocked on [bs_promise], but [bs_promise] is no longer blocked.
     Must not misinterpret this as [x2] being sender-hosted (not blocked on anything)! *)
  resolve_ok resolver "reply" [bs];
  Logs.info (fun f -> f "bs=%t" bs#pp);
  S.handle_msg s ~expect:"call:q4";
  let to_x2, a4 = service#pop1 "q4" in
  dec_ref to_x2;
  CS.flush c s;
  let a1 = service#pop0 "q1" in
  let a2 = service#pop0 "q2" in
  resolve_ok a2 "reply" [Core_types.null];
  CS.dump c s;
  CS.flush c s;
  (* Clean up *)
  resolve_ok a1 "a1" [];
  resolve_ok a4 "a4" [];
  dec_ref x1;
  dec_ref x2;
  dec_ref q3;
  dec_ref q4;
  dec_ref bs_promise;
  dec_ref promise;
  CS.flush c s;
  CS.check_finished c s

(* Returning an already-broken capability. *)
let test_broken_return () =
  let err = Exception.v "Broken" in
  let broken = Core_types.broken_cap err in
  let c, s = CS.create ~client_tags:Test_utils.client_tags ~server_tags:Test_utils.server_tags broken in
  let bs = C.bootstrap c in
  Alcotest.check (Alcotest.option exn) "Initially a promise" None bs#problem;
  S.handle_msg s ~expect:"bootstrap";
  C.handle_msg c ~expect:"return:(boot)";
  C.handle_msg c ~expect:"resolve";
  S.handle_msg s ~expect:"finish";
  Alcotest.check (Alcotest.option exn) "Resolves to broken" (Some err) bs#problem;
  dec_ref bs;
  CS.flush c s;
  CS.check_finished c s

let test_broken_call () =
  let err = Exception.v "Broken" in
  let broken = Core_types.broken_cap err in
  let service = Services.manual () in
  let c, s, bs = init_pair ~bootstrap_service:service in
  let q1 = call bs "q1" [broken] in
  S.handle_msg s ~expect:"call:q1";
  let broken_proxy, a1 = service#pop1 "q1" in
  Alcotest.check (Alcotest.option exn) "Initially a promise" None broken_proxy#problem;
  S.handle_msg s ~expect:"resolve";
  Alcotest.check (Alcotest.option exn) "Resolves to broken" (Some err) broken_proxy#problem;
  resolve_ok a1 "a1" [];
  dec_ref broken_proxy;
  dec_ref bs;
  dec_ref q1;
  CS.flush c s;
  CS.check_finished c s

(* Server returns a capability reference that later breaks. *)
let test_broken_later () =
  let err = Exception.v "Broken" in
  let broken = Core_types.broken_cap err in
  let promise = Cap_proxy.local_promise () in
  let c, s = CS.create ~client_tags:Test_utils.client_tags ~server_tags:Test_utils.server_tags promise in
  let bs = C.bootstrap c in
  Alcotest.check (Alcotest.option exn) "Initially a promise" None bs#problem;
  S.handle_msg s ~expect:"bootstrap";
  C.handle_msg c ~expect:"return:(boot)";
  S.handle_msg s ~expect:"finish";
  (* Server breaks promise *)
  promise#resolve broken;
  C.handle_msg c ~expect:"resolve";
  Alcotest.check (Alcotest.option exn) "Resolves to broken" (Some err) bs#problem;
  dec_ref bs;
  CS.flush c s;
  CS.check_finished c s

let test_broken_connection () =
  let service = Services.echo_service () in
  let c, s, bs = init_pair ~bootstrap_service:service in
  let q1 = call bs "Message-1" [] in
  CS.flush c s;
  Alcotest.check response_promise "Echo reply"
    (Some (Ok (Response.v "got:Message-1")))
    q1#response;
  dec_ref q1;
  let err = Exception.v "Connection lost" in
  C.disconnect c err;
  S.disconnect s err;
  Alcotest.check (Alcotest.option exn) "Resolves to broken" (Some err) bs#problem;
  dec_ref bs

let test_ref_counts () =
  let objects = Hashtbl.create 3 in
  let make () =
    let o = object (self)
      inherit Core_types.service
      val id = Capnp_rpc.Debug.OID.next ()
      method call results _ = Core_types.resolve_ok results (Response.v "answer")
      method! private release = Hashtbl.remove objects self
      method! pp f = Fmt.pf f "Service(%a, %t)" Capnp_rpc.Debug.OID.pp id self#pp_refcount
    end in
    Hashtbl.add objects o true;
    o
  in
  (* Test structs and fields *)
  let promise, resolver = Local_struct_promise.make () in
  let f0 = promise#cap 0 in
  f0#when_more_resolved dec_ref;
  let fields = [f0; promise#cap 1] in
  resolve_ok resolver "ok" [make (); make ()];
  let fields2 = [promise#cap 0; promise#cap 2] in
  dec_ref promise;
  List.iter dec_ref fields;
  List.iter dec_ref fields2;
  Alcotest.(check int) "Fields released" 0 (Hashtbl.length objects);
  (* With pipelining *)
  let promise, resolver = Local_struct_promise.make () in
  let f0 = promise#cap 0 in
  let q1 = call f0 "q1" [] in
  f0#when_more_resolved dec_ref;
  resolve_ok resolver "ok" [make ()];
  dec_ref f0;
  dec_ref promise;
  dec_ref q1;
  Alcotest.(check int) "Fields released" 0 (Hashtbl.length objects);
  (* Test local promise *)
  let promise = Cap_proxy.local_promise () in
  promise#when_more_resolved dec_ref;
  promise#resolve (make ());
  dec_ref promise;
  Alcotest.(check int) "Local promise released" 0 (Hashtbl.length objects);
  Gc.full_major ()

module Level0 = struct
  (* Client is level 0, server is level 1.
     We don't have a level 0 implementation, so we'll do it manually.
     Luckily, level 0 is very easy. *)

  type t = {
    from_server : [S.EP.Out.t | `Unimplemented of S.EP.In.t] Queue.t;
    to_server : [S.EP.In.t | `Unimplemented of S.EP.Out.t] Queue.t;
  }

  let send t m = Queue.add m t.to_server

  let qid_of_int x = S.EP.In.QuestionId.of_uint32 (Stdint.Uint32.of_int x)

  let init ~bootstrap =
    let from_server = Queue.create () in
    let to_server = Queue.create () in
    let c = { from_server; to_server } in
    let s = S.create ~tags:Test_utils.server_tags from_server to_server ~bootstrap in
    send c @@ `Bootstrap (qid_of_int 0, "");
    S.handle_msg s ~expect:"bootstrap";
    send c @@ `Finish (qid_of_int 0, false);
    S.handle_msg s ~expect:"finish";
    let bs =
      match Queue.pop from_server with
      | `Return (_, `Results (_, caps), false) ->
        begin match RO_array.get_exn caps 0 with
          | `SenderHosted id -> id
          | _ -> assert false
        end
      | _ -> assert false
    in
    c, s, bs

  let expect t expected =
    match Queue.pop t.from_server with
    | msg -> Alcotest.(check string) "Read message from server" expected (Testbed.Connection.summary_of_msg msg)
    | exception Queue.Empty -> Alcotest.fail "No messages found!"

  let expect_bs t =
    let bs_request = Queue.pop t.from_server in
    match bs_request with
    | `Bootstrap (qid, "") -> qid
    | _ -> Alcotest.fail (Fmt.str "Expecting bootstrap, got %s" (Testbed.Connection.summary_of_msg bs_request))

  let expect_call t expected =
    match Queue.pop t.from_server with
    | `Call (qid, _, msg, _, _) ->
      Alcotest.(check string) "Get call" expected @@ Request.data msg;
      qid
    | request -> Alcotest.fail (Fmt.str "Expecting call, got %s" (Testbed.Connection.summary_of_msg request))

  let call t target ~qid msg =
    send t @@ `Call (qid_of_int qid, `ReceiverHosted target, Request.v msg, RO_array.empty, `Caller)

  let finish t ~qid =
    send t @@ `Finish (qid_of_int qid, true)
end

(* Pretend that the peer only supports level 0, and therefore
   sets the auto-release flags. *)
let test_auto_release () =
  let service = Services.manual () in
  let c, s, bs = Level0.init ~bootstrap:service in
  let send = Level0.send c in
  (* Client makes a call. *)
  Level0.call c ~qid:0 bs "q0";
  S.handle_msg s ~expect:"call:q0";
  (* Server replies with some caps, which the client doesn't understand. *)
  let a0 = service#pop0 "q0" in
  let echo_service = Services.echo_service () in
  resolve_ok a0 "a0" [echo_service];
  Level0.expect c "return:a0";
  (* Client asks it to drop all caps *)
  Level0.finish c ~qid:0;
  S.handle_msg s ~expect:"finish";
  Alcotest.(check bool) "Echo released" true echo_service#released;
  (* Now test the other direction. Service invokes bootstap on client. *)
  let proxy_to_client = S.bootstrap s in
  let logger = Services.logger () in
  let q1 = call proxy_to_client "q1" [logger] in
  dec_ref logger;
  let bs_qid = Level0.expect_bs c in
  let client_bs_id = S.EP.In.ExportId.zero in
  send @@ `Return (bs_qid, `Results (Response.v "bs", RO_array.of_list [`SenderHosted client_bs_id]), true);
  let q1_qid = Level0.expect_call c "q1" in
  send @@ `Return (q1_qid, `Results (Response.v "a1", RO_array.empty), true);
  S.handle_msg s ~expect:"return:bs";
  S.handle_msg s ~expect:"return:a1";
  Alcotest.(check bool) "Logger released" true logger#released;
  dec_ref proxy_to_client;
  (* Clean up.
     A real level-0 client would just disconnect, but release cleanly so we can
     check nothing else was leaked. *)
  dec_ref q1;
  send @@ `Release (S.EP.Out.ExportId.zero, 1);
  S.handle_msg s ~expect:"release";
  try S.check_finished s ~name:"Server"
  with ex ->
    Logs.err (fun f -> f "Error: %a@\n%a" Fmt.exn ex S.dump s);
    raise ex

(* We send a resolve to a level 0 implementation, which echoes it back as
   "unimplemented". We release the cap. *)
let test_unimplemented () =
  let service = Services.manual () in
  let c, s, bs = Level0.init ~bootstrap:service in
  (* The client makes a call on [service] and gets back a promise. *)
  Level0.call c ~qid:0 bs "q0";
  S.handle_msg s ~expect:"call:q0";
  let a0 = service#pop0 "q0" in
  let promise = Cap_proxy.local_promise () in
  inc_ref promise;
  resolve_ok a0 "a0" [promise];
  (* The server resolves the promise *)
  let echo_service = Services.echo_service () in
  promise#resolve (echo_service :> Core_types.cap);
  dec_ref promise;
  (* The client doesn't understand the resolve message. *)
  Level0.expect c "return:a0";
  Level0.finish c ~qid:0;
  S.handle_msg s ~expect:"finish";
  let resolve =
    match Queue.pop c.from_server with
    | `Resolve _ as r -> r
    | _ -> assert false
  in
  Level0.send c @@ `Unimplemented resolve;
  S.handle_msg s ~expect:"unimplemented";
  (* The server releases the export. *)
  Alcotest.(check bool) "Echo released" true echo_service#released;
  (* The server tries to get the client's bootstrap object *)
  let bs = S.bootstrap s in
  let q2 = call bs "q2" [] in
  (* The client doesn't support bootstrap or call *)
  let bs_msg =
    match Queue.pop c.from_server with
    | `Bootstrap _ as bs -> bs
    | _ -> assert false
  in
  Level0.send c @@ `Unimplemented bs_msg;
  let call_msg =
    match Queue.pop c.from_server with
    | `Call _ as call -> call
    | _ -> assert false
  in
  Level0.send c @@ `Unimplemented call_msg;
  S.handle_msg s ~expect:"unimplemented";
  S.handle_msg s ~expect:"unimplemented";
  dec_ref bs;
  Alcotest.(check response_promise) "Server got error"
    (Some (Error (Error.exn ~ty:`Unimplemented "Call message not implemented by peer!")))
    q2#response;
  dec_ref q2;
  (* Clean up.
     A real level-0 client would just disconnect, but release cleanly so we can
     check nothing else was leaked. *)
  Level0.send c @@ `Release (S.EP.Out.ExportId.zero, 1);
  S.handle_msg s ~expect:"release";
  try S.check_finished s ~name:"Server"
  with ex ->
    Logs.err (fun f -> f "Error: %a@\n%a" Fmt.exn ex S.dump s);
    raise ex

(* The client's only reference to an import is a callback on the import itself.
   The import must not be released, even though the leak detector would normally
   do that. *)
let test_import_callbacks () =
  let service = Services.manual () in
  let c, s, bs = init_pair ~bootstrap_service:service in
  let q1 = call bs "q1" [] in
  S.handle_msg s ~expect:"call:q1";
  let a1 = service#pop0 "q1" in
  let promise = Cap_proxy.local_promise () in
  resolve_ok a1 "a1" [promise];
  C.handle_msg c ~expect:"return:a1";
  let ok =
    let r = ref "-" in
    let f1 = q1#cap 0 in
    f1#when_more_resolved (fun x ->
        r := "resolved";
        dec_ref x;
        dec_ref f1
      );
    r
  in
  dec_ref q1;
  Gc.full_major ();
  promise#resolve (Core_types.broken_cap (Capnp_rpc.Exception.v "broken"));
  CS.flush c s;
  dec_ref bs;
  Alcotest.(check string) "ok set" "resolved" !ok;
  CS.flush c s;
  CS.check_finished c s

let tests = [
  "Return",     `Quick, test_return;
  "Return error", `Quick, test_return_error;
  "Connection", `Quick, test_simple_connection;
  "Local embargo", `Quick, test_local_embargo;
  "Local embargo 2", `Quick, test_local_embargo_2;
  "Local embargo 3", `Quick, test_local_embargo_3;
  "Local embargo 4", `Quick, test_local_embargo_4;
  "Local embargo 5", `Quick, test_local_embargo_5;
  "Local embargo 6", `Quick, test_local_embargo_6;
  "Local embargo 7", `Quick, test_local_embargo_7;
  "Local embargo 8", `Quick, test_local_embargo_8;
  "Local embargo 9", `Quick, _test_local_embargo_9;
  "Local embargo 10", `Quick, test_local_embargo_10;
  "Local embargo 11", `Quick, test_local_embargo_11;
  "Local embargo 12", `Quick, test_local_embargo_12;
  "Local embargo 13", `Quick, test_local_embargo_13;
  "Local embargo 14", `Quick, test_local_embargo_14;
  "Local embargo 15", `Quick, test_local_embargo_15;
  "Shared cap", `Quick, test_share_cap;
  "Fields", `Quick, test_fields;
  "Cancel", `Quick, test_cancel;
  "Cancel 2", `Quick, test_cancel_2;
  "Cancel 3", `Quick, test_cancel_3;
  "Duplicates", `Quick, test_duplicates;
  "Re-export", `Quick, test_single_export;
  "Shorten field", `Quick, test_shorten_field;
  "Cycle", `Quick, test_cycle;
  "Cycle 2", `Quick, test_cycle_2;
  "Cycle 3", `Quick, test_cycle_3;
  "Cycle 4", `Quick, test_cycle_4;
  "Cycle 5", `Quick, test_cycle_5;
  "Cycle 6", `Quick, test_cycle_6;
  "Resolve", `Quick, test_resolve;
  "Resolve 2", `Quick, test_resolve_2;
  "Resolve 3", `Quick, test_resolve_3;
  "Resolve 4", `Quick, test_resolve_4;
  "Resolve 5", `Quick, test_resolve_5;
  "Resolve 6", `Quick, test_resolve_6;
  "Resolve 7", `Quick, test_resolve_7;
  "Ref-counts", `Quick, test_ref_counts;
  "Auto-release", `Quick, test_auto_release;
  "Unimplemented", `Quick, test_unimplemented;
  "Broken return", `Quick, test_broken_return;
  "Broken call", `Quick, test_broken_call;
  "Broken later", `Quick, test_broken_later;
  "Broken connection", `Quick, test_broken_connection;
  "Import callbacks", `Quick, test_import_callbacks;
] |> List.map (fun (name, speed, test) ->
    name, speed, (fun () ->
        Testbed.Capnp_direct.ref_leaks := 0;
        test ();
        Gc.full_major ();
        if !Testbed.Capnp_direct.ref_leaks > 0 then (
          Alcotest.fail "Reference leaks detected!";
        )
      )
  )

let () =
  Printexc.record_backtrace true;
  Alcotest.run ~and_exit:false "capnp-rpc" [
    "core", tests;
  ]
