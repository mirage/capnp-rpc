open Astring

module Core_types = Testbed.Capnp_direct.Core_types
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

let empty = RO_array.empty

let dec_ref x = x#dec_ref

let error = Alcotest.of_pp Capnp_rpc.Error.pp
let pp_cap f p = p#pp f
let cap : Core_types.cap Alcotest.testable = Alcotest.of_pp pp_cap
let ro_array x = Alcotest.testable (RO_array.pp (Alcotest.pp x)) (RO_array.equal (Alcotest.equal x))
let response_promise = Alcotest.(option (result (pair string (ro_array cap)) error))

let exn = Alcotest.of_pp Capnp_rpc.Exception.pp

(* Takes ownership of caps *)
let resolve_ok (ans:#Core_types.struct_resolver) msg caps =
  let caps = List.map (fun x -> (x :> Core_types.cap)) caps in
  ans#resolve (Ok (msg, RO_array.of_list caps))

let test_simple_connection () =
  let c, s = CS.create ~client_tags:Test_utils.client_tags ~server_tags:Test_utils.server_tags (Services.echo_service ()) in
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
  let c, s = CS.create ~client_tags:Test_utils.client_tags ~server_tags:Test_utils.server_tags bootstrap_service in
  let bs = C.bootstrap c in
  S.handle_msg s ~expect:"bootstrap";
  C.handle_msg c ~expect:"return:(boot)";
  S.handle_msg s ~expect:"finish";
  c, s, bs

let call target msg caps =
  let caps = List.map (fun x -> (x :> Core_types.cap)) caps in
  List.iter (fun c -> c#inc_ref) caps;
  target#call msg (RO_array.of_list caps)

(* The server gets an object and then sends it back. When the object arrives back
   at the client, it must be the original (local) object, not a proxy. *)
let test_return () =
  let c, s, bs = init_pair ~bootstrap_service:(Services.echo_service ()) in
  (* Pass callback *)
  let slot = ref ("empty", empty) in
  let local = Services.swap_service slot in
  let q = call bs "c1" [local] in
  local#dec_ref;
  (* Server echos args back *)
  S.handle_msg s ~expect:"call:c1";
  C.handle_msg c ~expect:"return:got:c1";
  Alcotest.(check response_promise) "Client got response"
    (Some (Ok ("got:c1", RO_array.of_list [(local :> Core_types.cap)])))
    q#response;
  bs#dec_ref;
  S.handle_msg s ~expect:"finish";
  S.handle_msg s ~expect:"release";
  C.handle_msg c ~expect:"release";
  q#finish;
  CS.check_finished c s

let test_return_error () =
  let c, s, bs = init_pair ~bootstrap_service:(Core_types.broken_cap (Exception.v "test-error")) in
  (* Pass callback *)
  let slot = ref ("empty", empty) in
  let local = Services.swap_service slot in
  let q = call bs "call" [local] in
  local#dec_ref;
  (* Server echos args back *)
  CS.flush c s;
  Alcotest.(check response_promise) "Client got response" (Some (Error (Error.exn "test-error"))) q#response;
  q#finish;
  bs#dec_ref;
  CS.flush c s;
  CS.check_finished c s

let test_share_cap () =
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
  let c, s, bs = init_pair ~bootstrap_service:(Services.echo_service ()) in
  let local = Services.logger () in
  let q = call bs "Get service" [(local :> Core_types.cap)] in
  let service = q#cap 0 in
  let _ = service#call "Message-1" empty in
  S.handle_msg s ~expect:"call:Get service";
  C.handle_msg c ~expect:"return:got:Get service";
  q#finish;
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
  local#dec_ref;
  bs#dec_ref;
  service#dec_ref;
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
  resolve_ok a2 "a2" [local];
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
  local_reg#dec_ref;
  CS.flush c s;
  CS.check_finished c s

(* Embargo on a resolve message *)
let test_local_embargo_3 () =
  let service = Services.manual () in
  let c, s, bs = init_pair ~bootstrap_service:service in
  let local = Services.logger () in
  let q1 = call bs "q1" [local] in
  S.handle_msg s ~expect:"call:q1";
  let (_, q1_args, a1) = service#pop in
  let proxy_to_logger = RO_array.get q1_args 0 in
  let promise = Cap_proxy.local_promise () in
  resolve_ok a1 "a1" [promise];
  C.handle_msg c ~expect:"return:a1";
  let service = q1#cap 0 in
  let _ = service#call "Message-1" empty in
  promise#resolve proxy_to_logger;
  C.handle_msg c ~expect:"resolve";
  (* We've received the resolve message, so we know that [service] is local,
     but the pipelined message we sent to it via [s] hasn't arrived yet. *)
  let _ = service#call "Message-2" empty in
  S.handle_msg s ~expect:"finish";
  S.handle_msg s ~expect:"call:Message-1";
  C.handle_msg c ~expect:"call:Message-1";            (* Gets pipelined message back *)
  S.handle_msg s ~expect:"disembargo-request";
  C.handle_msg c ~expect:"disembargo-reply";
  Alcotest.(check string) "Pipelined arrived first" "Message-1" local#pop;
  Alcotest.(check string) "Embargoed arrived second" "Message-2" local#pop;
  (* Clean up *)
  local#dec_ref;
  q1#finish;
  bs#dec_ref;
  service#dec_ref;
  CS.flush c s;
  CS.check_finished c s

(* Embargo an local answer that doesn't have the specified cap. *)
let test_local_embargo_4 () =
  let service = Services.manual () in
  let c, s, bs = init_pair ~bootstrap_service:service in
  let local = Services.echo_service () in
  let q1 = call bs "q1" [local] in
  let broken = q1#cap 0 in
  let _ = call broken "pipeline" [] in
  S.handle_msg s ~expect:"call:q1";
  let (_, q1_args, a1) = service#pop in
  let proxy_to_local = RO_array.get q1_args 0 in
  let q2 = call proxy_to_local "q2" [] in
  a1#resolve (Ok ("a1", RO_array.of_list [q2#cap 0]));
  C.handle_msg c ~expect:"call:q2";
  C.handle_msg c ~expect:"return:a1";
  (* At this point, the client knows that [broken] is its own answer to [q2], which is an error.
     It therefore does not try to disembargo it. *)
  Alcotest.(check string) "Error not embargoed"
    "Failed: Invalid cap index 0 in []"
   (Fmt.strf "%t" broken#shortest#pp);
  (* Clean up *)
  local#dec_ref;
  proxy_to_local#dec_ref;
  q1#finish;
  bs#dec_ref;
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
  let _ = call test "Message-1" [] in
  S.handle_msg s ~expect:"call:q1";
  let (_, q1_args, a1) = service#pop in
  let proxy_to_local = RO_array.get q1_args 0 in
  let server_promise = Cap_proxy.local_promise () in
  resolve_ok a1 "a1" [server_promise];
  C.handle_msg c ~expect:"return:a1";
  (* [test] is now known to be at [service]; no embargo needed.
     The server now resolves it to a client service. *)
  server_promise#resolve proxy_to_local;
  C.handle_msg c ~expect:"resolve";
  let _ = call test "Message-2" [] in
  CS.flush c s;
  Alcotest.(check string) "Pipelined arrived first" "Message-1" local#pop;
  Alcotest.(check string) "Embargoed arrived second" "Message-2" local#pop;
  CS.flush c s;
  (* Clean up *)
  local#dec_ref;
  test#dec_ref;
  q1#finish;
  bs#dec_ref;
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
  let q1 = call bs "q1" [local] in
  let target = q1#cap 0 in
  q1#finish;
  let _ = call target "Message-1" [] in
  S.handle_msg s ~expect:"call:q1";
  let proxy_to_local, a1 = service#pop1 "q1" in
  (* Server makes a call on [local] and uses that promise to answer [q1]. *)
  let q2 = call proxy_to_local "q2" [] in
  resolve_ok a1 "a1" [q2#cap 0];
  C.handle_msg c ~expect:"call:q2";
  S.handle_msg s ~expect:"call:Message-1";      (* Forwards pipelined call back to the client *)
  (* Client resolves a2 to [service]. *)
  let a2 = local#pop0 "q2" in
  resolve_ok a2 "a2" [bs];
  S.handle_msg s ~expect:"return:a2";
  (* Client gets results from q1 - need to embargo it until we've forwarded the pipelined message
     back to the server. *)
  C.handle_msg c ~expect:"return:a1";
  Logs.info (fun f -> f "target = %t" target#pp);
  let _ = call target "Message-2" [] in         (* Client tries to send message-2, but it gets embargoed *)
  target#dec_ref;
  S.handle_msg s ~expect:"disembargo-request";
  S.handle_msg s ~expect:"finish";              (* Finish for q1 *)
  C.handle_msg c ~expect:"call:Message-1";      (* Pipelined message-1 arrives at client *)
  C.handle_msg c ~expect:"disembargo-request";  (* (the server is doing its own embargo on q2) *)
  S.handle_msg s ~expect:"call:Message-1";
  S.handle_msg s ~expect:"disembargo-reply";    (* (the server is doing its own embargo on q2) *)
  C.handle_msg c ~expect:"disembargo-reply";    (* XXX: disembargo for finished answer! *)
  S.handle_msg s ~expect:"call:Message-2";
  let m1 = service#pop0 "Message-1" in
  let m2 = service#pop0 "Message-2" in
  resolve_ok m1 "m1" [];
  resolve_ok m2 "m2" [];
  q2#finish;
  proxy_to_local#dec_ref;
  local#dec_ref;
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
  q1#finish;
  let _ = call target "Message-1" [] in
  S.handle_msg s ~expect:"call:q1";
  let proxy_to_local, a1 = service#pop1 "q1" in
  (* Server makes a call on [local] and uses that promise to answer [q1]. *)
  let q2 = call proxy_to_local "q2" [] in
  resolve_ok a1 "a1" [q2#cap 0];
  q2#finish;
  C.handle_msg c ~expect:"call:q2";
  S.handle_msg s ~expect:"call:Message-1";      (* Forwards pipelined call back to the client *)
  (* Client resolves a2 to a local promise. *)
  let client_promise = Cap_proxy.local_promise () in
  let a2 = local#pop0 "q2" in
  client_promise#inc_ref;
  resolve_ok a2 "a2" [client_promise];
  (* Client gets answer to a1 and sends disembargo. *)
  C.handle_msg c ~expect:"return:a1";
  let _ = call target "Message-2" [] in
  S.handle_msg s ~expect:"return:a2";
  (* At this point, the server's answer to q1 is a switchable, because it expects the client
     to resolve the promise at some point in the future. *)
  S.handle_msg s ~expect:"disembargo-request";
  C.handle_msg c ~expect:"call:Message-1";      (* Pipelined message-1 arrives at client *)
  C.handle_msg c ~expect:"disembargo-reply";
  let client_logger = Services.logger () in
  client_logger#inc_ref;
  client_promise#resolve (client_logger :> Core_types.cap);
  client_promise#dec_ref;
  CS.flush c s;
  Alcotest.(check string) "Pipelined arrived first" "Message-1" client_logger#pop;
  Alcotest.(check string) "Embargoed arrived second" "Message-2" client_logger#pop;
  client_logger#dec_ref;
  proxy_to_local#dec_ref;
  local#dec_ref;
  bs#dec_ref;
  target#dec_ref;
  CS.flush c s;
  CS.check_finished c s

(* The client tries to disembargo a resolved local switchable. *)
let test_local_embargo_8 () =
  let service = Services.manual () in
  let c, s, bs = init_pair ~bootstrap_service:service in
  let local = Services.manual () in
  (* Client calls the server, giving it [local]. *)
  let q1 = call bs "q1" [local] in
  let target = q1#cap 0 in
  q1#finish;
  let _ = call target "Message-1" [] in
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
  S.handle_msg s ~expect:"call:Message-1";      (* Forwards pipelined call back to the client *)
  S.handle_msg s ~expect:"return:a2";
  resolve_ok a1 "a1" [q2#cap 0];
  q2#finish;
  C.handle_msg c ~expect:"finish";
  (* The client resolves the local promise to a remote one *)
  let q3 = call bs "q3" [] in
  let remote_promise = q3#cap 0 in
  local_promise#resolve remote_promise;
  (* Client gets answer to a1 and sends disembargo. *)
  C.handle_msg c ~expect:"call:Message-1";      (* Forwards pipelined call back to the server again *)
  S.handle_msg s ~expect:"call:q3";
  S.handle_msg s ~expect:"resolve";
  S.handle_msg s ~expect:"call:Message-1";
  CS.dump c s;
  C.handle_msg c ~expect:"return:a1";
  (* We should now know that [target] is local, but will have embargoed it. *)
  let _ = call target "Message-2" [] in
  S.handle_msg s ~expect:"disembargo-request";
  C.handle_msg c ~expect:"disembargo-request"; (* Server is also doing its own embargo *)
  S.handle_msg s ~expect:"finish";
  C.handle_msg c ~expect:"disembargo-reply";
  S.handle_msg s ~expect:"disembargo-reply";
  let logger = Services.logger () in
  let a3 = service#pop0 "q3" in
  logger#inc_ref;
  resolve_ok a3 "a3" [logger];
  C.handle_msg c ~expect:"release";
  C.handle_msg c ~expect:"return:logged";
  S.handle_msg s ~expect:"call:Message-2";
  Alcotest.(check string) "Pipelined arrived first" "Message-1" logger#pop;
  Alcotest.(check string) "Embargoed arrived second" "Message-2" logger#pop;
  q3#finish;
  target#dec_ref;
  proxy_to_local#dec_ref;
  logger#dec_ref;
  bs#dec_ref;
  local#dec_ref;
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
  Alcotest.(check response_promise) "Echo response" (Some (Ok ("got:c1", empty))) q1#response;
  q1#finish;
  let q2 = call f0 "c2" [] in
  CS.flush c s;
  Alcotest.(check response_promise) "Echo response 2" (Some (Ok ("got:c2", empty))) q2#response;
  f0#dec_ref;
  CS.flush c s;
  CS.check_finished c s

let test_cancel () =
  let service = Services.manual () in
  let c, s = CS.create ~client_tags:Test_utils.client_tags ~server_tags:Test_utils.server_tags
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

(* Asking for the same field twice gives the same object. *)
let test_duplicates () =
  let service = Services.manual () in
  let c, s = CS.create ~client_tags:Test_utils.client_tags ~server_tags:Test_utils.server_tags
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
  q1#finish;
  q2#finish;
  let ignore msg =
    let got, caps, a = service#pop in
    Alcotest.(check string) ("Ignore " ^ msg) msg got;
    RO_array.iter dec_ref caps;
    resolve_ok a "a" []
  in
  ignore "q1";
  ignore "q2";
  local#dec_ref;
  bs#dec_ref;
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
  direct_to_logger#dec_ref;
  bs#dec_ref;
  proxy_to_logger#dec_ref;
  q1#finish;
  q2#finish;
  CS.flush c s;
  CS.check_finished c s

let ensure_is_cycle_error (x:#Core_types.struct_ref) : unit =
  match x#response with
  | Some (Error (`Exception ex))
    when (String.is_prefix ~affix:"Attempt to create a cycle detected:" ex.Exception.reason) -> ()
  | _ -> Alcotest.fail (Fmt.strf "Not a cycle error: %t" x#pp)

let ensure_is_cycle_error_cap cap =
  match cap#problem with
  | Some ex when (String.is_prefix ~affix:"<cycle: " ex.Exception.reason) -> ()
  | _ -> Alcotest.fail (Fmt.strf "Not a cycle error: %t" cap#pp)

let test_cycle () =
  (* Cap cycles *)
  let module P = Testbed.Capnp_direct.Cap_proxy in
  let p1 = P.local_promise () in
  let p2 = P.local_promise () in
  p1#resolve (p2 :> Core_types.cap);
  p2#resolve (p1 :> Core_types.cap);
  ensure_is_cycle_error (call p2 "test" []);
  (* Connect struct to its own field *)
  let p1 = Local_struct_promise.make () in
  let c = p1#cap 0 in
  p1#resolve (Ok ("msg", RO_array.of_list [c]));
  ensure_is_cycle_error_cap c;
  (* Connect struct to itself *)
  let p1 = Local_struct_promise.make () in
  p1#connect (p1 :> Core_types.struct_ref);
  ensure_is_cycle_error p1

(* Resolve a promise with an answer that includes the result of a pipelined
   call on the promise itself. *)
let test_cycle_2 () =
  let s1 = Local_struct_promise.make () in
  let s2 = call (s1#cap 0) "get-s2" [] in
  s1#resolve (Ok ("a7", RO_array.of_list [s2#cap 0]));
  ensure_is_cycle_error_cap (s1#cap 0)

(* It's not a cycle if one field resolves to another. *)
let test_cycle_3 () =
  let echo = Services.echo_service () in
  let a1 = Local_struct_promise.make () in
  resolve_ok a1 "a1" [a1#cap 1; (echo :> Core_types.cap)];
  let target = a1#cap 1 in
  let q2 = call target "q2" [] in
  Alcotest.(check response_promise) "Field 1 OK"
    (Some (Ok ("got:q2", RO_array.empty)))
    q2#response;
  target#dec_ref;
  a1#finish

(* The server returns an answer containing a promise. Later, it resolves the promise
   to a resource at the client. The client must be able to invoke the service locally. *)
let test_resolve () =
  let service = Services.manual () in
  let client_logger = Services.logger () in
  let c, s, proxy_to_service = init_pair ~bootstrap_service:service in
  (* The client makes a call and gets a reply, but the reply contains a promise. *)
  let q1 = call proxy_to_service "q1" [client_logger] in
  proxy_to_service#dec_ref;
  S.handle_msg s ~expect:"call:q1";
  let proxy_to_logger, a1 = service#pop1 "q1" in
  let promise = Cap_proxy.local_promise () in
  promise#inc_ref;
  resolve_ok a1 "a1" [promise];
  C.handle_msg c ~expect:"return:a1";
  (* The server now resolves the promise *)
  promise#resolve proxy_to_logger;
  promise#dec_ref;
  CS.flush c s;
  (* The client can now use the logger directly *)
  let x = q1#cap 0 in
  let q2 = call x "test-message" [] in
  Alcotest.(check string) "Got message directly" "test-message" client_logger#pop;
  x#dec_ref;
  q1#finish;
  q2#finish;
  client_logger#dec_ref;
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
  client_logger#dec_ref;
  proxy_to_service#dec_ref;
  S.handle_msg s ~expect:"call:q1";
  let proxy_to_logger, a1 = service#pop1 "q1" in
  let promise = Cap_proxy.local_promise () in
  resolve_ok a1 "a1" [promise];
  C.handle_msg c ~expect:"return:a1";
  (* The client doesn't care about the result and releases it *)
  q1#finish;
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
  a1_promise#inc_ref;
  resolve_ok a1 "a1" [a1_promise];
  C.handle_msg c ~expect:"return:a1";
  q1#finish;
  S.handle_msg s ~expect:"finish";
  S.handle_msg s ~expect:"release";
  (* Make another call, get a settled export this time. *)
  let q2 = call proxy_to_service "q2" [] in
  S.handle_msg s ~expect:"call:q2";
  CS.flush c s;
  let a2 = service#pop0 "q2" in
  let echo = Services.echo_service () in
  echo#inc_ref;
  resolve_ok a2 "a2" [echo];
  C.handle_msg c ~expect:"return:a2";
  (* Service now resolves first answer *)
  a1_promise#resolve (echo :> Core_types.cap);
  a1_promise#dec_ref;
  proxy_to_service#dec_ref;
  CS.flush c s;
  q2#finish;
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
  broken_proxy#dec_ref;
  bs#dec_ref;
  q1#finish;
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
  CS.flush c s;
  CS.check_finished c s

let test_broken_connection () =
  let service = Services.echo_service () in
  let c, s, bs = init_pair ~bootstrap_service:service in
  let q1 = call bs "Message-1" [] in
  CS.flush c s;
  Alcotest.check response_promise "Echo reply"
    (Some (Ok ("got:Message-1", RO_array.empty)))
    q1#response;
  q1#finish;
  let err = Exception.v "Connection lost" in
  C.disconnect c err;
  S.disconnect s err;
  Alcotest.check (Alcotest.option exn) "Resolves to broken" (Some err) bs#problem

let test_ref_counts () =
  let objects = Hashtbl.create 3 in
  let make () =
    let o = object (self)
      inherit Core_types.service
      val id = Capnp_rpc.Debug.OID.next ()
      method call _ _  = Core_types.return ("answer", RO_array.empty)
      method! private release = Hashtbl.remove objects self
      method! pp f = Fmt.pf f "Service(%a, %t)" Capnp_rpc.Debug.OID.pp id self#pp_refcount
    end in
    Hashtbl.add objects o true;
    o
  in
  (* Test structs and fields *)
  let promise = Local_struct_promise.make () in
  let f0 = promise#cap 0 in
  f0#when_more_resolved dec_ref;
  let fields = [f0; promise#cap 1] in
  resolve_ok promise "ok" [make (); make ()];
  let fields2 = [promise#cap 0; promise#cap 2] in
  promise#finish;
  List.iter dec_ref fields;
  List.iter dec_ref fields2;
  Alcotest.(check int) "Fields released" 0 (Hashtbl.length objects);
  (* With pipelining *)
  let promise = Local_struct_promise.make () in
  let f0 = promise#cap 0 in
  let q1 = call f0 "q1" [] in
  f0#when_more_resolved dec_ref;
  resolve_ok promise "ok" [make ()];
  f0#dec_ref;
  promise#finish;
  q1#finish;
  Alcotest.(check int) "Fields released" 0 (Hashtbl.length objects);
  (* Test local promise *)
  let promise = Cap_proxy.local_promise () in
  promise#when_more_resolved dec_ref;
  promise#resolve (make ());
  promise#dec_ref;
  Alcotest.(check int) "Local promise released" 0 (Hashtbl.length objects);
  (* Test embargo *)
  let embargo = Cap_proxy.embargo (make ()) in
  embargo#when_more_resolved dec_ref;
  embargo#disembargo;
  embargo#dec_ref;
  Alcotest.(check int) "Disembargo released" 0 (Hashtbl.length objects);
  (* Test embargo without disembargo *)
  let embargo = Cap_proxy.embargo (make ()) in
  embargo#when_more_resolved dec_ref;
  embargo#dec_ref;
  Alcotest.(check int) "Embargo released" 0 (Hashtbl.length objects);
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

  let qid_of_int x = S.EP.In.QuestionId.of_uint32 (Uint32.of_int x)

  let init ~bootstrap =
    let from_server = Queue.create () in
    let to_server = Queue.create () in
    let c = { from_server; to_server } in
    let s = S.create ~tags:Test_utils.server_tags from_server to_server ~bootstrap in
    bootstrap#dec_ref;
    send c @@ `Bootstrap (qid_of_int 0);
    S.handle_msg s ~expect:"bootstrap";
    send c @@ `Finish (qid_of_int 0, false);
    S.handle_msg s ~expect:"finish";
    let bs =
      match Queue.pop from_server with
      | `Return (_, `Results (_, caps), false) ->
        begin match RO_array.get caps 0 with
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
    | `Bootstrap qid -> qid
    | _ -> Alcotest.fail (Fmt.strf "Expecting bootstrap, got %s" (Testbed.Connection.summary_of_msg bs_request))

  let expect_call t expected =
    match Queue.pop t.from_server with
    | `Call (qid, _, msg, _) ->
      Alcotest.(check string) "Get call" expected msg;
      qid
    | request -> Alcotest.fail (Fmt.strf "Expecting call, got %s" (Testbed.Connection.summary_of_msg request))

  let call t target ~qid msg =
    send t @@ `Call (qid_of_int qid, `ReceiverHosted target, msg, RO_array.empty)

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
  let _q1 = call proxy_to_client "q1" [logger] in
  logger#dec_ref;
  let bs_qid = Level0.expect_bs c in
  let client_bs_id = S.EP.In.ExportId.zero in
  send @@ `Return (bs_qid, `Results ("bs", RO_array.of_list [`SenderHosted client_bs_id]), true);
  let q1_qid = Level0.expect_call c "q1" in
  send @@ `Return (q1_qid, `Results ("a1", RO_array.empty), true);
  S.handle_msg s ~expect:"return:bs";
  S.handle_msg s ~expect:"return:a1";
  Alcotest.(check bool) "Logger released" true logger#released;
  proxy_to_client#dec_ref;
  (* Clean up.
     A real level-0 client would just disconnect, but release cleanly so we can
     check nothing else was leaked. *)
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
  promise#inc_ref;
  resolve_ok a0 "a0" [promise];
  (* The server resolves the promise *)
  let echo_service = Services.echo_service () in
  promise#resolve (echo_service :> Core_types.cap);
  promise#dec_ref;
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
  bs#dec_ref;
  Alcotest.(check response_promise) "Server got error"
    (Some (Error (Error.exn ~ty:`Unimplemented "Call message not implemented by peer!")))
    q2#response;
  q2#finish;
  (* Clean up.
     A real level-0 client would just disconnect, but release cleanly so we can
     check nothing else was leaked. *)
  Level0.send c @@ `Release (S.EP.Out.ExportId.zero, 1);
  S.handle_msg s ~expect:"release";
  try S.check_finished s ~name:"Server"
  with ex ->
    Logs.err (fun f -> f "Error: %a@\n%a" Fmt.exn ex S.dump s);
    raise ex

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
  "Shared cap", `Quick, test_share_cap;
  "Fields", `Quick, test_fields;
  "Cancel", `Quick, test_cancel;
  "Duplicates", `Quick, test_duplicates;
  "Re-export", `Quick, test_single_export;
  "Shorten field", `Quick, test_shorten_field;
  "Cycle", `Quick, test_cycle;
  "Cycle 2", `Quick, test_cycle_2;
  "Cycle 3", `Quick, test_cycle_3;
  "Resolve", `Quick, test_resolve;
  "Resolve 2", `Quick, test_resolve_2;
  "Resolve 3", `Quick, test_resolve_3;
  "Ref-counts", `Quick, test_ref_counts;
  "Auto-release", `Quick, test_auto_release;
  "Unimplemented", `Quick, test_unimplemented;
  "Broken return", `Quick, test_broken_return;
  "Broken call", `Quick, test_broken_call;
  "Broken later", `Quick, test_broken_later;
  "Broken connection", `Quick, test_broken_connection;
] |> List.map (fun (name, speed, test) ->
    name, speed, (fun () -> test (); Gc.full_major ())
  )

let () =
  Printexc.record_backtrace true;
  Alcotest.run ~and_exit:false "capnp-rpc" [
    "core", tests;
  ]
