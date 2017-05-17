open Capnp_rpc

let pp_actor f = function
  | `Client -> Fmt.(styled `Green (const string "client")) f ()
  | `Server -> Fmt.(styled `Red (const string "server")) f ()
  | `Unknown -> Fmt.(const string "------") f ()

let actor_tag = Logs.Tag.def "actor" pp_actor

let pp_qid f = function
  | None -> ()
  | Some x ->
    let s = Uint32.to_string x in
    Fmt.(styled `Magenta (fun f x -> Fmt.pf f " (qid=%s)" x)) f s

let reporter =
  let report src level ~over k msgf =
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?(tags=Logs.Tag.empty) fmt ->
    let actor =
      match Logs.Tag.find actor_tag tags with
      | Some x -> x
      | None -> `Unknown
    in
    let qid = Logs.Tag.find Connection.qid_tag tags in
    let print _ =
      Fmt.(pf stderr) "%a@." pp_qid qid;
      over ();
      k ()
    in
    Fmt.kpf print Fmt.stderr ("%a %a %a: @[" ^^ fmt ^^ "@]")
      Fmt.(styled `Magenta string) (Printf.sprintf "%11s" src)
      Logs_fmt.pp_header (level, header)
      pp_actor actor
  in
  { Logs.report = report }

let () =
  Logs.set_reporter reporter;
  Logs.set_level (Some Logs.Info)

(* Create a client/server pair, with the server providing [service].
   Return the client proxy to it.
   Everything gets shut down when the switch is turned off. *)
let run_server ~switch ~service () =
  let server_socket, client_socket = Unix.(socketpair PF_UNIX SOCK_STREAM 0) in
  let _server =
    let tags = Logs.Tag.(empty |> add actor_tag `Server) in
    Connection.of_endpoint ~tags ~switch ~offer:service (Endpoint.of_socket ~switch server_socket)
  in
  let client =
    let tags = Logs.Tag.(empty |> add actor_tag `Client) in
    Connection.of_endpoint ~switch ~tags (Endpoint.of_socket ~switch client_socket)
  in
  Connection.bootstrap client

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
