open Astring

open Lwt.Infix

module Calc = Examples.Calc

let pp_addr f = function
  | `Unix path -> Fmt.pf f "unix:%s" path

let serve addr =
  let `Unix path = addr in
  begin match Unix.lstat path with
    | { Unix.st_kind = Unix.S_SOCK; _ } -> Unix.unlink path
    | _ -> ()
    | exception Unix.Unix_error(Unix.ENOENT, _, _) -> ()
  end;
  let socket = Unix.(socket PF_UNIX SOCK_STREAM 0) in
  Unix.bind socket (Unix.ADDR_UNIX path);
  Unix.listen socket 5;
  Fmt.pr "Waiting for connections on %a" pp_addr addr;
  let lwt_socket = Lwt_unix.of_unix_file_descr socket in
  let rec loop () =
    Lwt_unix.accept lwt_socket >>= fun (client, _addr) ->
    Logs.info (fun f -> f "New connection");
    let switch = Lwt_switch.create () in
    let ep = Capnp_rpc_lwt.Endpoint.of_socket ~switch (Lwt_unix.unix_file_descr client) in
    let calc = Calc.service in
    let _conn = Capnp_rpc_lwt.CapTP.connect ~switch ep ~offer:calc in
    loop ()
  in
  Lwt_main.run (loop ())

let connect (`Unix path) =
  Logs.info (fun f -> f "Connecting to %S..." path);
  let socket = Unix.(socket PF_UNIX SOCK_STREAM 0) in
  Unix.connect socket (Unix.ADDR_UNIX path);
  let switch = Lwt_switch.create () in
  let ep = Capnp_rpc_lwt.Endpoint.of_socket ~switch socket in
  let conn = Capnp_rpc_lwt.CapTP.connect ~switch ep in
  let calc = Capnp_rpc_lwt.CapTP.bootstrap conn in
  Lwt_main.run begin
    Logs.info (fun f -> f "Evaluating expression...");
    let remote_add = Calc.Client.getOperator calc `Add in
    let result = Calc.Client.evaluate calc Calc.(Call (remote_add, [Float 40.0; Float 2.0])) in
    Calc.Client.read result >>= fun v ->
    Fmt.pr "Result: %f@." v;
    Capnp_rpc_lwt.CapTP.disconnect conn (Capnp_rpc.Exception.v ~ty:`Disconnected "Bye!")
  end

let pp_qid f = function
  | None -> ()
  | Some x ->
    let s = Uint32.to_string x in
    Fmt.(styled `Magenta (fun f x -> Fmt.pf f " (qid=%s)" x)) f s

let reporter =
  let report src level ~over k msgf =
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?(tags=Logs.Tag.empty) fmt ->
    let qid = Logs.Tag.find Capnp_rpc.Debug.qid_tag tags in
    let print _ =
      Fmt.(pf stdout) "%a@." pp_qid qid;
      over ();
      k ()
    in
    Fmt.kpf print Fmt.stdout ("%a %a: @[" ^^ fmt ^^ "@]")
      Fmt.(styled `Magenta string) (Printf.sprintf "%11s" src)
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report = report }

open Cmdliner

let addr_of_string s =
  match String.cut ~sep:":" s with
  | None -> Error (`Msg "Missing ':'")
  | Some ("unix", path) -> Ok (`Unix path)
  | Some _ -> Error (`Msg "Only unix:PATH addresses are currently supported")

let addr_conv =
  Arg.conv (addr_of_string, pp_addr)

let addr_t =
  let i = Arg.info [] ~docv:"ADDR" ~doc:"e.g. unix:/path/socket or tcp:host:port" in
  Arg.(required @@ pos 0 (some addr_conv) None i)

let serve_cmd =
  Term.(const serve $ addr_t),
  let doc = "provide a Cap'n Proto calculator service" in
  Term.info "serve" ~doc

let connect_cmd =
  Term.(const connect $ addr_t),
  let doc = "connect to a Cap'n Proto calculator service" in
  Term.info "connect" ~doc

let default_cmd =
  let doc = "a calculator example" in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "calc" ~version:"v0.1" ~doc

let cmds = [serve_cmd; connect_cmd]

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter reporter;
  Logs.set_level ~all:true (Some Logs.Info);
  Term.(exit @@ eval_choice default_cmd cmds)
