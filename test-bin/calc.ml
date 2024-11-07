open Eio.Std

module Vat = Capnp_rpc_unix.Vat
module Calc = Testlib.Calc

(* Verbose logging *)

let pp_qid f = function
  | None -> ()
  | Some x ->
    let s = Stdint.Uint32.to_string x in
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

(* Run as server *)

let serve vat_config =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let service_id = Capnp_rpc_net.Restorer.Id.public "" in
  let service = Calc.local ~sw in
  let restore = Capnp_rpc_net.Restorer.single service_id service in
  let vat = Capnp_rpc_unix.serve ~sw ~net:env#net ~restore vat_config in
  let sr = Vat.sturdy_uri vat service_id in
  traceln "Waiting for incoming connections at:@.%a" Uri.pp_hum sr;
  Fiber.await_cancel ()

(* Run as client *)

let connect addr =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let vat = Capnp_rpc_unix.client_only_vat ~sw env#net in
  let sr = Vat.import_exn vat addr in
  Capnp_rpc_unix.with_cap_exn sr @@ fun calc ->
  Logs.info (fun f -> f "Evaluating expression...");
  let remote_add = Calc.getOperator calc `Add in
  let result = Calc.evaluate calc Calc.Expr.(Call (remote_add, [Float 40.0; Float 2.0])) in
  let v = Calc.Value.read result in
  traceln "Result: %f" v

(* Command-line parsing *)

open Cmdliner

let connect_addr =
  let i = Arg.info [] ~docv:"ADDR" ~doc:"Address of server (capnp://...)" in
  Arg.(required @@ pos 0 (some Capnp_rpc_unix.sturdy_uri) None i)

let serve_cmd =
  let doc = "provide a Cap'n Proto calculator service" in
  let info = Cmd.info "serve" ~doc in
  Cmd.v info Term.(const serve $ Capnp_rpc_unix.Vat_config.cmd)

let connect_cmd =
  let doc = "connect to a Cap'n Proto calculator service" in
  let info = Cmd.info "connect" ~doc in
  Cmd.v info Term.(const connect $ connect_addr)

let cmds = [serve_cmd; connect_cmd]

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter reporter;
  Logs.set_level ~all:true (Some Logs.Info);
  Logs.Src.list () |> List.iter (fun src ->
      if Astring.String.is_prefix ~affix:"capnp" (Logs.Src.name src) then
        Logs.Src.set_level src (Some Logs.Debug);
    );
  let doc = "a calculator example" in
  let info = Cmd.info "calc" ~version:"v0.1" ~doc in
  match Cmd.eval ~catch:false (Cmd.group info cmds) with
  | exception Failure msg -> Fmt.epr "%s@." msg; exit 1
  | status -> exit status
