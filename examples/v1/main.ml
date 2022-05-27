open Lwt.Infix

let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  Lwt_main.run begin
    let service = Echo.local in
    Echo.ping service "foo" >>= fun reply ->
    Fmt.pr "Got reply %S@." reply;
    Lwt.return_unit
  end
