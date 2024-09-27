open Eio.Std

let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  Eio_main.run @@ fun _ ->
  let service = Echo.local in
  let reply = Echo.ping service "foo" in
  traceln "Got reply %S" reply
