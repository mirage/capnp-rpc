open Lwt.Infix

include Capnp_rpc.Service

(* A convenient way to implement a simple blocking local function, where
   pipelining is not supported (messages sent to the result promise will be
   queued up at this host until it returns). *)
let return_lwt fn =
  Lwt_eio.run_lwt @@ fun () ->
  fn () >|= function
  | Ok resp      -> return resp
  | Error (`Capnp e) -> error e

let fail_lwt ?ty fmt =
  fmt |> Fmt.kstr @@ fun msg ->
  Lwt_result.fail (`Capnp (`Exception (Capnp_rpc.Exception.v ?ty msg)))
