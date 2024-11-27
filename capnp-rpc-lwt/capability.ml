include Capnp_rpc.Capability

open Lwt.Infix

let with_ref t fn =
  Lwt.finalize
    (fun () -> fn t)
    (fun () -> dec_ref t; Lwt.return_unit)

let await_settled t =
  Lwt_eio.run_eio @@ fun () -> await_settled t

let await_settled_exn t =
  Lwt_eio.run_eio @@ fun () -> await_settled_exn t

let call_and_wait cap m req =
  Lwt_eio.run_eio @@ fun () -> call_and_wait cap m req

let call_for_value cap m req =
  Lwt_eio.run_eio @@ fun () -> call_for_value cap m req

let call_for_value_exn cap m req =
  Lwt_eio.run_eio @@ fun () -> call_for_value_exn cap m req

let call_for_unit cap m req =
  call_for_value cap m req >|= function
  | Ok _ -> Ok ()
  | Error _ as e -> e

let call_for_unit_exn cap m req = call_for_value_exn cap m req >|= ignore
