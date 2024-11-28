include Capnp_rpc.Sturdy_ref

open Lwt.Infix

let connect t =
  Lwt_eio.run_eio @@ fun () ->
  connect t

let connect_exn t =
  connect t >>= function
  | Ok x -> Lwt.return x
  | Error e -> Lwt.fail_with (Fmt.to_to_string Capnp_rpc.Exception.pp e)

let with_cap t f =
  connect t >>= function
  | Ok x -> Capability.with_ref x f
  | Error e -> Lwt_result.fail (`Capnp e)

let with_cap_exn t f =
  connect_exn t >>= fun x ->
  Capability.with_ref x f
