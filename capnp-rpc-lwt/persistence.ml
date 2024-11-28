open Lwt.Infix

class type ['a] persistent = object
  method save : ('a Sturdy_ref.t, Capnp_rpc.Exception.t) result Lwt.t
end

let with_persistence persistent ty obj =
  let wrapped = object
    method save = Lwt_eio.run_lwt (fun () -> persistent#save)
  end in
  Capnp_rpc.Persistence.with_persistence wrapped ty obj

let with_sturdy_ref = Capnp_rpc.Persistence.with_sturdy_ref

let save cap =
  Lwt_eio.run_eio @@ fun () ->
  Capnp_rpc.Persistence.save cap

let save_exn cap =
  save cap >>= function
  | Error (`Capnp e) -> Lwt.fail_with (Fmt.to_to_string Capnp_rpc.Error.pp e)
  | Ok x -> Lwt.return x
