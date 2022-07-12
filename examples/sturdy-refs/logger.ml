module Api = Api.MakeRPC(Capnp_rpc_lwt)

open Capnp_rpc_lwt

let local label =
  let module Logger = Api.Service.Logger in
  Logger.local @@ object
    inherit Logger.service

    method log_impl params release_param_caps =
      let open Logger.Log in
      let msg = Params.msg_get params in
      release_param_caps ();
      Printf.printf "[server] %S says %S\n%!" label msg;
      Service.return_empty ()
  end

module Logger = Api.Client.Logger

let log t msg =
  let open Logger.Log in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.msg_set params msg;
  Capability.call_for_unit_exn t method_id request
