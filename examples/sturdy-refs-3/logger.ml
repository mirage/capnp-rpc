module Api = Api.MakeRPC(Capnp_rpc_lwt)

open Capnp_rpc_lwt

(* $MDX part-begin=local *)
let rec local ~services sr label =
  let module Logger = Api.Service.Logger in
  Persistence.with_sturdy_ref sr Logger.local @@ object
(* $MDX part-end *)
    inherit Logger.service

    method log_impl params release_param_caps =
      let open Logger.Log in
      let msg = Params.msg_get params in
      release_param_caps ();
      Printf.printf "[server] %S says %S\n%!" label msg;
      Service.return_empty ()

    method sub_impl params release_param_caps =
      let open Logger.Sub in
      let sub_label = Params.label_get params in
      release_param_caps ();
      let id = Capnp_rpc_net.Restorer.Id.generate () in
      let sr = Capnp_rpc_net.Restorer.Table.sturdy_ref services id in
      let sub = local ~services sr (Printf.sprintf "%s/%s" label sub_label) in
      let response, results = Service.Response.create Results.init_pointer in
      Results.logger_set results (Some sub);
      Capnp_rpc_net.Restorer.Table.add services id sub; (* Takes ownership of [sub] *)
      Service.return response

    method! pp f =
      Fmt.pf f "Logger(%s)" label
  end

module Logger = Api.Client.Logger

let log t msg =
  let open Logger.Log in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.msg_set params msg;
  Capability.call_for_unit_exn t method_id request

let sub t label =
  let open Logger.Sub in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.label_set params label;
  Capability.call_for_caps t method_id request Results.logger_get_pipelined
