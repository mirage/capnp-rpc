open Lwt.Infix

module Api = Api.MakeRPC(Capnp_rpc)

open Capnp_rpc.Std

let local ~persist_new sr label =
  let module Logger = Api.Service.Logger in
  Capnp_rpc.Persistence.with_sturdy_ref sr Logger.local @@ object
    inherit Logger.service

    method log_impl params release_param_caps =
      let open Logger.Log in
      let msg = Params.msg_get params in
      release_param_caps ();
      Printf.printf "[server] %S says %S\n%!" label msg;
      Service.return_empty ()

(* $MDX part-begin=sub-impl *)
    method sub_impl params release_param_caps =
      let open Logger.Sub in
      let sub_label = Params.label_get params in
      release_param_caps ();
      let label = Printf.sprintf "%s/%s" label sub_label in
      Service.return_lwt @@ fun () ->
      persist_new ~label >|= function
      | Error e -> Error (`Capnp (`Exception e))
      | Ok logger ->
        let response, results = Service.Response.create Results.init_pointer in
        Results.logger_set results (Some logger);
        Capability.dec_ref logger;
        Ok response
(* $MDX part-end *)

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
