module Api = Echo_api.MakeRPC(Capnp_rpc_lwt)

open Eio.Std
open Capnp_rpc_lwt

module Callback = struct
  let local fn =
    let module Callback = Api.Service.Callback in
    Callback.local @@ object
      inherit Callback.service

      method log_impl params release_param_caps =
        let open Callback.Log in
        let msg = Params.msg_get params in
        release_param_caps ();
        fn msg;
        Service.return_empty ()
    end

  module Callback = Api.Client.Callback

  let log t msg =
    let open Callback.Log in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.msg_set params msg;
    Capability.call_for_unit t method_id request
end

let notify ~clock msg callback =
  let rec loop = function
    | 0 ->
      Service.return_empty ()
    | i ->
      match Callback.log callback msg with
      | Error (`Capnp e) -> Service.error e
      | Ok () ->
        Eio.Time.sleep clock 1.0;
        loop (i - 1)
  in
  loop 3

let service_logger =
  Callback.local (traceln "[server] Received %S")

let local ~clock =
  let module Echo = Api.Service.Echo in
  Echo.local @@ object
    inherit Echo.service

    method ping_impl params release_param_caps =
      let open Echo.Ping in
      let msg = Params.msg_get params in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      Results.reply_set results ("echo:" ^ msg);
      Service.return response

    method heartbeat_impl params release_params =
      let open Echo.Heartbeat in
      let msg = Params.msg_get params in
      let callback = Params.callback_get params in
      release_params ();
      match callback with
      | None -> Service.fail "No callback parameter!"
      | Some callback ->
        Capability.with_ref callback (notify ~clock msg)

    (* $MDX part-begin=server-get-logger *)
    method get_logger_impl _ release_params =
      let open Echo.GetLogger in
      release_params ();
      let response, results = Service.Response.create Results.init_pointer in
      Results.callback_set results (Some service_logger);
      Service.return response
    (* $MDX part-end *)
  end

module Echo = Api.Client.Echo

let ping t msg =
  let open Echo.Ping in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.msg_set params msg;
  Capability.call_for_value_exn t method_id request |> Results.reply_get

let heartbeat t msg callback =
  let open Echo.Heartbeat in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.msg_set params msg;
  Params.callback_set params (Some callback);
  Capability.call_for_unit_exn t method_id request

(* $MDX part-begin=client-get-logger *)
let get_logger t =
  let open Echo.GetLogger in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_caps t method_id request Results.callback_get_pipelined
(* $MDX part-end *)
