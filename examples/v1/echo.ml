(* $MDX part-begin=server *)
module Api = Echo_api.MakeRPC(Capnp_rpc)

open Capnp_rpc.Std

let local =
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
  end
(* $MDX part-end *)

(* $MDX part-begin=client *)
module Echo = Api.Client.Echo

let ping t msg =
  let open Echo.Ping in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.msg_set params msg;
  Capability.call_for_value_exn t method_id request |> Results.reply_get
(* $MDX part-end *)
