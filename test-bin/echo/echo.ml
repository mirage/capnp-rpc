module Api = Echo_api.MakeRPC(Capnp_rpc_lwt)

open Capnp_rpc_lwt

(*-- Server ----------------------------------------*)
let local =
  let module Echo = Api.Service.Echo in

  Echo.local @@ object
    inherit Echo.service

    method ping_impl params release_param_caps =
      let open Echo.Ping in
      let msg = Params.msg_get params in
      release_param_caps ();
      let resp = "echo:" ^ msg in
      let message_size = 100 + String.length resp in
      let response, results = Service.Response.create ~message_size Results.init_pointer in
      Results.reply_set results resp;
      Service.return response
  end

(*-- Client ----------------------------------------*)
module Echo = Api.Client.Echo

let ping t msg =
  let open Echo.Ping in
  let message_size = 200 + String.length msg in  (* (rough estimate) *)
  let request, params = Capability.Request.create ~message_size Params.init_pointer in
  Params.msg_set params msg;
  Capability.call_for_value_exn t method_id request |> Results.reply_get
