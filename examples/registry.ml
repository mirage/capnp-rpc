open Capnp_rpc

(* A service that can return other services. *)
let service =
  Api.Builder.Registry.local @@
  object (_ : Api.Builder.Registry.service)
    method echo_service _params =
      let module R = Api.Builder.Registry.EchoService_results in
      let resp, results = Service.Response.create R.init_pointer in
      let cap_index = Service.Response.export resp Echo.service in
      R.service_set results (Some cap_index);
      Service.return resp
  end

let client proxy =
  let proxy = new Api.Reader.Registry.client proxy in
  object
    method echo_service =
      let req = Capability.Request.create_no_args () in
      let module R = Api.Reader.Registry.EchoService_results in
      Capability.call_for_cap proxy#echo_service req
      |> R.service_get_pipelined
      |> Echo.client
  end
