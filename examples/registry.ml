open Lwt.Infix
open Capnp_rpc_lwt

(* A service that can return other services. *)
let service () =
  Api.Builder.Registry.local @@
  object (_ : Api.Builder.Registry.service)
    val mutable blocked = Lwt.wait ()
    val mutable echo_service = Echo.service ()

    method set_echo_service params =
      let module P = Api.Reader.Registry.SetEchoService_params in
      match P.service_get (P.of_payload params) with
      | None -> assert false
      | Some s ->
        echo_service <- Payload.import params s;
        Service.return_empty ()

    method echo_service _params =
      let module R = Api.Builder.Registry.EchoService_results in
      let resp, results = Service.Response.create R.init_pointer in
      let cap_index = Service.Response.export resp echo_service in
      R.service_set results (Some cap_index);
      Service.return_lwt (
        fst blocked >|= fun () -> Ok resp
      )

    method unblock _ =
      Lwt.wakeup (snd blocked) ();
      blocked <- Lwt.wait ();
      Service.return_empty ()
  end

module Client = struct
  let set_echo_service t echo_service =
    let proxy = new Api.Reader.Registry.client t in
    let module P = Api.Builder.Registry.SetEchoService_params in
    let req, p = Capability.Request.create P.init_pointer in
    P.service_set p (Some (Capability.Request.export req echo_service));
    Capability.call_for_value proxy#set_echo_service req >|= ignore

  let echo_service t =
    let proxy = new Api.Reader.Registry.client t in
    let req = Capability.Request.create_no_args () in
    let module R = Api.Reader.Registry.EchoService_results in
    Capability.call proxy#echo_service req
    |> R.service_get_pipelined

  let unblock t =
    let proxy = new Api.Reader.Registry.client t in
    let req = Capability.Request.create_no_args () in
    Capability.call_for_value proxy#unblock req >|= ignore
end
