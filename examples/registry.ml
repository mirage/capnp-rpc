open Lwt.Infix
open Capnp_rpc_lwt

let version_service =
  Api.Builder.Version.local @@
  object
    inherit Api.Builder.Version.service

    method read_impl _ =
      let module R = Api.Builder.Version.Read_results in
      let resp, results = Service.Response.create R.init_pointer in
      R.version_set results "0.1";
      Service.return resp
  end

(* A service that can return other services. *)
let service () =
  Api.Builder.Registry.local @@
  object
    inherit Api.Builder.Registry.service

    val mutable blocked = Lwt.wait ()
    val mutable echo_service = Echo.service ()

    method! release = Capability.dec_ref echo_service

    method! pp f = Fmt.string f "registry"

    method set_echo_service_impl params =
      let module P = Api.Reader.Registry.SetEchoService_params in
      match P.service_get (P.of_payload params) with
      | None -> assert false
      | Some s ->
        Capability.dec_ref echo_service;
        echo_service <- Payload.import params s;
        Payload.release params;
        Service.return_empty ()

    method echo_service_impl _params =
      let module R = Api.Builder.Registry.EchoService_results in
      let resp, results = Service.Response.create R.init_pointer in
      let cap_index = Service.Response.export resp echo_service in
      R.service_set results (Some cap_index);
      Service.return_lwt (fun () ->
        fst blocked >|= fun () -> Ok resp
      )

    method echo_service_promise_impl _params =
      let module R = Api.Builder.Registry.EchoServicePromise_results in
      let resp, results = Service.Response.create R.init_pointer in
      let promise, resolver = Capability.promise () in
      let cap_index = Service.Response.export resp promise in
      Capability.dec_ref promise;
      R.service_set results (Some cap_index);
      Lwt.async (fun () ->
          fst blocked >|= fun () ->
          Capability.inc_ref echo_service;
          Capability.resolve_ok resolver echo_service
        );
      Service.return resp

    method unblock_impl _ =
      Lwt.wakeup (snd blocked) ();
      blocked <- Lwt.wait ();
      Service.return_empty ()

    method complex_impl _ =
      (* Returns:
         foo (f1):
           b (b2) = {}
           echo = echo_service
         bar (b1):
           version = version_service
       *)
      let module R = Api.Builder.Registry.Complex_results in
      let resp, results = Service.Response.create R.init_pointer in
      let f1 = R.foo_init results in
      let b1 = R.bar_init results in
      let module Foo = Api.Builder.Foo in
      let module Bar = Api.Builder.Bar in
      let _b2 = Foo.b_init f1 in
      Foo.echo_set f1 (Some (Service.Response.export resp echo_service));
      Bar.version_set b1 (Some (Service.Response.export resp version_service));
      Service.return resp
  end

module Client = struct
  let set_echo_service t echo_service =
    let module P = Api.Builder.Registry.SetEchoService_params in
    let req, p = Capability.Request.create P.init_pointer in
    P.service_set p (Some (Capability.Request.export req echo_service));
    Capability.call_for_value t Api.Reader.Registry.set_echo_service_method req >|= ignore

  (* Waits until unblocked before returning *)
  let echo_service t =
    let req = Capability.Request.create_no_args () in
    let module R = Api.Reader.Registry.EchoService_results in
    Capability.call_for_caps t Api.Reader.Registry.echo_service_method req R.service_get_pipelined

  (* Returns a promise immediately. Resolves promise when unblocked. *)
  let echo_service_promise t =
    let req = Capability.Request.create_no_args () in
    let module R = Api.Reader.Registry.EchoServicePromise_results in
    Capability.call_for_caps t Api.Reader.Registry.echo_service_promise_method req R.service_get_pipelined

  let unblock t =
    let req = Capability.Request.create_no_args () in
    Capability.call_for_value t Api.Reader.Registry.unblock_method req >|= ignore

  let complex t =
    let req = Capability.Request.create_no_args () in
    let module R = Api.Reader.Registry.Complex_results in
    let module Foo = Api.Reader.Foo in
    let module Bar = Api.Reader.Bar in
    Capability.call_for_caps t Api.Reader.Registry.complex_method req @@ fun result ->
    let echo_service = R.foo_get_pipelined result |> Foo.echo_get_pipelined in
    let version = R.bar_get_pipelined result |> Bar.version_get_pipelined in
    (echo_service, version)
end

module Version = struct
  let read t =
    let req = Capability.Request.create_no_args () in
    let module R = Api.Reader.Version.Read_results in
    Capability.call_for_value_exn t Api.Reader.Version.read_method req >|= fun p ->
    R.version_get (R.of_payload p)
end
