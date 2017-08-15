open Lwt.Infix
open Capnp_rpc_lwt

type t = Api.Service.Registry.t Capability.t

let version_service =
  let module Version = Api.Service.Version in

  Version.local @@ object
    inherit Version.service

    method read_impl _ release_params =
      let open Version.Read in
      release_params ();
      let resp, results = Service.Response.create Results.init_pointer in
      Results.version_set results "0.1";
      Service.return resp
  end

let service () =
  let module Registry = Api.Service.Registry in
  Registry.local @@ object
    inherit Registry.service

    val mutable blocked = Lwt.wait ()
    val mutable echo_service = Echo.service ()

    method! release = Capability.dec_ref echo_service

    method! pp f = Fmt.string f "registry"

    method set_echo_service_impl params release_params =
      let open Registry.SetEchoService in
      let new_service = Params.service_get params in
      release_params ();
      match new_service with
      | None -> assert false
      | Some new_service ->
        Capability.dec_ref echo_service;
        echo_service <- new_service;
        Service.return_empty ()

    method echo_service_impl _params release_params =
      release_params ();
      let open Registry.EchoService in
      let resp, results = Service.Response.create Results.init_pointer in
      Results.service_set results (Some echo_service);
      Service.return_lwt (fun () ->
        fst blocked >|= fun () -> Ok resp
      )

    method echo_service_promise_impl _params release_params =
      release_params ();
      let open Registry.EchoServicePromise in
      let resp, results = Service.Response.create Results.init_pointer in
      let promise, resolver = Capability.promise () in
      Results.service_set results (Some promise);
      Capability.dec_ref promise;
      Lwt.async (fun () ->
          fst blocked >|= fun () ->
          Capability.inc_ref echo_service;
          Capability.resolve_ok resolver echo_service
        );
      Service.return resp

    method unblock_impl _ release_params =
      release_params ();
      Lwt.wakeup (snd blocked) ();
      blocked <- Lwt.wait ();
      Service.return_empty ()

    method complex_impl _ release_params =
      release_params ();
      (* Returns:
         foo (f1):
           b (b2) = {}
           echo = echo_service
         bar (b1):
           version = version_service
       *)
      let open Registry.Complex in
      let resp, results = Service.Response.create Results.init_pointer in
      let f1 = Results.foo_init results in
      let b1 = Results.bar_init results in
      let module Foo = Api.Builder.Foo in
      let module Bar = Api.Builder.Bar in
      let _b2 = Foo.b_init f1 in
      Foo.echo_set f1 (Some echo_service);
      Bar.version_set b1 (Some version_service);
      Service.return resp
  end

module Client = struct
  module Registry = Api.Client.Registry

  let set_echo_service t echo_service =
    let open Registry.SetEchoService in
    let req, p = Capability.Request.create Params.init_pointer in
    Params.service_set p (Some echo_service);
    Capability.call_for_unit_exn t method_id req

  let echo_service t =
    let open Registry.EchoService in
    let req = Capability.Request.create_no_args () in
    Capability.call_for_caps t method_id req Results.service_get_pipelined

  let echo_service_promise t =
    let open Registry.EchoServicePromise in
    let req = Capability.Request.create_no_args () in
    Capability.call_for_caps t method_id req Results.service_get_pipelined

  let unblock t =
    let open Registry.Unblock in
    let req = Capability.Request.create_no_args () in
    Capability.call_for_unit_exn t method_id req

  let complex t =
    let open Registry.Complex in
    let req = Capability.Request.create_no_args () in
    let module Foo = Api.Reader.Foo in
    let module Bar = Api.Reader.Bar in
    Capability.call_for_caps t method_id req @@ fun result ->
    let echo_service = Results.foo_get_pipelined result |> Foo.echo_get_pipelined in
    let version = Results.bar_get_pipelined result |> Bar.version_get_pipelined in
    (echo_service, version)
end

module Version = struct
  module Version = Api.Client.Version

  type t = Version.t Capability.t

  let read t =
    let open Version.Read in
    let req = Capability.Request.create_no_args () in
    Capability.call_for_value_exn t method_id req >|= Results.version_get
end
