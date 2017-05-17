open Lwt.Infix
open Capnp_rpc

(* This was supposed to be a simple ping service, but I wanted to test out-of-order
   replies, so it has become a bit messy... *)
let service =
  Api.Builder.Echo.local @@
  object (_ : Api.Builder.Echo.service)
    val mutable blocked = Lwt.wait ()

    method ping = fun req ->
      let module P = Api.Reader.Echo.Ping_params in
      let module R = Api.Builder.Echo.Ping_results in
      let params = P.of_payload req in
      let msg = P.msg_get params in
      let resp, results = Service.Response.create R.init_pointer in
      R.reply_set results ("got:" ^ msg);
      if P.slow_get params then (
        Service.return_lwt (
          fst blocked >|= fun () -> resp
        )
      )
      else Service.return resp

    method unblock _ =
      Lwt.wakeup (snd blocked) ();
      blocked <- Lwt.wait ();
      Service.return_empty ()
  end

let client proxy =
  let proxy = new Api.Reader.Echo.client proxy in
  object
    method ping ?(slow=false) msg =
      let module P = Api.Builder.Echo.Ping_params in
      let module R = Api.Reader.Echo.Ping_results in
      let req, p = Capability.Request.create P.init_pointer in
      P.slow_set p slow;
      P.msg_set p msg;
      Capability.call_for_value proxy#ping req >|= fun resp ->
      R.of_payload resp |> R.reply_get

    method unblock () =
      let req = Capability.Request.create_no_args () in
      Capability.call_for_value proxy#unblock req >|= ignore
  end
