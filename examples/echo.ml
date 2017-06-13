open Lwt.Infix
open Capnp_rpc_lwt

(* This was supposed to be a simple ping service, but I wanted to test out-of-order
   replies, so it has become a bit messy... *)
let service () =
  Api.Builder.Echo.local @@
  object (_ : Api.Builder.Echo.service)
    val mutable blocked = Lwt.wait ()
    val mutable count = 0

    method ping = fun req ->
      let module P = Api.Reader.Echo.Ping_params in
      let module R = Api.Builder.Echo.Ping_results in
      let params = P.of_payload req in
      let msg = P.msg_get params in
      let resp, results = Service.Response.create R.init_pointer in
      R.reply_set results (Fmt.strf "got:%d:%s" count msg);
      count <- count + 1;
      if P.slow_get params then (
        Service.return_lwt (fun () ->
          fst blocked >|= fun () -> Ok resp
        )
      )
      else Service.return resp

    method unblock _ =
      Lwt.wakeup (snd blocked) ();
      blocked <- Lwt.wait ();
      Service.return_empty ()
  end

module Client = struct
  type t = Api.Reader.Echo.t Capability.t

  let ping t ?(slow=false) msg =
    let module P = Api.Builder.Echo.Ping_params in
    let module R = Api.Reader.Echo.Ping_results in
    let req, p = Capability.Request.create P.init_pointer in
    P.slow_set p slow;
    P.msg_set p msg;
    Capability.call_for_value_exn t Api.Reader.Echo.ping_method req >|= fun resp ->
    R.of_payload resp |> R.reply_get

  let unblock t =
    let req = Capability.Request.create_no_args () in
    Capability.call_for_value t Api.Reader.Echo.unblock_method req >|= ignore
end
