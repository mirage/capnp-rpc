open Eio.Std
open Capnp_rpc_lwt

type t = Api.Service.Echo.t Capability.t

(* This was supposed to be a simple ping service, but I wanted to test out-of-order
   replies, so it has become a bit messy... *)
let local () =
  let module Echo = Api.Service.Echo in
  Echo.local @@ object
    inherit Echo.service

    val mutable blocked = Promise.create ()
    val mutable count = 0

    val id = Capnp_rpc.Debug.OID.next ()

    method! pp f = Fmt.pf f "echo-service(%a)" Capnp_rpc.Debug.OID.pp id

    method ping_impl params release_params =
      let open Echo.Ping in
      let msg = Params.msg_get params in
      release_params ();
      let resp, results = Service.Response.create Results.init_pointer in
      Results.reply_set results (Fmt.str "got:%d:%s" count msg);
      count <- count + 1;
      if Params.slow_get params then (
        Promise.await (fst blocked);
        Service.return resp
      )
      else Service.return resp

    method unblock_impl _ release_params =
      release_params ();
      Promise.resolve (snd blocked) ();
      blocked <- Promise.create ();
      Service.return_empty ()
  end

module Echo = Api.Client.Echo

let ping t ?(slow=false) msg =
  let open Echo.Ping in
  let req, p = Capability.Request.create Params.init_pointer in
  Params.slow_set p slow;
  Params.msg_set p msg;
  Capability.call_for_value_exn t method_id req |> Results.reply_get

let ping_result t ?(slow=false) msg =
  let open Echo.Ping in
  let req, p = Capability.Request.create Params.init_pointer in
  Params.slow_set p slow;
  Params.msg_set p msg;
  match Capability.call_for_value t method_id req with
  | Ok x -> Ok (Results.reply_get x)
  | Error _ as e -> e

let unblock t =
  let open Echo.Unblock in
  let req = Capability.Request.create_no_args () in
  Capability.call_for_unit_exn t method_id req
