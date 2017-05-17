open Lwt.Infix

module Log = Rpc.Log

module Response = Response

type abstract_response_promise = Rpc.value * (Rpc.resp_msg * Rpc.value Ro_array.t) Lwt.t

type abstract_method_t =
  Schema.Reader.Payload.t * Rpc.value Ro_array.t -> abstract_response_promise

type 'a response_promise = abstract_response_promise
type ('a, 'b) method_t = abstract_method_t

let local s =
  object (_ : Rpc.value)
    method apply _ = failwith "Not a message!"
    method call xs call caps =
      if xs <> [] then failwith "Can't transform a service!";
      let open Schema.Reader in
      let call = Rpc.readable_req call in
      let interface_id = Call.interface_id_get call in
      let method_id = Call.method_id_get call in
      Log.info (fun f -> f "Invoking local method %a" Capnp.RPC.Registry.pp_method (interface_id, method_id));
      let p = Call.params_get call in
      let m = s ~interface_id ~method_id in
      m (p, caps)
  end

(* The simple case for returning a message (rather than another value). *)
let return resp : abstract_response_promise =
  let msg, caps = Response.finish resp in
  let pointers = Payload_caps.of_content ~caps msg in
  pointers, Lwt.return (msg, caps)

let return_empty () =
  return @@ Response.create_no_args ()

let buffer () =
  let q = Queue.create () in
  let add _xs _req _caps =
    failwith "todo: create promise for result and enqueue"
  in
  add, q

(* A convenient way to implement a simple blocking local function, where
   pipelining is not supported (further messages will be queued up at this
   host until it returns). *)
let return_lwt resp =
  let handler, q = buffer () in
  let result = new Promise.promise handler in
  let msg, waker = Lwt.wait () in
  Lwt.async (fun () ->
      resp >|= fun resp ->
      let msg, caps = Response.finish resp in
      let new_target = Payload_caps.of_content ~caps msg in
      result#resolve new_target;
      let flush _ = failwith "todo: fliush" in
      Queue.iter flush q;               (* todo: ordering probably not right here *)
      Lwt.wakeup waker (msg, caps)
    );
  (result :> Rpc.value), msg

let return_ref
    (result, (payload : (Schema.Reader.Payload.t * Rpc.value Ro_array.t) Lwt.t))
  : abstract_response_promise =
  let module B = Schema.Builder in
  let module R = Schema.Reader in
  let ret = payload >|= fun (rpayload, caps) ->
    let msg = B.Message.init_root () in
    let ret = B.Message.return_init msg in
    let payload = B.Return.results_init ret in
    B.Payload.content_set_reader payload (R.Payload.content_get rpayload) |> ignore;
    Rpc.Builder ret, caps
  in
  result, ret
