open Capnp_core
open Lwt.Infix

module Log = Rpc.Log

module Response = Response
module RO_array = Capnp_rpc.RO_array

type abstract_response_promise = Core_types.struct_ref

type abstract_method_t =
  Schema.Reader.Payload.t * Core_types.cap RO_array.t -> abstract_response_promise

type 'a response_promise = abstract_response_promise
type ('a, 'b) method_t = abstract_method_t

let local s =
  object (self : Core_types.cap)
    inherit Core_types.ref_counted

    method private release = ()

    method pp f = Fmt.string f "local-service"

    method call call caps =
      let open Schema.Reader in
      let call = Rpc.readable_req call in
      let interface_id = Call.interface_id_get call in
      let method_id = Call.method_id_get call in
      Log.info (fun f -> f "Invoking local method %a" Capnp.RPC.Registry.pp_method (interface_id, method_id));
      let p = Call.params_get call in
      let m = s ~interface_id ~method_id in
      m (p, caps)

    method shortest = self
  end

(* The simple case for returning a message (rather than another value). *)
let return resp =
  let resp, caps = Response.finish resp in
  Core_types.return (resp, caps)

let return_empty () =
  return @@ Response.create_empty ()

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
  let result = Local_struct_promise.make () in
  Lwt.async (fun () ->
      resp >|= function
      | Ok resp ->
        let msg, caps = Response.finish resp in
        result#resolve (Ok (msg, caps));
      | Error _ as e -> result#resolve e
    );
  (result :> Core_types.struct_ref)

let fail fmt =
  fmt |> Fmt.kstrf @@ fun msg ->
  Core_types.broken (`Exception msg)
