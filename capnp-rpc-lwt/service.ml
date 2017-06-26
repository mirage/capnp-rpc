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

let pp_method = Capnp.RPC.Registry.pp_method

let local s =
  object
    inherit Core_types.service

    method! pp f = Fmt.string f "local-service"

    method call call caps =
      let open Schema.Reader in
      let call = Rpc.readable_req call in
      let interface_id = Call.interface_id_get call in
      let method_id = Call.method_id_get call in
      Log.info (fun f -> f "Invoking local method %a" pp_method (interface_id, method_id));
      let p = Call.params_get call in
      let m = s ~interface_id ~method_id in
      try m (p, caps)
      with ex ->
        Log.warn (fun f -> f "Uncaught exception handling %a: %a" pp_method (interface_id, method_id) Fmt.exn ex);
        Core_types.fail "Internal error from %a" pp_method (interface_id, method_id)
  end

(* The simple case for returning a message (rather than another value). *)
let return resp =
  let resp, caps = Response.finish resp in
  Core_types.return (resp, caps)

let return_empty () =
  return @@ Response.create_empty ()

(* A convenient way to implement a simple blocking local function, where
   pipelining is not supported (further messages will be queued up at this
   host until it returns). *)
let return_lwt fn =
  let result = Local_struct_promise.make () in
  Lwt.async (fun () ->
      Lwt.catch (fun () ->
          fn () >|= function
          | Ok resp ->
            let msg, caps = Response.finish resp in
            result#resolve (Ok (msg, caps));
          | Error _ as e -> result#resolve e
        )
        (fun ex ->
           Log.warn (fun f -> f "Uncaught exception: %a" Fmt.exn ex);
           result#resolve (Error (Capnp_rpc.Error.exn "Internal error"));
           Lwt.return_unit
        );
    );
  (result :> Core_types.struct_ref)

let fail = Core_types.fail
