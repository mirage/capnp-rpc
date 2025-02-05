open Capnp_core
open Utils

module Log = Capnp_rpc.Debug.Log

module Response = Response
module RO_array = Capnp_rpc.RO_array

type abstract_response_promise = Core_types.struct_ref

type abstract

type abstract_method_t =
  abstract Schema.reader_t -> (unit -> unit) -> abstract_response_promise

type 'a response_promise = abstract_response_promise
type ('a, 'b) method_t = 'a -> (unit -> unit) -> Core_types.struct_ref

let pp_method = Capnp.RPC.Registry.pp_method

class type generic = object
  method dispatch : interface_id:Stdint.Uint64.t -> method_id:int -> abstract_method_t
  method release : unit
  method pp : Format.formatter -> unit
end

let local (s:#generic) =
  object (_ : Core_types.cap)
    inherit Core_types.service as super

    method! pp f = Fmt.fprintf f "%t(%t)" s#pp super#pp_refcount

    method! private release =
      super#release;
      s#release

    method call results msg =
      let open Schema.Reader in
      let call = Msg.Request.readable msg in
      let interface_id = Call.interface_id_get call in
      let method_id = Call.method_id_get call in
      Log.info (fun f -> f "Invoking local method %a" pp_method (interface_id, method_id));
      let p = Call.params_get call in
      let m : abstract_method_t = s#dispatch ~interface_id ~method_id in
      let release_params () = Core_types.Request_payload.release msg in
      let contents : abstract Schema.reader_t =
        Payload.content_get p |> Schema.ReaderOps.deref_opt_struct_pointer |> Schema.ReaderOps.cast_struct in
      match m contents release_params with
      | r -> results#resolve r
      | exception ex ->
        release_params ();
        Log.warn (fun f -> f "Uncaught exception handling %a: %a" pp_method (interface_id, method_id) Fmt.exn ex);
        Core_types.resolve_payload results
          (Error (Capnp_rpc.Error.exn "Internal error from %a" pp_method (interface_id, method_id)))
  end

(* The simple case for returning a message (rather than another value). *)
let return resp =
  Core_types.return @@ Response.finish resp

let return_empty () =
  return @@ Response.create_empty ()


(* A convenient way to implement a simple blocking local function, where
   pipelining is not supported (further messages will be queued up at this
   host until it returns). *)
let return_fut fn : _ =
  let sync = SyncPoint.create() in
  let result, resolver = Local_struct_promise.make () in
  (* call [fn] in parallel *)
  ThreadPool.run
    (fun () ->
      try
        begin match Fut.get @@ fn () with
          | Ok resp      ->
            Core_types.resolve_ok resolver @@ Response.finish resp;
            SyncPoint.signal sync;
          | Error (`Capnp e) ->
            Core_types.resolve_payload resolver (Error e);
            SyncPoint.signal sync;
        end
      with ex ->
        Log.warn (fun f -> f "Uncaught exception: %a" Fmt.exn ex);
        Core_types.resolve_exn resolver @@ Capnp_rpc.Exception.v "Internal error";
            SyncPoint.signal sync;
    );
  SyncPoint.wait sync;
  result

let fail = Core_types.fail
