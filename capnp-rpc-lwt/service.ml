open Capnp_core

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

    method! pp f = Fmt.pf f "%t(%t)" s#pp super#pp_refcount

    method! private release =
      super#release;
      s#release

    method call results msg =
      let open Schema.Reader in
      let call = Msg.Request.readable msg in
      let interface_id = Call.interface_id_get call in
      let method_id = Call.method_id_get call in
      Log.debug (fun f -> f "Invoking local method %a" pp_method (interface_id, method_id));
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

let fail = Core_types.fail

let error = Core_types.broken_struct
