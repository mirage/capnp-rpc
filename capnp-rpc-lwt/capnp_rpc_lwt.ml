open Lwt.Infix
open Capnp_core

include Capnp.Message.BytesMessage

type 'a or_error = ('a, Capnp_rpc.Error.t) result

module Log = Capnp_rpc.Debug.Log
module RO_array = Capnp_rpc.RO_array

module Capability = struct
  type 'a t = Core_types.cap
  type 'a capability_t = 'a t
  type ('t, 'a, 'b) method_t = ('t, 'a, 'b) Capnp.RPC.MethodID.t

  module Request = Request

  let inc_ref = Core_types.inc_ref
  let dec_ref = Core_types.dec_ref
  let pp f x = x#pp f

  let broken = Core_types.broken_cap
  let when_broken = Core_types.when_broken
  let problem x = x#problem

  let call (target : 't capability_t) (m : ('t, 'a, 'b) method_t) (req : 'a Request.t) =
    Log.info (fun f -> f "Calling %a" Capnp.RPC.MethodID.pp m);
    let msg = Request.finish m req in
    let results, resolver = Local_struct_promise.make () in
    target#call resolver msg;
    results

  let call_and_wait cap (m : ('t, 'a, 'b StructStorage.reader_t) method_t) req =
    let p, r = Lwt.task () in
    let result = call cap m req in
    let finish = lazy (Core_types.dec_ref result) in
    Lwt.on_cancel p (fun () -> Lazy.force finish);
    result#when_resolved (function
        | Error _ as e -> Lwt.wakeup r e
        | Ok resp ->
          Lazy.force finish;
          let payload = Msg.Response.readable resp in
          let release_response_caps () = Core_types.Response_payload.release resp in
          let contents = Schema.Reader.Payload.content_get payload |> Schema.Reader.of_pointer in
          Lwt.wakeup r @@ Ok (contents, release_response_caps)
      );
    p

  let call_for_value cap m req =
    call_and_wait cap m req >|= function
    | Error _ as response -> response
    | Ok (response, release_response_caps) ->
      release_response_caps ();
      Ok response

  let call_for_value_exn cap m req =
    call_for_value cap m req >>= function
    | Ok x -> Lwt.return x
    | Error e ->
      let msg = Fmt.strf "Error calling %t(%a): %a"
          cap#pp
          Capnp.RPC.MethodID.pp m
          Capnp_rpc.Error.pp e in
      Lwt.fail (Failure msg)

  let call_for_unit cap m req =
    call_for_value cap m req >|= function
    | Ok _ -> Ok ()
    | Error _ as e -> e

  let call_for_unit_exn cap m req = call_for_value_exn cap m req >|= ignore

  let call_for_caps cap m req fn =
    let q = call cap m req in
    match fn q with
    | r -> Core_types.dec_ref q; r
    | exception ex -> Core_types.dec_ref q; raise ex

  type 'a resolver = Cap_proxy.resolver_cap

  let promise () =
    let cap = Cap_proxy.local_promise () in
    (cap :> Core_types.cap), (cap :> 'a resolver)

  let resolve_ok r x = r#resolve x

  let resolve_exn r ex = r#resolve (Core_types.broken_cap ex)
end

module StructRef = struct
  type 'a t = Core_types.struct_ref

  let inc_ref = Core_types.inc_ref
  let dec_ref = Core_types.dec_ref
end

module Untyped = struct
  let struct_field t i =
    (* todo: would be better to have a separate type for this *)
    object (_ : Core_types.struct_ref)
      method cap path = t#cap (Xform.Field i :: path)
      method when_resolved _ = failwith "Can't use when_resolved on a sub-struct"
      method response = failwith "Can't use response on a sub-struct"
      method update_rc = failwith "Can't use rec-counts on a sub-struct"
      method sealed_dispatch _ = None
      method pp f = Fmt.pf f "pointer %d in %t" i t#pp
      method blocker = failwith "struct_field: blocker"
      method check_invariants = ()
    end

  let capability_field t f = t#cap [Xform.Field f]

  let local = Service.local

  type abstract_method_t = Service.abstract_method_t

  let abstract_method x req release =
    x (StructStorage.cast_reader req) release

  let get_cap a i =
    Core_types.Attachments.cap (Uint32.to_int i) (Msg.unwrap_attachments a)

  let add_cap a cap =
    Core_types.Attachments.add_cap (Msg.unwrap_attachments a) cap |> Uint32.of_int

  let clear_cap a i =
    Core_types.Attachments.clear_cap (Msg.unwrap_attachments a) (Uint32.to_int i)

  let unknown_interface ~interface_id _req release_params =
    release_params ();
    Core_types.fail ~ty:`Unimplemented "Unknown interface %a" Uint64.printer interface_id

  let unknown_method ~interface_id ~method_id _req release_params =
    release_params ();
    Core_types.fail ~ty:`Unimplemented "Unknown method %a.%d" Uint64.printer interface_id method_id

  class type generic_service = Service.generic
end

module Service = Service
module Endpoint = Endpoint

module S = S

module Restorer = Restorer

module Networking (N : S.NETWORK) (F : Mirage_flow_lwt.S) = struct
  type flow = F.flow
  type 'a capability = 'a Capability.t
  type restorer = Restorer.t

  module Network = N
  module Vat = Vat.Make (N) (F)
  module Sturdy_ref = Vat.Sturdy_ref
  module CapTP = Vat.CapTP
end

module Two_party_network = Two_party_network
module Auth = Auth
