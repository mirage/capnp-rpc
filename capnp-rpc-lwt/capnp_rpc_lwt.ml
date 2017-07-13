open Lwt.Infix
open Capnp_core

type 'a or_error = ('a, Capnp_rpc.Error.t) result

module Log = Rpc.Log
module RO_array = Capnp_rpc.RO_array

module Payload = struct
  type 'a t = Schema.Reader.Payload.t * Core_types.cap RO_array.t
  type 'a index = Uint32.t

  let import (t:'a t) i =
    let cap = RO_array.get (snd t) (Uint32.to_int i) in       (* TODO: out-of-bounds *)
    Core_types.inc_ref cap;
    cap

  let release (_, caps) =
    RO_array.iter Core_types.dec_ref caps
end

module Capability = struct
  type 'a t = Core_types.cap
  type 'a capability_t = 'a t
  type ('t, 'a, 'b) method_t = Uint64.t * int

  module Request = Request
  module Response = Payload

  let inc_ref = Core_types.inc_ref
  let dec_ref = Core_types.dec_ref
  let pp f x = x#pp f

  let call (target : 't capability_t) (m : ('t, 'a, 'b) method_t) req =
    let open Schema.Builder in
    Log.info (fun f -> f "Calling %a" Capnp.RPC.Registry.pp_method m);
    let c = Request.get_call req in
    let (interface_id, method_id) = m in
    Call.interface_id_set c interface_id;
    Call.method_id_set_exn c method_id;
    let results, resolver = Local_struct_promise.make () in
    target#call resolver (Rpc.Builder c) (Request.caps req);
    results

  let call_for_value cap m req =
    let p, r = Lwt.task () in
    let result = call cap m req in
    let finish = lazy (Core_types.dec_ref result) in
    Lwt.on_cancel p (fun () -> Lazy.force finish);
    result#when_resolved (function
        | Error _ as e -> Lwt.wakeup r e
        | Ok (resp, caps) ->
          Lazy.force finish;
          let open Schema.Reader in
          let resp = Rpc.readable_resp resp in
          match Return.get resp with
          | Results results ->
            Lwt.wakeup r @@ Ok (results, caps)
          | _ -> assert false
      );
    p

  let call_for_value_exn cap m req =
    call_for_value cap m req >>= function
    | Ok x -> Lwt.return x
    | Error e ->
      let msg = Fmt.strf "Error calling %t(%a): %a"
          cap#pp
          Capnp.RPC.Registry.pp_method m
          Capnp_rpc.Error.pp e in
      Lwt.fail (Failure msg)

  let call_for_caps cap m req fn =
    let q = call cap m req in
    match fn q with
    | r -> Core_types.dec_ref q; r
    | exception ex -> Core_types.dec_ref q; raise ex
end

module StructRef = struct
  type 'a t = Core_types.struct_ref

  let inc_ref = Core_types.inc_ref
  let dec_ref = Core_types.dec_ref
end

module Untyped = struct
  type pointer_r = Capnp.Message.ro Capnp.BytesMessage.Slice.t option

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

  let content_of_payload (t : 'a Payload.t) : pointer_r =
    Schema.Reader.Payload.content_get (fst t)

  let local = Service.local

  let define_method ~interface_id ~method_id : ('t, 'a, 'b) Capability.method_t =
    (interface_id, method_id)

  type abstract_method_t = Service.abstract_method_t

  let abstract_method x = x

  let cap_index x = x

  let unknown_interface ~interface_id _req =
    Core_types.fail "Unknown interface %a" Uint64.printer interface_id

  let unknown_method ~interface_id ~method_id _req =
    Core_types.fail "Unknown method %a.%d" Uint64.printer interface_id method_id

  class type generic_service = Service.generic
end

module Service = Service
module CapTP = CapTP_capnp
module Endpoint = Endpoint
