open Lwt.Infix

module Log = Rpc.Log

module Payload = struct
  type 'a t = Schema.Reader.Payload.t * Rpc.value Ro_array.t
  type 'a index = Uint32.t

  let import (t:'a t) i = Ro_array.get (snd t) (Uint32.to_int i)
end

module Capability = struct
  type 'a t = Rpc.value
  type 'a capability_t = 'a t
  type ('a, 'b) method_t = Rpc.value * Uint64.t * int

  module Request = Request
  module Response = Payload

  let call ((target, interface_id, method_id) : ('a, 'b) method_t) req =
    let open Schema.Builder in
    Log.info (fun f -> f "Calling %a" Capnp.RPC.Registry.pp_method (interface_id, method_id));
    let c = Request.get_call req in
    Call.interface_id_set c interface_id;
    Call.method_id_set_exn c method_id;
    target#call [] (Rpc.Builder c) (Request.caps req)

  let payload_of_response (resp, caps) =
    let resp = Rpc.readable_resp resp in
    let open Schema.Reader in
    match Return.get resp with
    | Return.Results p -> p, caps
    | _ -> failwith "Not results!"

  let call_full m req : 'b t * 'b Response.t Lwt.t =
    let cap, msg = call m req in
    cap, (msg >|= payload_of_response)

  let call_for_cap m req =
    fst (call m req)

  let call_for_value m req =
    snd (call_full m req)
end

module StructRef = struct
  type 'a t = Rpc.value
end

module Untyped = struct
  type pointer_r = Capnp.Message.ro Capnp.BytesMessage.Slice.t option

  let struct_field t f = t#apply [Xform.Field f]
  let capability_field t f = t#apply [Xform.Field f]

  let content_of_payload (t : 'a Payload.t) : pointer_r =
    Schema.Reader.Payload.content_get (fst t)

  let local = Service.local

  let bind_method target ~interface_id ~method_id : ('a, 'b) Capability.method_t =
    (target, interface_id, method_id)

  type abstract_method_t = Service.abstract_method_t

  let abstract_method x = x

  let cap_index x = x

  let unknown_interface ~interface_id _req =
    Service.fail "Unknown interface %a" Uint64.printer interface_id

  let unknown_method ~interface_id ~method_id _req =
    Service.fail "Unknown method %a.%d" Uint64.printer interface_id method_id
end

module Service = Service
module Ro_array = Ro_array
module Connection = Connection
module Endpoint = Endpoint
