open Capnp_core

include Capnp.Message.BytesMessage

type 'a or_error = ('a, Capnp_rpc.Error.t) result

module Log = Capnp_rpc.Debug.Log
module RO_array = Capnp_rpc.RO_array

module Capability = Capability
module StructRef = struct
  type 'a t = Core_types.struct_ref

  let inc_ref = Core_types.inc_ref
  let dec_ref = Core_types.dec_ref
end

module Sturdy_ref = Sturdy_ref

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
  type service_id = Restorer.Id.t
  class type [+'a] sturdy_ref = ['a] Sturdy_ref.t

  module Network = N
  module Vat = Vat.Make (N) (F)
  module CapTP = Vat.CapTP
end

module Capnp_address = Capnp_address
module Persistence = Persistence
module Two_party_network = Two_party_network
module Auth = Auth
module Tls_wrapper = Tls_wrapper
