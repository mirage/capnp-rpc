module Capnp_content = struct
  include Msg

  let ref_leak_detected = Leak_handler.ref_leak_detected
end

module Core_types = Capnp_rpc_proto.Core_types(Capnp_content)

module Local_struct_promise = Capnp_rpc_proto.Local_struct_promise.Make(Core_types)
module Cap_proxy = Capnp_rpc_proto.Cap_proxy.Make(Core_types)

module type ENDPOINT = Capnp_rpc_proto.Message_types.ENDPOINT with
  module Core_types = Core_types

class type sturdy_ref = object
  method connect : (Core_types.cap, Capnp_rpc_proto.Exception.t) result
  method to_uri_with_secrets : Uri.t
end
