module Capnp_content = struct
  include Msg

  let ref_leak_detected fn =
    (* XXX: need to call [fn] in the leaked cap's domain, from
       the event loop. *)
    fn ()
end

module Core_types = Capnp_rpc.Core_types(Capnp_content)

module Local_struct_promise = Capnp_rpc.Local_struct_promise.Make(Core_types)
module Cap_proxy = Capnp_rpc.Cap_proxy.Make(Core_types)

module type ENDPOINT = Capnp_rpc.Message_types.ENDPOINT with
  module Core_types = Core_types

class type sturdy_ref = object
  method connect : (Core_types.cap, Capnp_rpc.Exception.t) result
  method to_uri_with_secrets : Uri.t
end
