open Lwt.Infix

module Capnp_content = struct
  include Msg

  let ref_leak_detected fn =
    Lwt.async (fun () ->
        Lwt.pause () >|= fun () ->
        fn ();
        failwith "ref_leak_detected"
      )
end

module Core_types = Capnp_rpc.Core_types(Capnp_content)

module Local_struct_promise = Capnp_rpc.Local_struct_promise.Make(Core_types)
module Cap_proxy = Capnp_rpc.Cap_proxy.Make(Core_types)

module type ENDPOINT = Capnp_rpc.Message_types.ENDPOINT with
  module Core_types = Core_types
