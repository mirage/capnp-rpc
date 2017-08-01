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

module Core_types = struct
  include Capnp_rpc.Core_types(Capnp_content)

  type sturdy_ref
  type provision_id
  type recipient_id
  type third_party_cap_id = [`TODO_3rd_party]
  type join_key_part
end

module Endpoint_types = Capnp_rpc.Message_types.Endpoint(Core_types)( )
module Local_struct_promise = Capnp_rpc.Local_struct_promise.Make(Core_types)
module Cap_proxy = Capnp_rpc.Cap_proxy.Make(Core_types)
