(** A simple instantiation of Capnp_rpc for testing, in which messages are just strings. *)

let src = Logs.Src.create "test-core" ~doc:"Cap'n Proto RPC tests"
module Log = (val Logs.src_log src: Logs.LOG)

module String_content = struct
  module Path = struct
    type t = int
    let compare = compare
    let pp = Fmt.int
    let root = 0
  end

  module Request = struct
    type t = string
    let pp = Fmt.string
    let cap_index _ i = Some i
  end

  module Response = struct
    type t = string
    let pp = Fmt.string
    let cap_index _ i = Some i
    let bootstrap = "(boot)"
  end

  let ref_leak_detected fn =
    fn ();
    failwith "ref_leak_detected"
end

module Core_types = struct
  include Capnp_rpc.Core_types(String_content)

  type sturdy_ref
  type provision_id
  type recipient_id
  type third_party_cap_id
  type join_key_part
end

module type ENDPOINT = Capnp_rpc.Message_types.ENDPOINT with
  module Core_types = Core_types

module Local_struct_promise = Capnp_rpc.Local_struct_promise.Make(Core_types)
module Cap_proxy = Capnp_rpc.Cap_proxy.Make(Core_types)
