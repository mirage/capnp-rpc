(** A simple instantiation of Capnp_rpc for testing, in which messages are just strings. *)

let ref_leaks = ref 0

let src = Logs.Src.create "test-core" ~doc:"Cap'n Proto RPC tests"
module Log = (val Logs.src_log src: Logs.LOG)

module String_content = struct
  module Path = struct
    type t = int
    let compare = compare
    let pp = Fmt.int
    let root = 0
  end

  type request
  type response = request
  type 'a msg = {
    data : string;
    caps : Capnp_rpc.S.attachments;
  }

  module Request = struct
    type t = request msg

    let pp f t = Fmt.string f t.data
    let cap_index _ i = Some i

    let attachments t = t.caps

    let v data = {
      data;
      caps = Capnp_rpc.S.No_attachments;
    }

    let data t = t.data

    let bootstrap () = v "(boot)"

    let with_attachments caps t = { t with caps }
  end

  module Response = Request

  let ref_leak_detected fn =
    fn ();
    incr ref_leaks
end

module Core_types = Capnp_rpc.Core_types(String_content)

module Network_types = struct
  type provision_id
  type recipient_id
  type third_party_cap_id
  type join_key_part
end

module type ENDPOINT = Capnp_rpc.Message_types.ENDPOINT with
  module Core_types = Core_types and
  module Network_types = Network_types

module Local_struct_promise = Capnp_rpc.Local_struct_promise.Make(Core_types)
module Cap_proxy = Capnp_rpc.Cap_proxy.Make(Core_types)
