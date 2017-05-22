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
    let cap_index _ i = i
  end

  module Response = struct
    type t = string
    let pp = Fmt.string
    let cap_index _ i = i
    let bootstrap = "(boot)"
  end
end

module Network_types = struct
  type sturdy_ref
  type provision_id
  type recipient_id
  type third_party_cap_id
  type join_key_part
end

include Capnp_rpc.Make(String_content)(Network_types)
