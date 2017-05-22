module Capnp_content = struct
  module Path = struct
    type t = Xform.t list
    let compare = compare
    let pp = Fmt.Dump.list Xform.pp
    let root = []
  end

  module Request = struct
    type t = Rpc.req_msg
    let pp f _ = Fmt.string f "(request content)"
    let cap_index _ _ = 0       (* TODO *)
  end

  module Response = struct
    type t = Rpc.resp_msg
    let pp f _ = Fmt.string f "(response content)"
    let cap_index _ _ = 0       (* TODO *)

    let bootstrap =
      let open Schema.Builder in
      let msg = Message.init_root () in
      let ret = Message.return_init msg in
      let p = Return.results_init ret in
      Payload.content_set_interface p (Some Uint32.zero);   (* Cap index 0 *)
      Rpc.Builder ret
  end
end

module Network_types = struct
  type sturdy_ref
  type provision_id
  type recipient_id
  type third_party_cap_id = [`TODO_3rd_party]
  type join_key_part
end

include Capnp_rpc.Make(Capnp_content)(Network_types)
