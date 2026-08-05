include Rpc_schema.Make(Capnp.BytesMessage)

module ReaderOps = struct
  include Capnp.Runtime.ReaderInc.Make(Capnp.RPC.None(Capnp.BytesMessage))

  let string_of_pointer = function
    | None -> ""
    | Some ptr ->
      let open Capnp.BytesMessage in
      let data = { ptr with Slice.len = 0 } in
      let ss = StructStorage.v ~data ~pointers:ptr in
      get_text ~default:"" (Some ss) 0
end

module BuilderOps = struct
  include Capnp.Runtime.BuilderInc.Make(Capnp.RPC.None(Capnp.BytesMessage))

  let write_string ptr s =
    let open Capnp.BytesMessage in
    let data = { ptr with Slice.len = 0 } in
    let ss = StructStorage.v ~data ~pointers:ptr in
    BA_.set_text ss 0 s
end
