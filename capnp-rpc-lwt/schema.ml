include Rpc_schema.Make(Capnp.BytesMessage)
module ReaderOps = Capnp.Runtime.ReaderInc.Make(Capnp.RPC.None(Capnp.BytesMessage))
