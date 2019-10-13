open Capnp_rpc_lwt.Private

module Make (EP : Capnp_core.ENDPOINT) : sig
  val message : EP.Out.t -> Rpc_schema.rw Schema.message_t
end
