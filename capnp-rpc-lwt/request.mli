type 'a t

module RO_array = Capnp_rpc.RO_array

val create : ?message_size:int -> (Capnp.Message.rw Capnp.BytesMessage.Slice.t -> 'a) -> 'a t * 'a
val create_no_args : unit -> 'a t
val finish : (_, 'a, _) Capnp.RPC.MethodID.t -> 'a t -> Msg.Request.t
val release : 'a t -> unit
