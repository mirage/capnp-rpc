type 'a t

module RO_array = Capnp_rpc.RO_array

val create : (Capnp.Message.rw Capnp.BytesMessage.Slice.t -> 'a) -> 'a t * 'a
val create_no_args : unit -> 'a t
val finish : interface_id:Uint64.t -> method_id:int -> 'a t -> Msg.Request.t
val release : 'a t -> unit
