type 'a t

val create : (Capnp.Message.rw Capnp.BytesMessage.Slice.t -> 'a) -> 'a t * 'a
val create_no_args : unit -> 'a t
val export : 'a t -> Rpc.value -> Uint32.t
val get_call : 'a t -> Schema.Builder.Call.t
val caps : 'a t -> Rpc.value Ro_array.t
