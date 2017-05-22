open Capnp_core

type 'a t

module RO_array = Capnp_rpc.RO_array

val create : (Capnp.Message.rw Capnp.BytesMessage.Slice.t -> 'a) -> 'a t * 'a
val create_no_args : unit -> 'a t
val export : 'a t -> Core_types.cap -> Uint32.t
val get_call : 'a t -> Schema.Builder.Call.t
val caps : 'a t -> Core_types.cap RO_array.t
