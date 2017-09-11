open Capnp_rpc

(* For each capability and struct_ref in the real system, we make a corresponding
   "direct" object. While a real capability must be converted to an export
   and sent as an integer ID over the network, its direct equivalent is included
   directly in the message content (this is possible only because we're running
   everything in a single process for testing).
   When a call is received and dispatched to a service by the CapTP system, the
   test service checks that the direct target in the message body matches the
   service's target. This ensures that the message was delivered to its intended
   recipient. *)

type cap
type struct_ref

val null : cap

val make_cap : unit -> cap
val equal : cap -> cap -> bool

val cancelled : struct_ref

val make_struct : unit -> struct_ref
val cap : struct_ref -> int -> cap
val return : struct_ref -> cap RO_array.t -> unit

val return_tail : struct_ref -> src:struct_ref -> unit
(** [return_tail x ~src] means that promise [x] has resolved to [src].
    If [x] is cancelled (now or later), [src] will be cancelled too. *)

val mark_cancelled : struct_ref -> unit
val on_cancel : struct_ref -> (unit -> unit) -> unit

val compare_cap : cap -> cap -> int
val compare_sr : struct_ref -> struct_ref -> int

val pp : cap Fmt.t
val pp_struct : struct_ref Fmt.t
