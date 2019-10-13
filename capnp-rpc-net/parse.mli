(** Parsing of Cap'n Proto RPC messages received from a remote peer. *)
open Capnp_rpc_lwt.Private

module Make (EP : Capnp_core.ENDPOINT) (Network : S.NETWORK with module Types = EP.Network_types) : sig
  val message :
    Schema.Reader.Message.t ->
    [ EP.In.t
    | `Unimplemented of EP.Out.t
    | `Not_implemented ]
  (** Parse a message received from a peer.
      Returns [`Not_implemented] if we don't understand it. *)
end
