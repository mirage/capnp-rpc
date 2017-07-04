(** Parsing of Cap'n Proto RPC messages received from a remote peer. *)

val message :
  Schema.Reader.Message.t ->
  [ Capnp_core.Endpoint_types.In.t
  | `Unimplemented of Capnp_core.Endpoint_types.Out.t
  | `Not_implemented ]
(** Parse a message received from a peer.
    Returns [`Not_implemented] if we don't understand it. *)
