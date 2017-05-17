(** Provides the RPC layer on top of [Endpoint]. *)

type t

val of_endpoint : ?offer:Rpc.value -> ?tags:Logs.Tag.set -> switch:Lwt_switch.t -> Endpoint.t -> t
(** [of_endpoint ?offer ~switch endpoint] is fresh CapTP state for communicating with [endpoint].
    If [offer] is given, the peer can use the "Bootstrap" message to get access to it.
    If the connection fails then [switch] will be turned off. *)

val bootstrap : t -> Rpc.value
(** [bootstrap t] is the peer's public bootstrap object, if any. *)

val qid_tag : Uint32.t Logs.Tag.def
(** [qid_tag] is used in log reports to tag the question (or answer) ID in the call. *)
