open Auth

module Make (Underlying : Mirage_flow_lwt.S) : sig
  (** Make an [Endpoint] from an [Underlying.flow], using TLS if appropriate. *)

  val connect_as_server :
    switch:Lwt_switch.t -> Underlying.flow -> Auth.Secret_key.t option ->
    (Endpoint.t, [> `Msg of string]) result Lwt.t

  val connect_as_client :
    switch:Lwt_switch.t -> Underlying.flow -> Auth.Secret_key.t Lazy.t -> Digest.t ->
    (Endpoint.t, [> `Msg of string]) result Lwt.t
  (** [connect_as_client ~switch underlying key digest] is an endpoint using flow [underlying].
      If [digest] requires TLS, it performs a TLS handshake. It uses [key] as its private key
      and checks that the server is the one required by [auth]. *)
end

