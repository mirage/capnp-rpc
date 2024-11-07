open Auth
open Eio.Std

val connect_as_server :
  [> Eio.Flow.two_way_ty | Eio.Resource.close_ty] r -> Auth.Secret_key.t option ->
  (Endpoint.t, [> `Msg of string]) result

val connect_as_client :
  [> Eio.Flow.two_way_ty | Eio.Resource.close_ty] r -> Auth.Secret_key.t Lazy.t -> Digest.t ->
  (Endpoint.t, [> `Msg of string]) result
(** [connect_as_client underlying key digest] is an endpoint using flow [underlying].
    If [digest] requires TLS, it performs a TLS handshake. It uses [key] as its private key
    and checks that the server is the one required by [auth]. *)
