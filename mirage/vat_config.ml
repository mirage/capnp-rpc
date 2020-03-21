module Auth = Capnp_rpc_net.Auth
module Log = Capnp_rpc.Debug.Log

module Secret_hash : sig
  type t

  val of_pem_data : string -> t
  val to_string : t -> string
end = struct
  type t = string

  let of_pem_data data = Mirage_crypto.Hash.SHA256.digest (Cstruct.of_string data) |> Cstruct.to_string
  let to_string x = x
end

module Make (N : Capnp_rpc_net.S.NETWORK with type Address.t = Network.Location.t * Auth.Digest.t) = struct
  module Listen_address = struct
    type t = [`TCP of int]

    let pp f (`TCP port) =
      Fmt.pf f "tcp:%d" port
  end

  type t = {
    secret_key : (Auth.Secret_key.t * Secret_hash.t) Lazy.t;
    serve_tls : bool;
    listen_address : Listen_address.t;
    public_address : Network.Location.t;
  }

  let secret_key t = fst @@ Lazy.force t.secret_key

  let hashed_secret t = Secret_hash.to_string @@ snd @@ Lazy.force t.secret_key

  let create ~public_address ~secret_key ?(serve_tls=true) listen_address =
    let secret_key = lazy (
      match secret_key with
      | `PEM data -> (Auth.Secret_key.of_pem_data data, Secret_hash.of_pem_data data)
      | `Ephemeral ->
        let key = Auth.Secret_key.generate () in
        let data = Auth.Secret_key.to_pem_data key in
        (key, Secret_hash.of_pem_data data)
    ) in
    { secret_key; serve_tls; listen_address; public_address }

  let derived_id t name =
    let secret = hashed_secret t in
    Capnp_rpc_net.Restorer.Id.derived ~secret name

  let auth t =
    if t.serve_tls then Capnp_rpc_net.Auth.Secret_key.digest (secret_key t)
    else Capnp_rpc_net.Auth.Digest.insecure

  let sturdy_uri t service =
    let address = (t.public_address, auth t) in
    N.Address.to_uri (address, Capnp_rpc_net.Restorer.Id.to_string service)
end
