open Asetmap

module Log = Capnp_rpc.Debug.Log

let default_rsa_key_bits = 2048
let default_hash = `SHA256

type hash = [`SHA256]
(* Note: if we add more hashes here, we need to modify [Vat] to store *all*
   hashes of peer's public keys when they connect. Otherwise, we might record
   a client as "sha-256:abc" but it later refers to itself in a sturdy ref as
   "sha-512:def". We need to detect it's the same peer. *)

let error fmt =
  fmt |> Fmt.kstr @@ fun msg ->
  Error (`Msg msg)

let ( >>= ) x f =
  match x with
  | Ok y -> f y
  | Error _ as e -> e

module Digest = struct
  type t = [`Insecure | `Fingerprint of hash * string]

  let equal = ( = )

  let insecure = `Insecure

  let alphabet = Base64.uri_safe_alphabet

  let string_of_hash = function
    | `SHA256 -> "sha-256"

  let parse_hash = function
    | "sha-256" -> Ok `SHA256
    | x -> error "Unknown hash type %S" x

  let parse_digest s =
    B64.decode ~alphabet ~pad:false s

  let parse hash digest =
    parse_hash hash >>= fun hash ->
    parse_digest digest >>= fun digest ->
    Ok (hash, digest)

  let of_certificate cert : t =
    let hash = default_hash in
    let digest = X509.Public_key.fingerprint ~hash (X509.Certificate.public_key cert) in
    `Fingerprint (hash, Cstruct.to_string digest)

  let add_to_uri t uri =
    match t with
    | `Insecure -> Uri.with_userinfo uri (Some "insecure")
    | `Fingerprint (hash, digest) ->
      let hash = string_of_hash hash in
      let digest = B64.encode ~alphabet ~pad:false digest in
      let uri = Uri.with_userinfo uri (Some hash) in
      Uri.with_password uri (Some digest)

  let pp f = function
    | `Insecure -> Fmt.string f "insecure"
    | `Fingerprint (hash, digest) -> Fmt.pf f "%s@%s" (string_of_hash hash) (B64.encode ~alphabet ~pad:false digest)

  let from_uri uri =
    let hash_type = Uri.user uri in
    let digest = Uri.password uri in
    match hash_type, digest with
    | Some "insecure", None -> Ok `Insecure
    | Some hash, Some digest ->
      parse hash digest >>= fun digest ->
      Ok (`Fingerprint digest)
    | None, _ -> Error (`Msg "Missing digest hash type (e.g. '...://sha256:...')")
    | Some _, None -> Error (`Msg "Missing digest value (e.g. '...://sha256:DIGEST@...' or '...://insecure@...')")

  let authenticator = function
    | `Insecure -> None
    | `Fingerprint (hash, digest) ->
      let hash = (hash :> Mirage_crypto.Hash.hash) in
      let fingerprint = Cstruct.of_string digest in
      Some (X509.Authenticator.server_key_fingerprint ~hash ~fingerprint ~time:(fun _ -> None))

  module Map = Map.Make(struct
      type nonrec t = t
      let compare = compare
    end)
end

module Secret_key = struct
  type t = {
    priv : X509.Private_key.t;
    certificates : Tls.Config.own_cert;
    tls_server_config : Tls.Config.server;
  }

  let equal a b = a.priv = b.priv

  let tls_server_config t = t.tls_server_config

  let tls_client_config t ~authenticator =
    Tls.Config.client ~certificates:t.certificates ~authenticator ()

  let digest ?(hash=default_hash) t =
    let nc_hash = (hash :> Mirage_crypto.Hash.hash) in
    let pub = X509.Private_key.public t.priv in
    let value = X509.Public_key.fingerprint ~hash:nc_hash pub |> Cstruct.to_string in
    `Fingerprint (hash, value)

  let pp_fingerprint hash f t =
    Digest.pp f (digest ~hash t)

  let date_time ~date ~time =
    let tz_offset_s = 0 in
    match Ptime.of_date_time (date, (time, tz_offset_s)) with
    | Some dt -> dt
    | None -> failwith "Invalid date_time!"

  let x509 t =
    let dn =
      [ X509.Distinguished_name.(Relative_distinguished_name.singleton (CN "capnp")) ]
    in
    match X509.Signing_request.create dn t with
    | Error (`Msg m) ->
      Fmt.failwith "x509 certificate signing request creation failed %s" m
    | Ok csr ->
      let valid_from = date_time ~date:(1970, 1, 1) ~time:(1, 1, 1) in
      (* RFC 5280 says expiration date should be GeneralizedTime value 99991231235959Z *)
      let valid_until = date_time ~date:(9999, 12, 31) ~time:(23, 59, 59) in
      X509.Signing_request.sign csr ~valid_from ~valid_until t dn |>
      function
      | Ok v -> v
      | Error err -> Fmt.failwith "x509 signing failed: %a" X509.Validation.pp_signature_error err

  let of_priv priv =
    let cert = x509 priv in
    let certificates = `Single ([cert], priv) in
    (* We require a client cert to get the client's public key, although
       we allow any client to connect. We just want to know they key so that
       if we later need to resolve a sturdy ref hosted at the client, we can
       reuse this connection. *)
    let authenticator ?ip:_ ~host:_ _ = Ok None in
    let tls_server_config = Tls.Config.server ~certificates ~authenticator () in
    { priv; certificates; tls_server_config }

  let generate () =
    Log.info (fun f -> f "Generating new private key...");
    let priv = Mirage_crypto_pk.Rsa.generate ~bits:default_rsa_key_bits () in
    let t = of_priv (`RSA priv) in
    Log.info (fun f -> f "Generated key with hash %a" (pp_fingerprint `SHA256) t);
    t

  let of_pem_data data =
    match X509.Private_key.decode_pem (Cstruct.of_string data) with
    | Ok priv -> of_priv priv
    | Error (`Msg msg) -> Fmt.failwith "Failed to parse secret key!@ %s" msg

  let to_pem_data t =
    X509.Private_key.encode_pem t.priv |> Cstruct.to_string
end
