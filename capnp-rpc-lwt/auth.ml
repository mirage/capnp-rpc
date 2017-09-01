module Log = Capnp_rpc.Debug.Log

let default_rsa_key_bits = 2048
let default_hash = `SHA256

type hash = [`SHA256]
(* Note: if we add more hashes here, we need to modify [Vat] to store *all*
   hashes of peer's public keys when they connect. Otherwise, we might record
   a client as "sha-256:abc" but it later refers to itself in a sturdy ref as
   "sha-512:def". We need to detect it's the same peer. *)

let error fmt =
  fmt |> Fmt.kstrf @@ fun msg ->
  Error (`Msg msg)

let ( >>= ) x f =
  match x with
  | Ok y -> f y
  | Error _ as e -> e

module Digest = struct
  type t = [`Insecure | `Fingerprint of hash * string]

  let equal = ( = )

  let insecure = `Insecure

  let alphabet = B64.uri_safe_alphabet

  let string_of_hash = function
    | `SHA256 -> "sha-256"

  let parse_hash = function
    | "sha-256" -> Ok `SHA256
    | x -> error "Unknown hash type %S" x

  let parse_digest s =
    try Ok (B64.decode ~alphabet s)
    with ex -> error "Bad base64 digest %S: %a" s Fmt.exn ex

  let parse hash digest =
    parse_hash hash >>= fun hash ->
    parse_digest digest >>= fun digest ->
    Ok (hash, digest)

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
      let hash = (hash :> Nocrypto.Hash.hash) in
      (* todo: [server_key_fingerprint] insists on checking the DN, so this must match
         the one in [Secret_key.x509]. Maybe we should make our own authenticator in case
         other implementations use other names. *)
      let fingerprints = ["capnp", Cstruct.of_string digest] in
      Some (X509.Authenticator.server_key_fingerprint ~hash ~fingerprints ?time:None)
end

module Secret_key = struct
  type t = {
    priv : Nocrypto.Rsa.priv;
    certificates : Tls.Config.own_cert;
  }

  let equal a b = a.priv = b.priv

  let certificates t = t.certificates

  let digest ?(hash=default_hash) t =
    let nc_hash = (hash :> Nocrypto.Hash.hash) in
    let pub = Nocrypto.Rsa.pub_of_priv t.priv in
    let value = X509.key_fingerprint ~hash:nc_hash (`RSA pub) |> Cstruct.to_string in
    `Fingerprint (hash, value)

  let pp_fingerprint hash f t =
    Digest.pp f (digest ~hash t)

  let x509 t =
    let dn = [`CN "capnp"] in
    let csr = X509.CA.request dn (`RSA t) in
    let valid_from = { Asn.Time.
                       date = (1970, 1, 1);
                       time = (1, 1, 1, 0.);
                       tz = None;
                     } in
    (* RFC 5280 says expiration date should be GeneralizedTime value 99991231235959Z *)
    let valid_until = { Asn.Time.
                        date = (9999, 12, 31);
                        time = (23, 59, 59, 0.);
                        tz = None;
                      } in
    X509.CA.sign csr ~valid_from ~valid_until (`RSA t) dn

  let of_priv priv =
    let cert = x509 priv in
    let certificates = `Single ([cert], priv) in
    { priv; certificates }

  let generate () =
    Log.info (fun f -> f "Generating new private key...");
    let priv = Nocrypto.Rsa.generate default_rsa_key_bits in
    let t = of_priv priv in
    Log.info (fun f -> f "Generated key with hash %a" (pp_fingerprint `SHA256) t);
    t

  let of_pem_data data =
    match X509.Encoding.Pem.Private_key.of_pem_cstruct1 (Cstruct.of_string data) with
    | `RSA priv -> of_priv priv

  let to_pem_data t =
    X509.Encoding.Pem.Private_key.to_pem_cstruct1 (`RSA t.priv) |> Cstruct.to_string
end

module Tls_wrapper (Underlying : Mirage_flow_lwt.S) = struct
  open Lwt.Infix

  module Flow = Tls_mirage.Make(Underlying)

  let plain_endpoint ~switch flow =
    Endpoint.of_flow ~switch (module Underlying) flow

  let connect_as_server ~switch flow secret_key =
    match secret_key with
    | None -> Lwt.return @@ Ok (plain_endpoint ~switch flow)
    | Some key ->
      Log.info (fun f -> f "Doing TLS server-side handshake...");
      let certificates = Secret_key.certificates key in
      let client_cert = ref None in
      let authenticator ?host:_ = function
        | [] -> `Fail `EmptyCertificateChain
        | client :: _ ->  (* First certificate is the client's own *)
          client_cert := Some client;
          `Ok None
      in
      let tls_config = Tls.Config.server ~certificates ~authenticator () in
      Flow.server_of_flow tls_config flow >|= function
      | Error e -> error "TLS connection failed: %a" Flow.pp_write_error e
      | Ok flow ->
        match !client_cert with
        | None -> failwith "No client certificate after auth!"
        | Some _client_cert ->
          Ok (Endpoint.of_flow ~switch (module Flow) flow)

  let connect_as_client ~switch flow secret_key auth =
    match Digest.authenticator auth with
    | None -> Lwt.return @@ Ok (plain_endpoint ~switch flow)
    | Some authenticator ->
      let certificates = Secret_key.certificates (Lazy.force secret_key) in
      let tls_config = Tls.Config.client ~certificates ~authenticator () in
      Log.info (fun f -> f "Doing TLS client-side handshake...");
      Flow.client_of_flow tls_config flow >|= function
      | Error e -> error "TLS connection failed: %a" Flow.pp_write_error e
      | Ok flow -> Ok (Endpoint.of_flow ~switch (module Flow) flow)
end
