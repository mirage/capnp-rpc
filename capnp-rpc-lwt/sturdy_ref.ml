let error fmt =
  fmt |> Fmt.kstrf @@ fun msg ->
  Error (`Msg msg)

let ( >>= ) x f =
  match x with
  | Ok y -> f y
  | Error _ as e -> e

module Make (N : S.NETWORK) = struct
  type service_id = string

  type 'a t = {
    address : N.Address.t;
    service : service_id;
  }

  let v ~address ~service = {address; service}

  let equal {address; service} b =
    N.Address.equal address b.address &&
    service = b.service

  let address t = t.address
  let service t = t.service
  let cast t = (t :> _ t)

  let to_uri_with_secrets {address; service} =
    N.Address.to_uri address service

  let pp_with_secrets f t = Uri.pp_hum f (to_uri_with_secrets t)

  let pp_address f t =
    Fmt.pf f "<SturdyRef at %a>" N.Address.pp t.address

  let parse_capnp_uri uri =
    N.Address.parse_uri uri >>= fun (address, service) ->
    Ok (v ~address ~service)

  let of_uri uri =
    match Uri.scheme uri with
    | Some "capnp" -> parse_capnp_uri uri
    | Some scheme -> error "Unknown scheme %S (expected 'capnp://...')" scheme
    | None -> error "Missing scheme in %a (expected 'capnp://...')" Uri.pp_hum uri
end
