open Astring

let error fmt =
  fmt |> Fmt.kstr @@ fun msg ->
  Error (`Msg msg)

let none_if_empty = function
  | None | Some "" -> None
  | Some _ as x -> x

module Location = struct
  type t = [
    | `Unix of string
    | `TCP of string * int
  ]

  let pp f = function
    | `Unix path -> Fmt.pf f "unix:%s" path
    | `TCP (host, port) -> Fmt.pf f "tcp:%s:%d" host port

  let equal = ( = )
end

type t = Location.t * Auth.Digest.t

let digest = snd

let alphabet = Base64.uri_safe_alphabet

let b64encode = B64.encode ~alphabet ~pad:false
let b64decode = B64.decode ~alphabet ~pad:false

let to_uri ((addr, auth), service_id) =
  let service_id = b64encode service_id in
  let uri =
    match addr with
    | `Unix path ->
      let path = Printf.sprintf "%s/%s" path service_id in
      Uri.make ~scheme:"capnp" ~path ()
    | `TCP (host, port) ->
      Uri.make ~scheme:"capnp" ~host ~port ~path:service_id ()
  in
  Auth.Digest.add_to_uri auth uri

let pp f t =
  Uri.pp_hum f (to_uri (t, ""))

let ( >>= ) x f =
  match x with
  | Error _ as e -> e
  | Ok y -> f y

let strip_leading_slash s =
  if String.is_prefix ~affix:"/" s then String.with_range ~first:1 s
  else s

let check_sheme uri =
  match Uri.scheme uri with
  | Some "capnp" -> Ok ()
  | Some scheme -> error "Unknown scheme %S (expected 'capnp://...')" scheme
  | None -> error "Missing scheme in %a (expected 'capnp://...')" Uri.pp_hum uri

let parse_uri uri =
  check_sheme uri >>= fun () ->
  let host = Uri.host uri |> none_if_empty in
  let port = Uri.port uri in
  let path = Uri.path uri in
  Auth.Digest.from_uri uri >>= fun auth ->
  match host, port with
  | Some host, Some port ->
    b64decode (strip_leading_slash path) >>= fun service_id ->
    Ok ((`TCP (host, port), auth), service_id)
  | Some _,    None   -> error "Missing port in %a" Uri.pp_hum uri
  | None,      Some _ -> error "Port without host in %a!" Uri.pp_hum uri
  | None,      None   ->
    match String.cut ~rev:true ~sep:"/" path with
    | None -> Ok ((`Unix path, auth), "")
    | Some (path, service_id) ->
      b64decode service_id >>= fun service_id ->
      Ok ((`Unix path, auth), service_id)

let equal (addr, auth) (addr_b, auth_b) =
  Location.equal addr addr_b &&
  Auth.Digest.equal auth auth_b
