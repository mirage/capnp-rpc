module Types = struct
  type provision_id
  type recipient_id
  type third_party_cap_id = [`Two_party_only]
  type join_key_part
end

let parse_third_party_cap_id _ = `Two_party_only

let error fmt =
  fmt |> Fmt.kstrf @@ fun msg ->
  Error (`Msg msg)

let none_if_empty = function
  | None | Some "" -> None
  | Some _ as x -> x

module Address = struct
  type t = [
    | `Unix of string
    | `TCP of string * int
  ]

  let to_uri = function
    | `Unix path -> Uri.make ~scheme:"capnp" ~path ()
    | `TCP (host, port) -> Uri.make ~scheme:"capnp" ~host ~port ()

  let pp f = function
    | `Unix path -> Fmt.pf f "unix:%s" path
    | `TCP (host, port) -> Fmt.pf f "tcp:%s:%d" host port

  let parse_uri uri =
    let host = Uri.host uri |> none_if_empty in
    let port = Uri.port uri in
    let path = Uri.path uri in
    match host, port with
    | Some host, Some port when path = "" -> Ok (`TCP (host, port))
    | Some _,    Some _ -> error "Unexpected path component %S in %a" path Uri.pp_hum uri
    | Some _,    None   -> error "Missing port in %a" Uri.pp_hum uri
    | None,      Some _ -> error "Port without host in %a!" Uri.pp_hum uri
    | None,      None   -> Ok (`Unix path)

  let equal = ( = )
end
