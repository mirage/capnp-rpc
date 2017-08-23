open Astring

module Log = Capnp_rpc.Debug.Log

module Listen_address = struct
  type t = [
    | `Unix of string
  ]

  let pp f = function
    | `Unix path -> Fmt.pf f "unix:%s" path

  open Cmdliner

  let of_string s =
    match String.cut ~sep:":" s with
    | None -> Error (`Msg "Missing ':'")
    | Some ("unix", path) -> Ok (`Unix path)
    | Some _ -> Error (`Msg "Only unix:PATH addresses are currently supported")

  let addr_conv = Arg.conv (of_string, pp)

  let cmd =
    let i = Arg.info ["listen-address"] ~docv:"ADDR" ~doc:"Address to listen on, e.g. $(b,unix:/run/my.socket)." in
    Arg.(required @@ opt (some addr_conv) None i)
end

type t = {
  listen_address : Listen_address.t;
  public_address : Listen_address.t;
}

let v ?public_address listen_address =
  let public_address =
    match public_address with
    | Some x -> x
    | None -> listen_address
  in
  { listen_address; public_address }

open Cmdliner

let public_address =
  let i = Arg.info ["public-address"] ~docv:"ADDR" ~doc:"Address to tell others to connect on" in
  Arg.(value @@ opt (some Listen_address.addr_conv) None i)

let cmd =
  let make listen_address public_address =
    let public_address =
      match public_address with
      | None -> listen_address
      | Some x -> x
    in
    { listen_address; public_address } in
  Term.(pure make $ Listen_address.cmd $ public_address)
