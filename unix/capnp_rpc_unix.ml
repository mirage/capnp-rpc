open Astring
open Lwt.Infix
open Capnp_rpc_lwt

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

  let conv = Arg.conv (of_string, pp)
end

module Connect_address = Listen_address (* (for now) *)

let serve ?(backlog=5) ?offer addr =
  let vat = Vat.create ?bootstrap:offer () in
  let `Unix path = addr in
  begin match Unix.lstat path with
    | { Unix.st_kind = Unix.S_SOCK; _ } -> Unix.unlink path
    | _ -> ()
    | exception Unix.Unix_error(Unix.ENOENT, _, _) -> ()
  end;
  let socket = Unix.(socket PF_UNIX SOCK_STREAM 0) in
  Unix.bind socket (Unix.ADDR_UNIX path);
  Unix.listen socket backlog;
  Logs.info (fun f -> f "Waiting for connections on %a" Listen_address.pp addr);
  let lwt_socket = Lwt_unix.of_unix_file_descr socket in
  let rec loop () =
    Lwt_unix.accept lwt_socket >>= fun (client, _addr) ->
    Logs.info (fun f -> f "New connection on %S" path);
    let switch = Lwt_switch.create () in
    let ep = Capnp_rpc_lwt.Endpoint.of_socket ~switch (Lwt_unix.unix_file_descr client) in
    let _ : CapTP.t = Vat.connect vat ep in
    loop ()
  in
  loop ()

let connect ?switch ?offer (`Unix path) =
  let switch =
    match switch with
    | None -> Lwt_switch.create ()
    | Some x -> x
  in
  Logs.info (fun f -> f "Connecting to %S..." path);
  let socket = Unix.(socket PF_UNIX SOCK_STREAM 0) in
  Unix.connect socket (Unix.ADDR_UNIX path);
  let vat = Vat.create ~switch ?bootstrap:offer () in
  let ep = Endpoint.of_socket ~switch socket in
  let conn = Vat.connect vat ep in
  CapTP.bootstrap conn

