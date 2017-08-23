open Lwt.Infix

include Capnp_rpc_lwt.Networking (Network)
module Vat_config = Vat_config

let endpoint_of_socket ~switch socket =
  Capnp_rpc_lwt.Endpoint.of_flow ~switch (module Unix_flow) (Unix_flow.connect ~switch socket)

module Connect_address = struct
  include Vat_config.Listen_address (* (for now) *)

  let conv = Vat_config.Listen_address.addr_conv
end

let serve ?(backlog=5) ?offer {Vat_config.listen_address; public_address = _} =
  let vat = Vat.create ?bootstrap:offer () in
  let `Unix path = listen_address in
  begin match Unix.lstat path with
    | { Unix.st_kind = Unix.S_SOCK; _ } -> Unix.unlink path
    | _ -> ()
    | exception Unix.Unix_error(Unix.ENOENT, _, _) -> ()
  end;
  let socket = Unix.(socket PF_UNIX SOCK_STREAM 0) in
  Unix.bind socket (Unix.ADDR_UNIX path);
  Unix.listen socket backlog;
  Logs.info (fun f -> f "Waiting for connections on %a" Vat_config.Listen_address.pp listen_address);
  let lwt_socket = Lwt_unix.of_unix_file_descr socket in
  let rec loop () =
    Lwt_unix.accept lwt_socket >>= fun (client, _addr) ->
    Logs.info (fun f -> f "New connection on %S" path);
    let switch = Lwt_switch.create () in
    let ep = endpoint_of_socket ~switch client in
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
  let ep = endpoint_of_socket ~switch (Lwt_unix.of_unix_file_descr socket) in
  let conn = Vat.connect vat ep in
  CapTP.bootstrap conn

