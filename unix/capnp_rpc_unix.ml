open Astring
open Lwt.Infix

(* Slightly rude to set signal handlers in a library, but SIGPIPE makes no sense
   in a modern application. *)
let () = Sys.(set_signal sigpipe Signal_ignore)

module Networking = Capnp_rpc_lwt.Networking (Capnp_rpc_lwt.Two_party_network)

module Unix_flow = struct
  type buffer = Cstruct.t
  type flow = {
    fd : Lwt_unix.file_descr;
    mutable current_write : int Lwt.t option;
    mutable current_read : int Lwt.t option;
    mutable closed : bool;
  }
  type error = [`Closed | `Exception of exn]
  type write_error = [`Closed | `Exception of exn]
  type 'a io = 'a Lwt.t

  let opt_cancel = function
    | None -> ()
    | Some x -> Lwt.cancel x

  let close t =
    assert (not t.closed);
    t.closed <- true;
    opt_cancel t.current_read;
    opt_cancel t.current_write;
    Lwt_unix.close t.fd

  let pp_error f = function
    | `Exception ex -> Fmt.exn f ex
    | `Closed -> Fmt.string f "Closed"

  let pp_write_error = pp_error

  let write t buf =
    let rec aux buf =
      if t.closed then Lwt.return (Error `Closed)
      else (
        assert (t.current_write = None);
        let write_thread = Lwt_cstruct.write t.fd buf in
        t.current_write <- Some write_thread;
        write_thread >>= fun wrote ->
        t.current_write <- None;
        if wrote = Cstruct.len buf then Lwt.return (Ok ())
        else aux (Cstruct.shift buf wrote)
      )
    in
    Lwt.catch
      (fun () -> aux buf)
      (fun ex -> Lwt.return @@ Error (`Exception ex))

  let rec writev t = function
    | [] -> Lwt.return (Ok ())
    | x :: xs ->
      write t x >>= function
      | Ok () -> writev t xs
      | Error _ as e -> Lwt.return e

  let read t =
    let len = 4096 in
    let buf = Cstruct.create_unsafe len in
    Lwt.try_bind
      (fun () ->
         assert (t.current_read = None);
         if t.closed then raise Lwt.Canceled;
         let read_thread = Lwt_cstruct.read t.fd buf in
         t.current_read <- Some read_thread;
         read_thread
      )
      (function
        | 0 ->
          Lwt.return @@ Ok `Eof
        | got ->
          t.current_read <- None;
          Lwt.return @@ Ok (`Data (Cstruct.sub buf 0 got))
      )
      (function
        | Lwt.Canceled -> Lwt.return @@ Error `Closed
        | ex -> Lwt.return @@ Error (`Exception ex)
      )

  let connect ?switch fd =
    let t = { fd; closed = false; current_read = None; current_write = None } in
    Lwt_switch.add_hook switch (fun () -> close t);
    t
end

let endpoint_of_socket ~switch socket =
  Capnp_rpc_lwt.Endpoint.of_flow ~switch (module Unix_flow) (Unix_flow.connect ~switch socket)

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
  let vat = Networking.Vat.create ?bootstrap:offer () in
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
    let ep = endpoint_of_socket ~switch client in
    let _ : Networking.CapTP.t = Networking.Vat.connect vat ep in
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
  let vat = Networking.Vat.create ~switch ?bootstrap:offer () in
  let ep = endpoint_of_socket ~switch (Lwt_unix.of_unix_file_descr socket) in
  let conn = Networking.Vat.connect vat ep in
  Networking.CapTP.bootstrap conn

