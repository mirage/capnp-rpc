open Lwt.Infix

(* Slightly rude to set signal handlers in a library, but SIGPIPE makes no sense
   in a modern application. *)
let () = Sys.(set_signal sigpipe Signal_ignore)

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
