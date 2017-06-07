open Lwt.Infix

let src = Logs.Src.create "endpoint" ~doc:"Send and receive Cap'n'Proto messages"
module Log = (val Logs.src_log src: Logs.LOG)

(* Slight rude to set signal handlers in a library, but SIGPIPE makes no sense
   in a modern application. *)
let () = Sys.(set_signal sigpipe Signal_ignore)

let compression = `None

let record_sent_messages = false

type t = {
  to_remote : Lwt_io.output Lwt_io.channel;
  from_remote : Lwt_io.input Lwt_io.channel;
  decoder : Capnp.Codecs.FramedStream.t;
  switch : Lwt_switch.t;
}

let of_socket ~switch socket =
  let from_remote = Lwt_io.(of_unix_fd ~mode:input ~close:Lwt.return) socket in
  let to_remote = Lwt_io.(of_unix_fd ~mode:output ~close:Lwt.return) socket in
  Lwt_switch.add_hook (Some switch) (fun () ->
      Lwt_io.close from_remote >>= fun () ->
      Lwt_io.close to_remote >>= fun () ->
      Unix.close socket;
      Lwt.return ()
    );
  let decoder = Capnp.Codecs.FramedStream.empty compression in
  { from_remote; to_remote; decoder; switch }

let of_flow (type flow) ~switch (module F : Mirage_flow_lwt.S with type flow = flow) (flow:flow) =
  let module U = Mirage_flow_unix.Make(F) in
  let to_remote = U.oc ~close:false flow in
  let from_remote = U.ic ~close:false flow in
  Lwt_switch.add_hook (Some switch) (fun () ->
      Lwt_io.close from_remote >>= fun () ->
      Lwt_io.close to_remote >>= fun () ->
      F.close flow
    );
  let decoder = Capnp.Codecs.FramedStream.empty compression in
  { from_remote; to_remote; decoder; switch }

let dump_msg =
  let next = ref 0 in
  fun data ->
    let name = Fmt.strf "/tmp/msg-%d.capnp" !next in
    Log.info (fun f -> f "Saved message as %S" name);
    incr next;
    let ch = open_out_bin name in
    output_string ch data;
    close_out ch

let send t msg =
  t.to_remote |> Lwt_io.atomic @@ fun to_remote ->
  let data = Capnp.Codecs.serialize ~compression msg in
  if record_sent_messages then dump_msg data;
  Lwt_io.write to_remote data

let rec recv t =
  match Capnp.Codecs.FramedStream.get_next_frame t.decoder with
  | Ok msg -> Lwt.return (Ok (Capnp.BytesMessage.Message.readonly msg))
  | Error Capnp.Codecs.FramingError.Unsupported -> failwith "Unsupported Cap'n'Proto frame received"
  | Error Capnp.Codecs.FramingError.Incomplete ->
    Log.debug (fun f -> f "Incomplete; waiting for more data...");
    Lwt.try_bind
      (fun () -> Lwt_io.read ~count:4096 t.from_remote)
      (function
        | "" ->
          Log.info (fun f -> f "Connection closed");
          Lwt_switch.turn_off t.switch >|= fun () ->
          Error `Closed
        | data ->
          Log.debug (fun f -> f "Got %S" data);
          Capnp.Codecs.FramedStream.add_fragment t.decoder data;
          recv t
      )
      (function
        | Lwt_io.Channel_closed _ -> Lwt_switch.turn_off t.switch >|= fun () -> Error `Closed
        | ex -> Lwt.fail ex
      )
