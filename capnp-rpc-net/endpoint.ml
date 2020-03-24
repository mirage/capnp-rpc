open Lwt.Infix

let src = Logs.Src.create "endpoint" ~doc:"Send and receive Cap'n'Proto messages"
module Log = (val Logs.src_log src: Logs.LOG)

let compression = `None

let record_sent_messages = false

type flow = Flow : (module Mirage_flow.S with type flow = 'a) * 'a -> flow

type t = {
  flow : flow;
  decoder : Capnp.Codecs.FramedStream.t;
  switch : Lwt_switch.t;
  peer_id : Auth.Digest.t;
}

let peer_id t = t.peer_id

let of_flow (type flow) ~switch ~peer_id (module F : Mirage_flow.S with type flow = flow) (flow:flow) =
  let generic_flow = Flow ((module F), flow) in
  let decoder = Capnp.Codecs.FramedStream.empty compression in
  { flow = generic_flow; decoder; switch; peer_id }

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
  let (Flow ((module F), flow)) = t.flow in
  let data = Capnp.Codecs.serialize ~compression msg in
  if record_sent_messages then dump_msg data;
  F.write flow (Cstruct.of_string data) >|= function
  | Ok ()
  | Error `Closed as e -> e
  | Error e -> Error (`Msg (Fmt.to_to_string F.pp_write_error e))

let rec recv t =
  let (Flow ((module F), flow)) = t.flow in
  match Capnp.Codecs.FramedStream.get_next_frame t.decoder with
  | _ when not (Lwt_switch.is_on t.switch) -> Lwt.return @@ Error `Closed
  | Ok msg -> Lwt.return (Ok (Capnp.BytesMessage.Message.readonly msg))
  | Error Capnp.Codecs.FramingError.Unsupported -> failwith "Unsupported Cap'n'Proto frame received"
  | Error Capnp.Codecs.FramingError.Incomplete ->
    Log.debug (fun f -> f "Incomplete; waiting for more data...");
    F.read flow >>= function
    | Ok (`Data data) ->
      Log.debug (fun f -> f "Read %d bytes" (Cstruct.len data));
      Capnp.Codecs.FramedStream.add_fragment t.decoder (Cstruct.to_string data);
      recv t
    | Ok `Eof ->
      Log.info (fun f -> f "Connection closed");
      Lwt_switch.turn_off t.switch >|= fun () ->
      Error `Closed
    | Error ex when Lwt_switch.is_on t.switch -> Capnp_rpc.Debug.failf "recv: %a" F.pp_error ex
    | Error _ -> Lwt.return (Error `Closed)

let disconnect t =
  Lwt_switch.turn_off t.switch

let pp_error f = function
  | `Closed -> Fmt.string f "Connection closed"
  | `Msg m -> Fmt.string f m
