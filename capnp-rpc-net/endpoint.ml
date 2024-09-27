open Eio.Std

module Metrics = struct
  open Prometheus

  let namespace = "capnp"

  let subsystem = "net"

  let connections =
    let help = "Number of live capnp-rpc connections" in
    Gauge.v ~help ~namespace ~subsystem "connections"

  let messages_inbound_received_total =
    let help = "Total number of messages received" in
    Counter.v ~help ~namespace ~subsystem "messages_inbound_received_total"

  let messages_outbound_enqueued_total =
    let help = "Total number of messages enqueued to be transmitted" in
    Counter.v ~help ~namespace ~subsystem "messages_outbound_enqueued_total"
end

module Write = Eio.Buf_write

let src = Logs.Src.create "endpoint" ~doc:"Send and receive Cap'n'Proto messages"
module Log = (val Logs.src_log src: Logs.LOG)

let compression = `None

let record_sent_messages = false

type flow = Eio.Flow.two_way_ty r

type t = {
  flow : flow;
  writer : Write.t;
  decoder : Capnp.Codecs.FramedStream.t;
  peer_id : Auth.Digest.t;
}

let peer_id t = t.peer_id

let dump_msg =
  let next = ref 0 in
  fun data ->
    let name = Fmt.str "/tmp/msg-%d.capnp" !next in
    Log.info (fun f -> f "Saved message as %S" name);
    incr next;
    let ch = open_out_bin name in
    output_string ch data;
    close_out ch

let disconnect t =
  try
    Eio.Flow.shutdown t.flow `All
  with Eio.Io (Eio.Net.E Connection_reset _, _) ->
    (* TCP connection already shut down, so TLS shutdown failed. Ignore. *)
    ()

let send t msg =
  Log.debug (fun f ->
      let module M = Capnp_rpc_lwt.Private.Schema.MessageWrapper.Message in
      f "queue_send: %d/%d allocated bytes in %d segs"
        (M.total_size msg)
        (M.total_alloc_size msg)
        (M.num_segments msg));
  Capnp.Codecs.serialize_iter_copyless ~compression msg ~f:(fun x len -> Write.string t.writer x ~len);
  Prometheus.Counter.inc_one Metrics.messages_outbound_enqueued_total;
  if record_sent_messages then dump_msg (Capnp.Codecs.serialize ~compression msg)

let rec run_writer t =
  let bufs = Write.await_batch t.writer in
  match Eio.Flow.single_write t.flow bufs with
  | n -> Write.shift t.writer n; run_writer t
  | exception (Eio.Io (Eio.Net.E Connection_reset _, _) as ex) ->
    Log.info (fun f -> f "%a" Eio.Exn.pp ex);
    disconnect t            (* We'll read a close soon *)
  | exception ex ->
    Eio.Fiber.check ();
    Log.warn (fun f -> f "Error sending messages: %a (will shutdown connection)" Fmt.exn ex);
    disconnect t

let of_flow ~sw ~peer_id flow =
  let decoder = Capnp.Codecs.FramedStream.empty compression in
  let flow = (flow :> flow) in
  let writer = Write.create 4096 in
  let t = { flow; writer; decoder; peer_id } in
  Prometheus.Gauge.inc_one Metrics.connections;
  Switch.on_release sw (fun () -> Prometheus.Gauge.dec_one Metrics.connections);
  Fiber.fork_daemon ~sw (fun () -> run_writer t; `Stop_daemon);
  t

let rec recv t =
  match Capnp.Codecs.FramedStream.get_next_frame t.decoder with
  | Ok msg ->
    Prometheus.Counter.inc_one Metrics.messages_inbound_received_total;
    (* We often want to send multiple response messages while processing a batch of requests,
       so pause the writer to collect them. We'll unpause on the next read. *)
    Write.pause t.writer;
    Ok (Capnp.BytesMessage.Message.readonly msg)
  | Error Capnp.Codecs.FramingError.Unsupported -> failwith "Unsupported Cap'n'Proto frame received"
  | Error Capnp.Codecs.FramingError.Incomplete ->
    Log.debug (fun f -> f "Incomplete; waiting for more data...");
    (* We probably scheduled one or more application fibers to run while handling the last
       batch of messages. Given them a chance to run now while the writer is paused, because
       they might want to send more messages immediately. *)
    Fiber.yield ();
    Write.unpause t.writer;
    let buf = Cstruct.create 4096 in    (* TODO: make this efficient *)
    match Eio.Flow.single_read t.flow buf with
    | got ->
      Log.debug (fun f -> f "Read %d bytes" got);
      Capnp.Codecs.FramedStream.add_fragment t.decoder (Cstruct.to_string buf ~len:got);
      recv t
    | exception End_of_file ->
      Error `Closed
    | exception (Eio.Io (Eio.Net.E Connection_reset _, _) as ex) ->
      Log.info (fun f -> f "%a" Eio.Exn.pp ex);
      Error `Closed
