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

  let messages_outbound_sent_total =
    let help = "Total number of messages transmitted" in
    Counter.v ~help ~namespace ~subsystem "messages_outbound_sent_total"

  let messages_outbound_dropped_total =
    let help = "Total number of messages lost due to disconnections" in
    Counter.v ~help ~namespace ~subsystem "messages_outbound_dropped_total"
end

module Log = Capnp_rpc.Debug.Log

module Builder = Capnp_rpc.Private.Schema.Builder
module Reader = Capnp_rpc.Private.Schema.Reader

module Table_types = Capnp_rpc_proto.Message_types.Table_types ( )

module Make (Network : S.NETWORK) = struct
  module Endpoint_types = Capnp_rpc_proto.Message_types.Endpoint(Capnp_rpc.Private.Capnp_core.Core_types)(Network.Types)(Table_types)
  module Conn = Capnp_rpc_proto.CapTP.Make(Endpoint_types)
  module Parse = Parse.Make(Endpoint_types)(Network)
  module Serialise = Serialise.Make(Endpoint_types)

  type t = {
    endpoint : Endpoint.t;
    conn : Conn.t;
    xmit_queue : Capnp.Message.rw Capnp.BytesMessage.Message.t Eio.Stream.t;
    mutable disconnecting : bool;
  }

  let bootstrap t id = Conn.bootstrap t.conn id |> Capnp_rpc.Cast.cap_of_raw

  let pp_msg f call =
    let open Reader in
    let call = Capnp_rpc.Private.Msg.Request.readable call in
    let interface_id = Call.interface_id_get call in
    let method_id = Call.method_id_get call in
    Capnp.RPC.Registry.pp_method f (interface_id, method_id)

  let tags t = Conn.tags t.conn

  let drop_queue q =
    let len = Eio.Stream.length q in
    Prometheus.Counter.inc Metrics.messages_outbound_dropped_total (float_of_int len)
    (* Queue.clear q           -- could close stream here instead *)

  (* [flush ~xmit_queue endpoint] writes each message in [xmit_queue] to [endpoint]. *)
  let rec flush ~xmit_queue endpoint =
    let next = Eio.Stream.take xmit_queue in
    match Endpoint.send endpoint next with
    | Error `Closed ->
      Endpoint.disconnect endpoint;      (* We'll read a close soon *)
      drop_queue xmit_queue;
      `Stop_daemon
    | Error (`Msg msg) ->
      Log.warn (fun f -> f "Error sending messages: %s (will shutdown connection)" msg);
      Endpoint.disconnect endpoint;
      drop_queue xmit_queue;
      `Stop_daemon
    | Ok () ->
      Prometheus.Counter.inc_one Metrics.messages_outbound_sent_total;
      flush ~xmit_queue endpoint
    | exception ex ->
      drop_queue xmit_queue;
      raise ex

  (* Enqueue [message] in [xmit_queue] and ensure the flush thread is running. *)
  let queue_send ~xmit_queue message =
    Log.debug (fun f ->
        let module M = Capnp_rpc.Private.Schema.MessageWrapper.Message in
        f "queue_send: %d/%d allocated bytes in %d segs"
                  (M.total_size message)
                  (M.total_alloc_size message)
                  (M.num_segments message));
    Eio.Stream.add xmit_queue message;
    Prometheus.Counter.inc_one Metrics.messages_outbound_enqueued_total

  let return_not_implemented t x =
    Log.debug (fun f -> f ~tags:(tags t) "Returning Unimplemented");
    let open Builder in
    let m = Message.init_root () in
    let _ : Builder.Message.t = Message.unimplemented_set_reader m x in
    queue_send ~xmit_queue:t.xmit_queue (Message.to_message m)

  let listen t =
    let rec loop () =
      match Endpoint.recv t.endpoint with
      | Error e -> e
      | Ok msg ->
        let open Reader.Message in
        let msg = of_message msg in
        Prometheus.Counter.inc_one Metrics.messages_inbound_received_total;
        match Parse.message msg with
        | #Endpoint_types.In.t as msg ->
          Log.debug (fun f ->
              let tags = Endpoint_types.In.with_qid_tag (Conn.tags t.conn) msg in
              f ~tags "<- %a" (Endpoint_types.In.pp_recv pp_msg) msg);
          begin match msg with
            | `Abort _ ->
              t.disconnecting <- true;
              Conn.handle_msg t.conn msg;
              Endpoint.disconnect t.endpoint;
              `Aborted
            | _ ->
              Conn.handle_msg t.conn msg;
              loop ()
          end
        | `Unimplemented x as msg ->
          Log.info (fun f ->
              let tags = Endpoint_types.Out.with_qid_tag (Conn.tags t.conn) x in
              f ~tags "<- Unimplemented(%a)" (Endpoint_types.Out.pp_recv pp_msg) x);
          Conn.handle_msg t.conn msg;
          loop ()
        | `Not_implemented ->
          Log.info (fun f -> f "<- unsupported message type");
          return_not_implemented t msg;
          loop ()
    in
    loop ()

  let send_abort t ex =
    queue_send ~xmit_queue:t.xmit_queue (Serialise.message (`Abort ex))

  let disconnect t ex =
    if not t.disconnecting then (
      t.disconnecting <- true;
      send_abort t ex;
      Endpoint.disconnect t.endpoint;
      Conn.disconnect t.conn ex
    )

  let disconnecting t = t.disconnecting

  let connect ~sw ~restore ?(tags=Logs.Tag.empty) endpoint =
    let xmit_queue = Eio.Stream.create 100 in   (* todo: tune this? make it configurable? *)
    Fiber.fork_daemon ~sw (fun () -> flush ~xmit_queue endpoint);
    let queue_send msg = Eio.Stream.add xmit_queue (Serialise.message msg) in
    let restore = Restorer.fn restore in
    let fork = Fiber.fork ~sw in
    let conn = Conn.create ~restore ~tags ~fork ~queue_send in
    {
      conn;
      endpoint;
      xmit_queue;
      disconnecting = false;
    }

  let listen t =
    Prometheus.Gauge.inc_one Metrics.connections;
    let tags = Conn.tags t.conn in
    begin
      match listen t with
      | `Closed | `Aborted -> ()
      | exception Eio.Cancel.Cancelled ex ->
        Log.debug (fun f -> f ~tags "Cancelled: %a" Fmt.exn ex)
      | exception ex ->
        Log.warn (fun f ->
            f ~tags "Uncaught exception handling CapTP connection: %a (dropping connection)" Fmt.exn ex
          );
        send_abort t (Capnp_rpc.Exception.v ~ty:`Failed (Printexc.to_string ex))
    end;
    Log.info (fun f -> f ~tags "Connection closed");
    Prometheus.Gauge.dec_one Metrics.connections;
    Eio.Cancel.protect (fun () ->
        disconnect t (Capnp_rpc.Exception.v ~ty:`Disconnected "Connection closed")
      )

  let dump f t = Conn.dump f t.conn
end
