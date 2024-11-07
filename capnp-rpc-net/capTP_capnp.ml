open Lwt.Infix

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
    xmit_queue : Capnp.Message.rw Capnp.BytesMessage.Message.t Queue.t;
    mutable disconnecting : bool;
  }

  let bootstrap t id = Conn.bootstrap t.conn id |> Capnp_rpc.Cast.cap_of_raw

  let async_tagged label fn =
    Lwt.async
      (fun () ->
         Lwt.catch fn
           (fun ex ->
              Log.warn (fun f -> f "Uncaught async exception in %S: %a" label Fmt.exn ex);
              Lwt.return_unit
           )
      )

  let pp_msg f call =
    let open Reader in
    let call = Capnp_rpc.Private.Msg.Request.readable call in
    let interface_id = Call.interface_id_get call in
    let method_id = Call.method_id_get call in
    Capnp.RPC.Registry.pp_method f (interface_id, method_id)

  let tags t = Conn.tags t.conn

  let drop_queue q =
    Prometheus.Counter.inc Metrics.messages_outbound_dropped_total (float_of_int (Queue.length q));
    Queue.clear q

  (* [flush ~xmit_queue endpoint] writes each message in the queue until it is empty.
     Invariant:
       Whenever Lwt blocks or switches threads, a flush thread is running iff the
       queue is non-empty. *)
  let rec flush ~xmit_queue endpoint =
    (* We keep the item on the queue until it is transmitted, as the queue state
       tells us whether there is a [flush] currently running. *)
    let next = Queue.peek xmit_queue in
    Endpoint.send endpoint next >>= function
    | Error `Closed ->
      Endpoint.disconnect endpoint >|= fun () ->      (* We'll read a close soon *)
      drop_queue xmit_queue
    | Error e ->
      Log.warn (fun f -> f "Error sending messages: %a (will shutdown connection)" Endpoint.pp_error e);
      Endpoint.disconnect endpoint >|= fun () ->
      drop_queue xmit_queue
    | Ok () ->
      Prometheus.Counter.inc_one Metrics.messages_outbound_sent_total;
      ignore (Queue.pop xmit_queue);
      if not (Queue.is_empty xmit_queue) then
        flush ~xmit_queue endpoint
      else (* queue is empty and flush thread is done *)
        Lwt.return_unit

  (* Enqueue [message] in [xmit_queue] and ensure the flush thread is running. *)
  let queue_send ~xmit_queue endpoint message =
    Log.debug (fun f ->
        let module M = Capnp_rpc.Private.Schema.MessageWrapper.Message in
        f "queue_send: %d/%d allocated bytes in %d segs"
                  (M.total_size message)
                  (M.total_alloc_size message)
                  (M.num_segments message));
    let was_idle = Queue.is_empty xmit_queue in
    Queue.add message xmit_queue;
    Prometheus.Counter.inc_one Metrics.messages_outbound_enqueued_total;
    if was_idle then async_tagged "Message sender thread" (fun () -> flush ~xmit_queue endpoint)

  let return_not_implemented t x =
    Log.debug (fun f -> f ~tags:(tags t) "Returning Unimplemented");
    let open Builder in
    let m = Message.init_root () in
    let _ : Builder.Message.t = Message.unimplemented_set_reader m x in
    queue_send ~xmit_queue:t.xmit_queue t.endpoint (Message.to_message m)

  let listen t =
    let rec loop () =
      Endpoint.recv t.endpoint >>= function
      | Error e -> Lwt.return e
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
              Endpoint.disconnect t.endpoint >>= fun () ->
              Lwt.return `Aborted
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

  let disconnect t ex =
    if not t.disconnecting then (
      t.disconnecting <- true;
      queue_send ~xmit_queue:t.xmit_queue t.endpoint (Serialise.message (`Abort ex));
      Endpoint.disconnect t.endpoint >|= fun () ->
      Conn.disconnect t.conn ex
    ) else (
      Lwt.return_unit
    )

  let disconnecting t = t.disconnecting

  let connect ~restore ?(tags=Logs.Tag.empty) endpoint =
    let xmit_queue = Queue.create () in
    let queue_send msg = queue_send ~xmit_queue endpoint (Serialise.message msg) in
    let restore = Restorer.fn restore in
    let conn = Conn.create ~restore ~tags ~queue_send in
    let t = {
      conn;
      endpoint;
      xmit_queue;
      disconnecting = false;
    } in
    Prometheus.Gauge.inc_one Metrics.connections;
    Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
             listen t >|= fun (`Closed | `Aborted) -> ()
          )
          (fun ex ->
             Log.warn (fun f ->
                 f ~tags "Uncaught exception handling CapTP connection: %a (dropping connection)" Fmt.exn ex
               );
             queue_send @@ `Abort (Capnp_rpc.Exception.v ~ty:`Failed (Printexc.to_string ex));
             Lwt.return_unit
          )
        >>= fun () ->
        Log.info (fun f -> f ~tags "Connection closed");
        Prometheus.Gauge.dec_one Metrics.connections;
        disconnect t (Capnp_rpc.Exception.v ~ty:`Disconnected "Connection closed")
      );
    t

  let dump f t = Conn.dump f t.conn
end
