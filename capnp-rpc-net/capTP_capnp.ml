open Eio.Std

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

  let return_not_implemented t x =
    Log.debug (fun f -> f ~tags:(tags t) "Returning Unimplemented");
    let open Builder in
    let m = Message.init_root () in
    let _ : Builder.Message.t = Message.unimplemented_set_reader m x in
    Endpoint.send t.endpoint (Message.to_message m)

  let rec listen t =
    match Endpoint.recv ~tags:(tags t) t.endpoint with
    | Error e -> e
    | Ok msg ->
      let open Reader.Message in
      let msg = of_message msg in
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
            Conn.disconnect t.conn (Capnp_rpc_proto.Exception.v "Received Abort from peer");
            `Aborted
          | _ ->
            Conn.handle_msg t.conn msg;
            listen t
        end
      | `Unimplemented x as msg ->
        Log.info (fun f ->
            let tags = Endpoint_types.Out.with_qid_tag (Conn.tags t.conn) x in
            f ~tags "<- Unimplemented(%a)" (Endpoint_types.Out.pp_recv pp_msg) x);
        Conn.handle_msg t.conn msg;
        listen t
      | `Not_implemented ->
        Log.info (fun f -> f "<- unsupported message type");
        return_not_implemented t msg;
        listen t

  let send_abort t ex =
    Endpoint.send t.endpoint (Serialise.message (`Abort ex));
    Endpoint.flush t.endpoint   (* We're probably about to disconnect *)

  let disconnect t ex =
    if not t.disconnecting then (
      t.disconnecting <- true;
      send_abort t ex;
      Endpoint.disconnect t.endpoint;
      Conn.disconnect t.conn ex
    )

  let disconnecting t = t.disconnecting

  let connect ~sw ~restore ?(tags=Logs.Tag.empty) endpoint =
    let queue_send msg = Endpoint.send endpoint (Serialise.message msg) in
    let restore = Restorer.fn restore in
    let fork = Fiber.fork ~sw in
    let conn = Conn.create ~restore ~tags ~fork ~queue_send in
    {
      conn;
      endpoint;
      disconnecting = false;
    }

  let listen t =
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
    Eio.Cancel.protect (fun () ->
        disconnect t (Capnp_rpc.Exception.v ~ty:`Disconnected "Connection closed")
      );
    Fiber.check ()

  let dump f t = Conn.dump f t.conn
end
