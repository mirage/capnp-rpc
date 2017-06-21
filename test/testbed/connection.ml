module RO_array = Capnp_rpc.RO_array

let src = Logs.Src.create "test-net" ~doc:"Cap'n Proto RPC tests"
module Log = (val Logs.src_log src: Logs.LOG)

module Stats = Capnp_rpc.Stats
let stats = Alcotest.of_pp Stats.pp

module Endpoint (EP : Capnp_direct.ENDPOINT) = struct
  module Conn = Capnp_rpc.CapTP.Make(EP)

  type t = {
    conn : Conn.t;
    recv_queue : EP.In.t Queue.t;
  }

  let dump f t =
    Conn.dump f t.conn

  let create ?bootstrap ~tags xmit_queue recv_queue =
    let queue_send x = Queue.add x xmit_queue in
    let conn = Conn.create ?bootstrap ~tags ~queue_send in
    {
      conn;
      recv_queue;
    }

  let summary_of_msg = function
    | `Bootstrap _ -> "bootstrap"
    | `Call (_, _, msg, _) -> "call:" ^ msg
    | `Return (_, `Results (msg, _)) -> "return:" ^ msg
    | `Return (_, `Exception msg) -> "return:" ^ msg
    | `Return (_, `Cancelled) -> "return:(cancelled)"
    | `Return (_, _) -> "return:(other)"
    | `Finish _ -> "finish"
    | `Release _ -> "release"
    | `Disembargo_request _ -> "disembargo-request"
    | `Disembargo_reply _ -> "disembargo-reply"

  let pop_msg ?expect t =
    match Queue.pop t.recv_queue, expect with
    | msg, Some expected ->
      Alcotest.(check string) ("Input " ^ expected) expected (summary_of_msg msg);
      msg
    | msg, None -> msg
    | exception Queue.Empty -> Alcotest.fail "No messages found!"

  let handle_msg ?expect t =
    let msg = pop_msg ?expect t in
    Conn.handle_msg t.conn msg

  let maybe_handle_msg t =
    if Queue.length t.recv_queue > 0 then handle_msg t

  let step t =
    if Queue.length t.recv_queue > 0 then (maybe_handle_msg t; true)
    else false

  let bootstrap t = Conn.bootstrap t.conn
end

module Make ( ) = struct
  module ProtoC = Capnp_rpc.Message_types.Endpoint(Capnp_direct.Core_types) ( )
  module ProtoS = struct
    module Core_types = Capnp_direct.Core_types
    module Table = Capnp_rpc.Message_types.Flip(ProtoC.Table)
    module In = ProtoC.Out
    module Out = ProtoC.In
  end

  module C = Endpoint(ProtoC)
  module S = Endpoint(ProtoS)

  let create ~client_tags ~server_tags bootstrap =
    let q1 = Queue.create () in
    let q2 = Queue.create () in
    let c = C.create ~tags:client_tags q1 q2 in
    let s = S.create ~tags:server_tags q2 q1 ~bootstrap in
    c, s

  let rec flush c s =
    let c_changed = C.step c in
    let s_changed = S.step s in
    if c_changed || s_changed then flush c s

  let dump c s =
    Logs.info (fun f -> f ~tags:(C.Conn.tags c.C.conn) "%a" C.dump c);
    Logs.info (fun f -> f ~tags:(S.Conn.tags s.S.conn) "%a" S.dump s)

  let check_finished c s =
    try
      Alcotest.(check stats) "Client finished" Stats.zero @@ C.Conn.stats c.C.conn;
      Alcotest.(check stats) "Server finished" Stats.zero @@ S.Conn.stats s.S.conn
    with ex ->
      dump c s;
      raise ex
end
