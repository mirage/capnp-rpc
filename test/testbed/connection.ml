open Capnp_direct.Core_types

module RO_array = Capnp_rpc.RO_array

let src = Logs.Src.create "test-net" ~doc:"Cap'n Proto RPC tests"
module Log = (val Logs.src_log src: Logs.LOG)

module Stats = Capnp_rpc.Stats
let stats = Alcotest.of_pp Stats.pp

module Endpoint
    (P : Capnp_direct.Protocol.S)
    (Other : Capnp_direct.Protocol.S with
      module T.QuestionId = P.T.AnswerId and
      module T.AnswerId = P.T.QuestionId and
      module T.ExportId = P.T.ImportId and
      module T.ImportId = P.T.ExportId
    )
= struct
  module Out = P.Out
  module In = Other.Out

  module Conn = Capnp_direct.CapTP.Make(P)

  type request = [ (* Waiting for test code to deal with it *)
    | `Bootstrap of P.answer
    | `Call of P.answer * cap * Request.t * cap RO_array.t
  ]

  type t = {
    conn : Conn.t;
    recv_queue : In.t Queue.t;
  }

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
  module Client_types = struct
    module QuestionId = Capnp_rpc.Id.Make ( )
    module AnswerId = Capnp_rpc.Id.Make ( )
    module ImportId = Capnp_rpc.Id.Make ( )
    module ExportId = Capnp_rpc.Id.Make ( )
  end

  module ProtoC = Capnp_direct.Protocol.Make(Client_types)
  module ProtoS = Capnp_direct.Protocol.Make(ProtoC.In)

  module C = Endpoint(ProtoC)(ProtoS)
  module S = Endpoint(ProtoS)(ProtoC)

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

  let check_finished c s =
    Alcotest.(check stats) "Client finished" Stats.zero @@ C.Conn.stats c.C.conn;
    Alcotest.(check stats) "Server finished" Stats.zero @@ S.Conn.stats s.S.conn
end
