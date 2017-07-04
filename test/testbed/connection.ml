module RO_array = Capnp_rpc.RO_array

let src = Logs.Src.create "test-net" ~doc:"Cap'n Proto RPC tests"
module Log = (val Logs.src_log src: Logs.LOG)

module Stats = Capnp_rpc.Stats
let stats_t = Alcotest.of_pp Stats.pp

let summary_of_msg = function
  | `Bootstrap _ -> "bootstrap"
  | `Call (_, _, msg, _) -> "call:" ^ msg
  | `Return (_, `Results (msg, _), _) -> "return:" ^ msg
  | `Return (_, `Exception ex, _) -> "return:" ^ ex.Capnp_rpc.Exception.reason
  | `Return (_, `Cancelled, _) -> "return:(cancelled)"
  | `Return (_, `AcceptFromThirdParty, _) -> "return:accept"
  | `Return (_, `ResultsSentElsewhere, _) -> "return:sent-elsewhere"
  | `Return (_, `TakeFromOtherQuestion, _) -> "return:take-from-other"
  | `Finish _ -> "finish"
  | `Release _ -> "release"
  | `Disembargo_request _ -> "disembargo-request"
  | `Disembargo_reply _ -> "disembargo-reply"
  | `Resolve _ -> "resolve"
  | `Unimplemented _ -> "unimplemented"

module type ENDPOINT = sig
  open Capnp_direct.Core_types

  type t

  module EP : Capnp_direct.ENDPOINT

  val dump : t Fmt.t

  val create : ?bootstrap:#cap -> tags:Logs.Tag.set ->
    [EP.Out.t | `Unimplemented of EP.In.t] Queue.t ->
    [EP.In.t | `Unimplemented of EP.Out.t] Queue.t ->
    t

  val handle_msg : ?expect:string -> t -> unit

  val maybe_handle_msg : t -> unit

  val step : t -> bool

  val bootstrap : t -> cap

  val stats : t -> Capnp_rpc.Stats.t

  val check_finished : t -> name:string -> unit
end

module Endpoint (EP : Capnp_direct.ENDPOINT) = struct
  module Conn = Capnp_rpc.CapTP.Make(EP)

  type t = {
    conn : Conn.t;
    recv_queue : [EP.In.t | `Unimplemented of EP.Out.t] Queue.t;
  }

  let dump f t =
    Conn.dump f t.conn

  module EP = EP

  let create ?bootstrap ~tags xmit_queue recv_queue =
    let queue_send x = Queue.add (x :> [EP.Out.t | `Unimplemented of EP.In.t]) xmit_queue in
    let conn = Conn.create ?bootstrap ~tags ~queue_send in
    {
      conn;
      recv_queue;
    }

  let pop_msg ?expect t =
    match Queue.pop t.recv_queue, expect with
    | msg, Some expected ->
      Alcotest.(check string) ("Input " ^ expected) expected (summary_of_msg msg);
      msg
    | msg, None -> msg
    | exception Queue.Empty -> Alcotest.fail "No messages found!"

  let handle_msg ?expect t =
    pop_msg ?expect t |> Conn.handle_msg t.conn

  let maybe_handle_msg t =
    if Queue.length t.recv_queue > 0 then handle_msg t

  let step t =
    if Queue.length t.recv_queue > 0 then (maybe_handle_msg t; true)
    else false

  let bootstrap t = Conn.bootstrap t.conn

  let stats t = Conn.stats t.conn

  let finished = Capnp_rpc.Exception.v "Tests finished"

  let check_finished t ~name =
      Alcotest.(check stats_t) (name ^ " finished") Stats.zero @@ stats t;
      Conn.check t.conn;
      Conn.disconnect t.conn finished
end

module Pair ( ) = struct
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
    bootstrap#dec_ref; (* [s] took a reference *)
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
      C.check_finished c ~name:"Client";
      S.check_finished s ~name:"Server";
    with ex ->
      dump c s;
      raise ex
end
