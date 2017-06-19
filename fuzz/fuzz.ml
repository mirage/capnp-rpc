(* We want to check that messages sent over a reference arrive in order. *)
type cap_ref_counters = {
  mutable next_to_send : int;
  mutable next_expected : int;
}

let pp_counters f {next_to_send; next_expected} = Fmt.pf f "{send=%d; expect=%d}" next_to_send next_expected

module Msg = struct
  module Path = struct
    type t = int
    let compare = compare
    let pp = Fmt.int
    let root = 0
  end

  module Request = struct
    type t = {
      seq : int;
      counters : cap_ref_counters;
    }
    let pp f {seq; counters} = Fmt.pf f "{seq=%d; cap_ref=%a}" seq pp_counters counters

    let cap_index _ i = Some i
  end

  module Response = struct
    type t = string
    let pp = Fmt.string
    let cap_index _ i = Some i
    let bootstrap = "(boot)"
  end
end

module Network_types = struct
  type sturdy_ref
  type provision_id
  type recipient_id
  type third_party_cap_id
  type join_key_part
end

module RPC = Capnp_rpc.Make(Msg)(Network_types)
module Core_types = RPC.Core_types

module Client_types = struct
  module QuestionId = Capnp_rpc.Id.Make ( )
  module AnswerId = QuestionId
  module ImportId = Capnp_rpc.Id.Make ( )
  module ExportId = ImportId
end

module Proto = RPC.Protocol.Make(Client_types)

module Endpoint = struct
  module RO_array = Capnp_rpc.RO_array

  module Conn = RPC.CapTP.Make(Proto)

  type t = {
    conn : Conn.t;
    recv_queue : Proto.In.t Queue.t;
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

  let handle_msg t =
    match Queue.pop t.recv_queue with
    | exception Queue.Empty -> Alcotest.fail "No messages found!"
    | msg -> Conn.handle_msg t.conn msg

  let maybe_handle_msg t =
    if Queue.length t.recv_queue > 0 then handle_msg t

  let bootstrap t = Conn.bootstrap t.conn
end

module RO_array = Capnp_rpc.RO_array
module Test_utils = Testbed.Test_utils

exception End_of_fuzz_data

let () =
  Format.pp_set_margin Fmt.stderr 120;
  Fmt.set_style_renderer Fmt.stderr `Ansi_tty

let choose_int limit =
  try
    let x = Char.code (input_char stdin) in
    if limit < 0x100 then x mod limit
    else (
      let y = Char.code (input_char stdin) in
      assert (limit < 0x10000);
      (x lor (y lsl 8)) mod limit
    )
  with End_of_file -> raise End_of_fuzz_data

let choose options =
  options.(choose_int (Array.length options))

module DynArray = struct
  type 'a t = {
    mutable items : 'a array;
    mutable len : int;
    default : 'a;
  }

  let create default = {
    items = Array.make 10 default;
    len = 0;
    default;
  }

  let add t v =
    if t.len = Array.length t.items then (
      t.items <- Array.init (t.len * 2) (fun i ->
          if i < t.len then t.items.(i)
          else t.default
        )
    );
    t.items.(t.len) <- v;
    t.len <- t.len + 1

  let pick t =
    if t.len = 0 then t.default
    else t.items.(choose_int t.len)

  let pop t =
    if t.len = 0 then t.default
    else (
      let i = choose_int t.len in
      let v = t.items.(i) in
      t.len <- t.len - 1;
      t.items.(i) <- t.items.(t.len);
      v
    )
end

let dummy_answer = object (self : Core_types.struct_resolver)
  method cap _ = failwith "dummy_answer"
  method connect x = x#finish
  method finish = failwith "dummy_answer"
  method pp f = Fmt.string f "dummy_answer"
  method resolve x = self#connect (Core_types.resolved x)
  method response = failwith "dummy_answer"
  method when_resolved _ = failwith "when_resolved"
  method blocker = None
end

type vat = {
  id : int;
  bootstrap : Core_types.cap option;
  caps : (Core_types.cap * cap_ref_counters) DynArray.t;
  structs : Core_types.struct_ref DynArray.t;
  actions : (unit -> unit) DynArray.t;
  mutable connections : (int * Endpoint.t) list;
  answers_needed : Core_types.struct_resolver DynArray.t;
}

let pp_vat f t =
  let pp_connection f (id, endpoint) =
    Fmt.pf f "@[<2>Connection to %d@,%a@]" id Endpoint.dump endpoint
  in
  Fmt.Dump.list pp_connection f t.connections

let do_action state () = DynArray.pick state.actions ()

let n_caps state n =
  let rec caps = function
    | 0 -> []
    | i -> DynArray.pick state.caps :: caps (i - 1)
  in
  let pairs = caps n in
  let args = RO_array.of_list @@ List.map fst pairs in
  let cap_refs = List.map snd pairs in
  args, cap_refs

(* Call a random cap, passing random arguments. *)
let do_call state () =
  let cap, counters = DynArray.pick state.caps in
  let n_args = choose_int 3 in
  let args, _arg_refs = n_caps state (n_args) in
  RO_array.iter (fun c -> c#inc_ref) args;
  Logs.info (fun f -> f "Call %t(%a)" cap#pp (RO_array.pp Core_types.pp_cap) args);
  let msg = { Msg.Request.counters; seq = counters.next_to_send } in
  counters.next_to_send <- succ counters.next_to_send;
  DynArray.add state.structs (cap#call msg args)

(* Reply to a random question. *)
let do_answer state () =
  let answer = DynArray.pop state.answers_needed in
  let n_args = choose_int 3 in
  let args, _arg_refs = n_caps state (n_args) in
  RO_array.iter (fun c -> c#inc_ref) args;
  Logs.info (fun f -> f "Return %a" (RO_array.pp Core_types.pp_cap) args);
  answer#resolve (Ok ("reply", args))
  (* TODO: reply with another promise or with an error *)

let make_cap_ref () =
  { next_to_send = 0; next_expected = 0 }

(* Pick a random cap from an answer. *)
let do_struct state () =
  let s = DynArray.pick state.structs in
  let i = choose_int 3 in
  Logs.info (fun f -> f "Get %t/%d" s#pp i);
  let cap = s#cap i in
  DynArray.add state.caps (cap, make_cap_ref ())

(* Finish an answer *)
let do_finish state () =
  let s = DynArray.pop state.structs in
  Logs.info (fun f -> f "Finish %t" s#pp);
  s#finish

let do_release state () =
  let c, _ = DynArray.pop state.caps in
  Logs.info (fun f -> f "Release %t" c#pp);
  c#dec_ref

let test_service vat =
  object (_ : Core_types.cap)
    inherit Core_types.service

    method! pp f = Fmt.string f "test-service"

    method call msg caps =
      let counters = msg.Msg.Request.counters in
      assert (msg.Msg.Request.seq = counters.next_expected);
      counters.next_expected <- succ counters.next_expected;
      caps |> RO_array.iter (fun c -> DynArray.add vat.caps (c, make_cap_ref ()));
      let answer = RPC.Local_struct_promise.make () in
      DynArray.add vat.answers_needed answer;
      (answer :> Core_types.struct_ref)
  end

let styles = [| `Red; `Green; `Blue |]

let tags_for_id id =
  let style = styles.(id mod Array.length styles) in
  Logs.Tag.empty |> Logs.Tag.add Test_utils.actor_tag (style, Fmt.strf "vat-%d" id)

let make_connection v1 v2 =
  let q1 = Queue.create () in
  let q2 = Queue.create () in
  let v1_tags = tags_for_id v1.id in
  let v2_tags = tags_for_id v2.id in
  let c = Endpoint.create ~tags:v1_tags q1 q2 ?bootstrap:v1.bootstrap in
  let s = Endpoint.create ~tags:v2_tags q2 q1 ?bootstrap:v2.bootstrap in
  DynArray.add v1.actions (fun () -> DynArray.add v1.caps (Endpoint.bootstrap c, make_cap_ref ()));
  DynArray.add v2.actions (fun () -> DynArray.add v2.caps (Endpoint.bootstrap s, make_cap_ref ()));
  DynArray.add v1.actions (fun () -> Logs.info (fun f -> f ~tags:v1_tags "Handle"); Endpoint.maybe_handle_msg c);
  DynArray.add v2.actions (fun () -> Logs.info (fun f -> f ~tags:v2_tags "Handle"); Endpoint.maybe_handle_msg s);
  v1.connections <- (v2.id, c) :: v1.connections;
  v2.connections <- (v1.id, s) :: v2.connections

let next_id = ref 0

let make_vat () =
  let id = !next_id in
  next_id := succ !next_id;
  let null = Core_types.null, make_cap_ref () in
  let t = {
    id;
    bootstrap = None;
    caps = DynArray.create null;
    structs = DynArray.create (Core_types.broken_struct `Cancelled);
    actions = DynArray.create ignore;
    connections = [];
    answers_needed = DynArray.create dummy_answer;
  } in
  let t = {t with bootstrap = Some (test_service t)} in
  DynArray.add t.actions (do_action t);
  DynArray.add t.actions (do_call t);
  DynArray.add t.actions (do_struct t);
  DynArray.add t.actions (do_finish t);
  DynArray.add t.actions (do_release t);
  DynArray.add t.actions (do_answer t);
  t

let step v =
  DynArray.pick v.actions ()

let () =
  (* Logs.set_level (Some Logs.Error); *)
  assert (Array.length (Sys.argv) = 1);
  AflPersistent.run @@ fun () ->

  let v1 = make_vat () in
  let v2 = make_vat () in
  let v3 = make_vat () in

  let vats = [| v1; v2; v3|] in

  make_connection v1 v2;
  make_connection v1 v3;

  try
    let rec loop () =
      let v = choose vats in
      step v;
      loop ()
    in
    try loop ()
    with End_of_fuzz_data -> () (* TODO: try releasing everything *)
  with Exit as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Logs.err (fun f -> f "%a" Fmt.exn_backtrace (ex, bt));
    Logs.err (fun f -> f "Got error - dumping state:");
    vats |> Array.iter (fun v ->
        let tags = tags_for_id v.id in
        Logs.info (fun f -> f ~tags "%a" pp_vat v)
      );
    raise ex
