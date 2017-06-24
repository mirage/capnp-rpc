module RO_array = Capnp_rpc.RO_array
module Test_utils = Testbed.Test_utils

let three_vats = false

(* Check that the state is valid after every step (slow). *)
let sanity_checks = true

let failf msg = Fmt.kstrf failwith msg

let styles = [| `Red; `Green; `Blue |]

let tags_for_id id =
  let style = styles.(id mod Array.length styles) in
  Logs.Tag.empty |> Logs.Tag.add Test_utils.actor_tag (style, Fmt.strf "vat-%d" id)

module Direct : sig
  (* For each capability and struct_ref in the real system, we make a corresponding
     "direct" object. While a real capability must be converted to an export
     and sent as an integer ID over the network, its direct equivalent is included
     directly in the message content (this is possible only because we're running
     everything in a single process for testing).
     When a call is received and dispatched to a service by the CapTP system, the
     test service checks that the direct target in the message body matches the
     service's target. This ensures that the message was delivered to its intended
     recipient. *)

  type cap
  type struct_ref

  val null : cap

  val make_cap : unit -> cap
  val equal : cap -> cap -> bool

  val cancelled : struct_ref

  val make_struct : unit -> struct_ref
  val cap : struct_ref -> int -> cap
  val return : struct_ref -> cap RO_array.t -> unit

  val pp : cap Fmt.t
  val pp_struct : struct_ref Fmt.t
end = struct
  let next = ref 0

  type target =
    | Id of int
    | See of cap
  and cap = {
    mutable target : target;
  }
  and struct_ref = {
    struct_id : int;
    caps : (int, cap) Hashtbl.t;
  }

  let make_cap () =
    let id = !next in
    incr next;
    { target = Id id }

  let null = make_cap ()

  let make_struct () =
    let struct_id = !next in
    incr next;
    {
      struct_id;
      caps = Hashtbl.create 3;
    }

  let cancelled = make_struct ()

  let rec target x =
    match x.target with
    | Id i -> i
    | See y -> target y

  let rec pp f t =
    match t.target with
    | See t' -> pp f t'
    | Id x -> Fmt.pf f "c-%d" x
  let pp = Fmt.styled `Magenta (Fmt.styled `Bold pp)

  let pp_struct f s = Fmt.pf f "s-%d" s.struct_id
  let pp_struct = Fmt.styled `Blue (Fmt.styled `Bold pp_struct)

  let rec unify a b =
    match a.target with
    | See a' -> unify a' b
    | Id 0 when target b = 0 -> ()        (* null *)
    | Id old ->
      Logs.info (fun f -> f "Unify: %a is now an alias for %a" pp a pp b);
      if old <> target b then
        a.target <- See b
      (* else cycle *)

  let equal a b =
    target a = target b

  let cap s i =
    match Hashtbl.find s.caps i with
    | c -> c
    | exception Not_found ->
      let c = make_cap () in
      Logs.info (fun f -> f "Pipeline %a/%d -> %a" pp_struct s i pp c);
      Hashtbl.add s.caps i c;
      c

  let return s caps =
    s.caps |> Hashtbl.iter (fun i c ->
        if i < RO_array.length caps then
          unify c (RO_array.get caps i);
      );
    caps |> RO_array.iteri (Hashtbl.replace s.caps)
end

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
      target : Direct.cap;
      seq : int;
      counters : cap_ref_counters;
      arg_ids : Direct.cap RO_array.t;
      answer : Direct.struct_ref;
    }
    let pp f {seq; counters; _} = Fmt.pf f "{seq=%d; cap_ref=%a}" seq pp_counters counters

    let cap_index _ i = Some i
  end

  module Response = struct
    type t = string
    let pp = Fmt.string
    let cap_index _ i = Some i
    let bootstrap = "(boot)"
  end
end

module Core_types = struct
  include Capnp_rpc.Core_types(Msg)
  type sturdy_ref
  type provision_id
  type recipient_id
  type third_party_cap_id
  type join_key_part
end

module Local_struct_promise = Capnp_rpc.Local_struct_promise.Make(Core_types)

module EP = struct
  module Core_types = Core_types

  module Table = struct
    module QuestionId = Capnp_rpc.Id.Make ( )
    module AnswerId = QuestionId
    module ImportId = Capnp_rpc.Id.Make ( )
    module ExportId = ImportId
  end

  module Out = Capnp_rpc.Message_types.Make(Core_types)(Table)
  module In = Capnp_rpc.Message_types.Make(Core_types)(Table)
end

module Endpoint = struct
  module Conn = Capnp_rpc.CapTP.Make(EP)

  type t = {
    conn : Conn.t;
    recv_queue : EP.In.t Queue.t;
  }

  let dump f t =
    Conn.dump f t.conn

  let check t =
    Conn.check t.conn

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
    if t.len = 0 then None
    else Some (t.items.(choose_int t.len))

  let pop t =
    if t.len = 0 then None
    else (
      let i = choose_int t.len in
      let v = t.items.(i) in
      t.len <- t.len - 1;
      t.items.(i) <- t.items.(t.len);
      Some v
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
  method check_invariants = ()
end

type cap_ref = {
  cr_cap : Core_types.cap;
  cr_target : Direct.cap;
  cr_counters : cap_ref_counters;
}

type vat = {
  id : int;
  mutable bootstrap : (Core_types.cap * Direct.cap) option;
  caps : cap_ref DynArray.t;
  structs : (Core_types.struct_ref * Direct.struct_ref) DynArray.t;
  actions : (unit -> unit) DynArray.t;
  mutable connections : (int * Endpoint.t) list;
  answers_needed : (Core_types.struct_resolver * Direct.struct_ref) DynArray.t;
}

let tags v = tags_for_id v.id

let pp_vat f t =
  let pp_connection f (id, endpoint) =
    Fmt.pf f "@[<v2>Connection to %d@,%a@]" id Endpoint.dump endpoint;
  in
  Fmt.Dump.list pp_connection f t.connections

let do_action state =
  match DynArray.pick state.actions with
  | Some fn -> fn ()
  | None -> assert false        (* There should always be some actions *)

let n_caps state n =
  let rec caps = function
    | 0 -> []
    | i ->
      match DynArray.pick state.caps with
      | Some c -> c :: caps (i - 1)
      | None -> []
  in
  let cap_refs = caps n in
  let args = RO_array.of_list @@ List.map (fun cr -> cr.cr_cap) cap_refs in
  args, cap_refs

(* Call a random cap, passing random arguments. *)
let do_call state () =
  match DynArray.pick state.caps with
  | None -> ()
  | Some cap_ref ->
    let cap = cap_ref.cr_cap in
    let counters = cap_ref.cr_counters in
    let target = cap_ref.cr_target in
    let n_args = choose_int 3 in
    let args, arg_refs = n_caps state (n_args) in
    let arg_ids = List.map (fun cr -> cr.cr_target) arg_refs |> RO_array.of_list in
    RO_array.iter (fun c -> c#inc_ref) args;
    let answer = Direct.make_struct () in
    Logs.info (fun f -> f ~tags:(tags state) "Call %a=%t(%a) (answer %a)"
                  Direct.pp target cap#pp
                  (RO_array.pp Core_types.pp) args
                  Direct.pp_struct answer);
    let msg = { Msg.Request.target; counters; seq = counters.next_to_send; answer; arg_ids } in
    counters.next_to_send <- succ counters.next_to_send;
    DynArray.add state.structs (cap#call msg args, answer)

(* Reply to a random question. *)
let do_answer state () =
  match DynArray.pop state.answers_needed with
  | None -> ()
  | Some (answer, answer_id) ->
    let n_args = choose_int 3 in
    let args, arg_refs = n_caps state (n_args) in
    let arg_ids = List.map (fun cr -> cr.cr_target) arg_refs in
    RO_array.iter (fun c -> c#inc_ref) args;
    Logs.info (fun f -> f ~tags:(tags state)
                  "Return %a (%a)" (RO_array.pp Core_types.pp) args Direct.pp_struct answer_id);
    Direct.return answer_id (RO_array.of_list arg_ids);
    answer#resolve (Ok ("reply", args))
    (* TODO: reply with another promise or with an error *)

let make_cap_ref ~target cap =
  {
    cr_cap = cap;
    cr_target = target;
    cr_counters = { next_to_send = 0; next_expected = 0 };
  }

let test_service ~target:self_id vat =
  object (_ : Core_types.cap)
    inherit Core_types.service

    method! pp f = Fmt.pf f "test-service(rc=%d) %a" ref_count Direct.pp self_id

    method call msg caps =
      assert (ref_count > 0);
      let {Msg.Request.target; counters; seq; arg_ids; answer} = msg in
      if not (Direct.equal target self_id) then
        failf "Call received by %a, but expected target was %a (answer %a)"
          Direct.pp self_id
          Direct.pp target
          Direct.pp_struct answer;
      if seq <> counters.next_expected then
        failf "Expecting message number %d, but got %d (target %a)" counters.next_expected seq Direct.pp target;
      counters.next_expected <- succ counters.next_expected;
      caps |> RO_array.iteri (fun i c ->
          let target = RO_array.get arg_ids i in
          DynArray.add vat.caps (make_cap_ref ~target c)
        );
      let answer_promise = Local_struct_promise.make () in
      DynArray.add vat.answers_needed (answer_promise, answer);
      (answer_promise :> Core_types.struct_ref)
  end

(* Pick a random cap from an answer. *)
let do_struct state () =
  match DynArray.pick state.structs with
  | None -> ()
  | Some (s, s_id) ->
    let i = choose_int 3 in
    Logs.info (fun f -> f ~tags:(tags state) "Get %t/%d" s#pp i);
    let cap = s#cap i in
    let target = Direct.cap s_id i in
    DynArray.add state.caps (make_cap_ref ~target cap)

(* Finish an answer *)
let do_finish state () =
  match DynArray.pop state.structs with
  | None -> ()
  | Some (s, _id) ->
    Logs.info (fun f -> f ~tags:(tags state) "Finish %t" s#pp);
    s#finish

let do_release state () =
  match DynArray.pop state.caps with
  | None -> ()
  | Some cr ->
    let c = cr.cr_cap in
    Logs.info (fun f -> f ~tags:(tags state) "Release %t" c#pp);
    c#dec_ref

(* Create a new local service *)
let do_create state () =
  let target = Direct.make_cap () in
  let ts = test_service ~target state in
  Logs.info (fun f -> f ~tags:(tags state) "Created %t" ts#pp);
  DynArray.add state.caps (make_cap_ref ~target ts)

let add_actions v conn ~target =
  DynArray.add v.actions (fun () ->
      Logs.info (fun f -> f ~tags:(tags v) "Expecting bootstrap reply to be target %a" Direct.pp target);
      DynArray.add v.caps (make_cap_ref ~target @@ Endpoint.bootstrap conn)
    );
  DynArray.add v.actions (fun () ->
      Logs.info (fun f -> f ~tags:(tags_for_id v.id) "Handle next message");
      Endpoint.maybe_handle_msg conn
    )

let make_connection v1 v2 =
  let q1 = Queue.create () in
  let q2 = Queue.create () in
  let v1_tags = tags_for_id v1.id in
  let v2_tags = tags_for_id v2.id in
  let cap = function
    | None -> None
    | Some (c, _) -> Some c
  in
  let target = function
    | None -> Direct.null
    | Some (_, id) -> id
  in
  let c = Endpoint.create ~tags:v1_tags q1 q2 ?bootstrap:(cap v1.bootstrap) in
  let s = Endpoint.create ~tags:v2_tags q2 q1 ?bootstrap:(cap v2.bootstrap) in
  add_actions v1 c ~target:(target v2.bootstrap);
  add_actions v2 s ~target:(target v1.bootstrap);
  v1.connections <- (v2.id, c) :: v1.connections;
  v2.connections <- (v1.id, s) :: v2.connections

let next_id = ref 0

let make_vat () =
  let id = !next_id in
  next_id := succ !next_id;
  let null = make_cap_ref ~target:Direct.null Core_types.null in
  let t = {
    id;
    bootstrap = None;
    caps = DynArray.create null;
    structs = DynArray.create (Core_types.broken_struct `Cancelled, Direct.cancelled);
    actions = DynArray.create ignore;
    connections = [];
    answers_needed = DynArray.create (dummy_answer, Direct.cancelled);
  } in
  let bs_id = Direct.make_cap () in
  t.bootstrap <- Some (test_service ~target:bs_id t, bs_id);
  DynArray.add t.actions (do_call t);
  DynArray.add t.actions (do_struct t);
  DynArray.add t.actions (do_finish t);
  DynArray.add t.actions (do_create t);
  DynArray.add t.actions (do_release t);
  DynArray.add t.actions (do_answer t);
  t

let () =
  (* Logs.set_level (Some Logs.Error); *)
  assert (Array.length (Sys.argv) = 1);
  AflPersistent.run @@ fun () ->

  let v1 = make_vat () in
  let v2 = make_vat () in

  make_connection v1 v2;

  let vats =
    if three_vats then (
      let v3 = make_vat () in
      make_connection v1 v3;
      [| v1; v2; v3 |]
    ) else (
      [| v1; v2 |]
    )
  in

  try
    let rec loop () =
      let v = choose vats in
      if sanity_checks then
        v.connections |> List.iter (fun (_, conn) -> Endpoint.check conn);
      do_action v;
      loop ()
    in
    try loop ()
    with End_of_fuzz_data -> () (* TODO: try releasing everything *)
  with ex ->
    let bt = Printexc.get_raw_backtrace () in
    Logs.err (fun f -> f "%a" Fmt.exn_backtrace (ex, bt));
    Logs.err (fun f -> f "Got error - dumping state:");
    vats |> Array.iter (fun v ->
        let tags = tags_for_id v.id in
        Logs.info (fun f -> f ~tags "%a" pp_vat v);
      );
    raise ex
