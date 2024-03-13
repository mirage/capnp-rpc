open Asetmap
module RO_array = Capnp_rpc.RO_array
module Test_utils = Testbed.Test_utils
module OID = Capnp_rpc.Debug.OID
module IntSet = Set.Make(struct type t = int let compare = compare end)

let running_under_afl =
  match Array.to_list Sys.argv with
  | [] -> assert false
  | [_] -> false
  | [_; "--fuzz"] -> true
  | prog :: _ -> Capnp_rpc.Debug.failf "Usage: %s < input-data" prog

let test_script_path = "test_script.ml"

let stop_after =
  match Sys.getenv "FUZZ_STOP" with
  | s ->
    int_of_string s
  | exception Not_found -> -1
(* If the ref-counting seems off after a while, try setting this to a low-ish number.
   This will cause it to try to clean up early and hopefully discover the problem sooner. *)

let dump_state_at_each_step = not running_under_afl

let sanity_checks = not running_under_afl
(* Check that the state is valid after every step (slow). *)

let () =
  if running_under_afl then (
    Logs.set_level ~all:true (Some Logs.Error);
  ) else (
    Fmt.pr "vi: foldmethod=marker syntax=capnp-rpc@.";
    Printexc.record_backtrace true
  )

let code =
  if running_under_afl then (fun _ -> ())
  else (
    let script = open_out test_script_path in
    let code = Format.formatter_of_out_channel script in
    Fmt.pf code "[@@@@@@ocaml.warning \"-26-27\"]@.";
    Fmt.pf code "@[<v2>let test_ () =@,";
    at_exit (fun () ->
        Fmt.pf code "CS.check_finished c s";
        Fmt.pf code "@]@.";
        close_out script;
        Fmt.pr "Wrote %S@." test_script_path
      );
    fun f -> f code; Format.pp_print_cut code ()
  )

let step = ref 0

let failf msg = Fmt.kstr failwith msg

let styles = [| `Red; `Green; `Blue |]

let actor_for_id id =
  let style = styles.(id mod Array.length styles) in
  (style, Fmt.str "vat-%d" id)

let tags_for_id id =
  Logs.Tag.empty |> Logs.Tag.add Test_utils.actor_tag (actor_for_id id)

(* We want to check that messages sent over a reference arrive in order. *)
type cap_ref_counters = {
  mutable next_to_send : int;
  mutable next_expected : int;
  mutable cancelled : IntSet.t;              (* It's OK if these get skipped *)
}

let pp_counters f {next_to_send; next_expected; _} = Fmt.pf f "{send=%d; expect=%d}" next_to_send next_expected

module Msg = struct
  type 'a msg = {
    contents : 'a;
    attachments : Capnp_rpc.S.attachments;
  }

  let pp_msg pp_contents f {contents; attachments = _} = pp_contents f contents

  module Path = struct
    type t = int
    let compare = compare
    let pp = Fmt.int
    let root = 0
  end

  module Request = struct
    type contents = {
      sender : OID.t;
      sending_step : int;
      target : Direct.cap;
      seq : int;
      counters : cap_ref_counters;
      arg_ids : Direct.cap RO_array.t;
      answer : Direct.struct_ref;
    }

    type t = contents msg

    let pp_contents f {sender; sending_step; seq; counters; _} =
      Fmt.pf f "{call_id=%a:%d; cap_ref=%a; sending_step=%d}"
        OID.pp sender seq
        pp_counters counters
        sending_step

    let pp = pp_msg pp_contents

    let cap_index _ i = Some i

    let mark_cancelled {contents; _} =
      Direct.mark_cancelled contents.answer

    let with_attachments attachments x = {x with attachments}
    let attachments x = x.attachments

    let v ~target ~sender ~sending_step ~counters ~seq ~answer ~arg_ids =
      let contents = { target; sender; sending_step;
                       counters; seq; answer; arg_ids } in
      Direct.on_cancel answer (fun () ->
          if seq < counters.next_expected then ()       (* Already delivered *)
          else (
            counters.cancelled <- IntSet.add seq counters.cancelled;
          )
        );
      { contents; attachments = Capnp_rpc.S.No_attachments }
  end

  module Response = struct
    type contents = string
    type t = contents msg
    let pp = pp_msg Fmt.string
    let cap_index _ i = Some i
    let bootstrap () = {contents = "(boot)"; attachments = Capnp_rpc.S.No_attachments}
    let with_attachments attachments x = {x with attachments}
    let attachments x = x.attachments
    let v contents =
      { contents; attachments = Capnp_rpc.S.No_attachments }
  end

  type request = Request.contents
  type response = Response.contents

  let ref_leak_detected fn =
    fn ();
    failwith "ref_leak_detected"

  let summary = function
    | `Abort _ -> "abort"
    | `Bootstrap _ -> "bootstrap"
    | `Call (_, _, msg, _, _) -> Fmt.str "call:%a:%d" OID.pp msg.contents.Request.sender msg.contents.Request.seq
    | `Return (_, `Results (msg, _), _) -> "return:" ^ msg.contents
    | `Return (_, `Exception ex, _) -> "return:" ^ ex.Capnp_rpc.Exception.reason
    | `Return (_, `Cancelled, _) -> "return:(cancelled)"
    | `Return (_, `AcceptFromThirdParty, _) -> "return:accept"
    | `Return (_, `ResultsSentElsewhere, _) -> "return:sent-elsewhere"
    | `Return (_, `TakeFromOtherQuestion _, _) -> "return:take-from-other"
    | `Finish _ -> "finish"
    | `Release _ -> "release"
    | `Disembargo_request _ -> "disembargo-request"
    | `Disembargo_reply _ -> "disembargo-reply"
    | `Resolve _ -> "resolve"
    | `Unimplemented _ -> "unimplemented"

end

module Core_types = Capnp_rpc.Core_types(Msg)

module Network_types = struct
  type provision_id
  type recipient_id
  type third_party_cap_id
  type join_key_part
end

module Local_struct_promise = Capnp_rpc.Local_struct_promise.Make(Core_types)

module Table_types = struct
  module QuestionId = Capnp_rpc.Id.Make ( )
  module AnswerId = QuestionId
  module ImportId = Capnp_rpc.Id.Make ( )
  module ExportId = ImportId
end
module EP = Capnp_rpc.Message_types.Endpoint(Core_types)(Network_types)(Table_types)

module Endpoint = struct
  module Conn = Capnp_rpc.CapTP.Make(EP)

  type t = {
    local_id : int;
    remote_id : int;
    conn : Conn.t;
    recv_queue : EP.In.t Queue.t;
    dump : Format.formatter -> unit;    (* Dump the whole vat *)
  }

  let pp_conn_mod f t =
    match t.local_id, t.remote_id with
    | 0, 1 -> Fmt.string f "C"
    | 1, 0 -> Fmt.string f "S"
    | a, b -> Fmt.pf f "V%d_%d" a b

  let pp_conn_var f t =
    match t.local_id, t.remote_id with
    | 0, 1 -> Fmt.string f "c"
    | 1, 0 -> Fmt.string f "s"
    | a, b -> Fmt.pf f "v%d_%d" a b

  let dump f t =
    Conn.dump f t.conn

  let check t =
    Conn.check t.conn

  let fork f = f ()

  let create ~restore ~tags ~dump ~local_id ~remote_id xmit_queue recv_queue =
    let queue_send x = Queue.add x xmit_queue in
    let conn = Conn.create ~restore ~tags ~queue_send ~fork in
    {
      local_id;
      remote_id;
      conn;
      recv_queue;
      dump;
    }

  let handle_msg t =
    match Queue.pop t.recv_queue with
    | exception Queue.Empty -> Alcotest.fail "No messages found!"
    | msg ->
      let tags = EP.In.with_qid_tag (Conn.tags t.conn) msg in
      Logs.info (fun f -> f ~tags "<- %a" (EP.In.pp_recv Msg.Request.pp) msg);
      code (fun f ->
          let expect = Msg.summary msg in
          Fmt.pf f "%a.handle_msg %a ~expect:%S;"
            pp_conn_mod t
            pp_conn_var t
            expect
        );
      Conn.handle_msg t.conn msg

  let maybe_handle_msg t =
    if Queue.length t.recv_queue > 0 then handle_msg t

  let bootstrap t = Conn.bootstrap t.conn

  let try_step t =
    if Queue.length t.recv_queue > 0 then (
      incr step;
      if dump_state_at_each_step then
        Logs.info (fun f -> f ~tags:(Conn.tags t.conn) "@[<v>Flush step %d {{{@,%t}}}@]" !step t.dump);
      handle_msg t;
      if sanity_checks then Conn.check t.conn;
      true
    ) else false

  let disconnect t =
    Conn.disconnect t.conn (Capnp_rpc.Exception.v "Tests finished")
end

let () =
  Format.pp_set_margin Fmt.stdout 120;
  Fmt_tty.setup_std_outputs ()

type cap_ref = {
  cr_id : OID.t;
  cr_cap : Core_types.cap;
  cr_target : Direct.cap;
  cr_counters : cap_ref_counters;
}

let pp_resolver f id =
  Fmt.pf f "resolver_%a" OID.pp id

let pp_struct f id =
  Fmt.pf f "sr_%a" OID.pp id

let pp_var f cr =
  Fmt.pf f "cr_%a" OID.pp cr.cr_id

let make_cap_ref ~target cap =
  {
    cr_id = OID.next ();
    cr_cap = (cap :> Core_types.cap);
    cr_target = target;
    cr_counters = { next_to_send = 0; next_expected = 0; cancelled = IntSet.empty };
  }

class type virtual test_service = object
  inherit Core_types.service
  method pp_var : Format.formatter -> unit
end

let pp_error f base =
  try base#check_invariants
  with ex ->
    Fmt.pf f "@,[%a] %a"
      Fmt.(styled `Red string) "ERROR"
      Capnp_rpc.Debug.pp_exn ex

module Struct_info = struct
  type t = {
    sr : Core_types.struct_ref;
    direct : Direct.struct_ref;
    var : OID.t;
    call : Msg.Request.t;
  }

  let dump f t =
    Fmt.pf f "%a : @[%t;%a@]"
      Direct.pp_struct t.direct
      t.sr#pp
      pp_error t.sr

  let compare a b =
    Direct.compare_sr a.direct b.direct

  let check_invariants t =
    t.sr#check_invariants
end

module Vat = struct
  type t = {
    id : int;
    mutable bootstrap : (test_service * Direct.cap) option;
    caps : cap_ref WrapArray.t;
    structs : Struct_info.t WrapArray.t;
    actions : (unit -> unit) DynArray.t;
    mutable connections : (int * Endpoint.t) list;
    answers_needed : (Core_types.struct_resolver * Direct.struct_ref * OID.t) WrapArray.t;
  }

  let tags t = tags_for_id t.id

  let dump_cap f {cr_id; cr_target; cr_cap; cr_counters} =
    Fmt.pf f "%a -> %a : @[%t %a;%a@]"
      OID.pp cr_id
      Direct.pp cr_target
      cr_cap#pp
      pp_counters cr_counters
      pp_error cr_cap

  let dump_an f (sr, target, _) =
    Fmt.pf f "%a : @[%t@]"
      Direct.pp_struct target
      sr#pp

  let compare_cr a b =
    Direct.compare_cap a.cr_target b.cr_target

  let compare_an (_, a, _) (_, b, _) =
    Direct.compare_sr a b

  let pp f t =
    let pp_connection f (id, endpoint) =
      Fmt.pf f "@[<v2>Connection to %d@,%a\
                @]" id
        Endpoint.dump endpoint
      ;
    in
    Fmt.pf f "@[<v2>Connections:@,%a@]@,\
              @[<v2>Caps:@,%a@]@,\
              @[<v2>Structs:@,%a@]@,\
              @[<v2>Answers waiting:@,%a@]@,\
              @]"
        (Fmt.Dump.list pp_connection) t.connections
        (WrapArray.dump ~compare:compare_cr dump_cap) t.caps
        (WrapArray.dump ~compare:Struct_info.compare Struct_info.dump) t.structs
        (WrapArray.dump ~compare:compare_an dump_an) t.answers_needed

  let check t =
    try
      t.connections |> List.iter (fun (_, conn) -> Endpoint.check conn);
      t.caps |> WrapArray.iter (fun c -> c.cr_cap#check_invariants);
      t.structs |> WrapArray.iter Struct_info.check_invariants
    with ex ->
      Logs.err (fun f -> f ~tags:(tags t) "Invariants check failed: %a" Capnp_rpc.Debug.pp_exn ex);
      raise ex

  let do_action state =
    match DynArray.pick state.actions with
    | Some fn -> fn ()
    | None -> assert false        (* There should always be some actions *)

  let n_caps state n =
    let rec caps = function
      | 0 -> []
      | i ->
        match WrapArray.pick state.caps with
        | Some c -> c :: caps (i - 1)
        | None -> []
    in
    let cap_refs = caps n in
    let args = RO_array.of_list @@ List.map (fun cr -> cr.cr_cap) cap_refs in
    args, cap_refs

  (* Call a random cap, passing random arguments. *)
  let do_call state () =
    match WrapArray.pick state.caps with
    | None -> ()
    | Some cap_ref ->
      let cap = cap_ref.cr_cap in
      let counters = cap_ref.cr_counters in
      let target = cap_ref.cr_target in
      let n_args = Choose.int 3 in
      let args, arg_refs = n_caps state (n_args) in
      let arg_ids = List.map (fun cr -> cr.cr_target) arg_refs |> RO_array.of_list in
      RO_array.iter Core_types.inc_ref args;
      let answer = Direct.make_struct () in
      let sender = cap_ref.cr_id in
      Logs.info (fun f -> f ~tags:(tags state) "Call %a=%t(%a) (call_id=%a:%d answer=%a)"
                    Direct.pp target cap#pp
                    (RO_array.pp Core_types.pp) args
                    OID.pp sender counters.next_to_send
                    Direct.pp_struct answer);
      let msg =
        Msg.Request.v
          ~target ~sender ~sending_step:!step ~counters ~seq:counters.next_to_send
          ~answer ~arg_ids
        |> Core_types.Request_payload.with_caps args
      in
      let answer_var = OID.next () in
      let results, resolver = Local_struct_promise.make () in
      WrapArray.add state.structs {Struct_info.sr = results; direct = answer; var = answer_var; call = msg};
      code (fun f -> Fmt.pf f "let %a = call %a \"%a:%d\" %a in"
               pp_struct answer_var
               pp_var cap_ref
               OID.pp sender counters.next_to_send
               (Fmt.Dump.list pp_var) arg_refs
           );
      counters.next_to_send <- succ counters.next_to_send;
      cap#call resolver msg

  let answer_type state =
    let x = Choose.int 5 in
    if x = 4 then (
      let i = WrapArray.choose_i state.structs in
      match WrapArray.get state.structs i with
      | None -> `Return_error
      | Some s -> `Return_struct (lazy (WrapArray.remove state.structs i; s))
    ) else if x = 3 then `Return_error
    else (
      let n_args = x in
      let args, arg_refs = n_caps state (n_args) in
      `Return_results (args, arg_refs)
    )

  (* Reply to a random question. *)
  let do_answer state () =
    (* Choose args before popping question, in case we run out of random data in the middle.
       In that case, we need to leave the question around so it can be cleaned up. *)
    let reply = answer_type state in
    match WrapArray.pop state.answers_needed with
    | None -> ()
    | Some (answer, answer_id, answer_var) ->
      match reply with
      | `Return_struct (lazy s) ->
        Logs.info (fun f -> f ~tags:(tags state)
                      "Return struct %a (%a)" Struct_info.dump s Direct.pp_struct answer_id);
        Direct.return_tail answer_id ~src:(s.Struct_info.direct);
        code (fun f ->
            Fmt.pf f "%a#resolve %a;"
              pp_resolver answer_var
              pp_struct s.Struct_info.var
          );
        answer#resolve s.Struct_info.sr
      | `Return_error ->
        Logs.info (fun f -> f ~tags:(tags state)
                      "Return error (%a)" Direct.pp_struct answer_id);
        Direct.return answer_id RO_array.empty;
        let msg = "(simulated-failure)" in
        code (fun f ->
            Fmt.pf f "resolve_exn %a (Capnp_rpc.Exception.v %S);"
              pp_resolver answer_var
              msg
          );
        Core_types.resolve_exn answer (Capnp_rpc.Exception.v msg)
      | `Return_results (args, arg_refs) ->
        let arg_ids = List.map (fun cr -> cr.cr_target) arg_refs in
        RO_array.iter Core_types.inc_ref args;
        Logs.info (fun f -> f ~tags:(tags state)
                      "Return %a (%a)" (RO_array.pp Core_types.pp) args Direct.pp_struct answer_id);
        Direct.return answer_id (RO_array.of_list arg_ids);
        code (fun f ->
            let pp_inc_var f x = Fmt.pf f "with_inc_ref %a" pp_var x in
            Fmt.pf f "resolve_ok %a %S %a;"
              pp_resolver answer_var
              "reply"
              (Fmt.Dump.list pp_inc_var) arg_refs
          );
        let reply = Msg.Response.v "reply" |> Core_types.Response_payload.with_caps args in
        Core_types.resolve_ok answer reply

  let test_service ~target:self_id vat =
    object (self : test_service)
      inherit Core_types.service as super

      val id = Capnp_rpc.Debug.OID.next ()

      method! pp f = Fmt.pf f "test-service(%a, %t) %a"
          Capnp_rpc.Debug.OID.pp id
          super#pp_refcount
          Direct.pp self_id

      method pp_var f = Fmt.pf f "service_%a" OID.pp id

      method call results msg =
        super#check_refcount;
        let {Msg.Request.target; sender; sending_step = _; counters; seq; arg_ids; answer} = msg.Msg.contents in
        if not (Direct.equal target self_id) then
          failf "Call received by %a, but expected target was %a (answer %a)"
            Direct.pp self_id
            Direct.pp target
            Direct.pp_struct answer;
        while counters.next_expected < seq && IntSet.mem counters.next_expected counters.cancelled do
          Logs.info (fun f -> f "(ignoring skipped cancelled call %d)" counters.next_expected);
          counters.cancelled <- IntSet.remove counters.next_expected counters.cancelled;
          counters.next_expected <- counters.next_expected + 1
        done;
        let expected_seq = counters.next_expected in
        let expected_msg = Fmt.str "%a:%d" OID.pp sender counters.next_expected in
        counters.next_expected <- succ counters.next_expected;
        let answer_var = OID.next () in
        begin
          let caps = Core_types.Request_payload.snapshot_caps msg in
          match RO_array.length caps with
          | 0 ->
            code (fun f -> Fmt.pf f "let %a = %t#pop0 %S in"
                     pp_resolver
                     answer_var self#pp_var
                     expected_msg
                 );
          | _ ->
            code (fun f -> Fmt.pf f "let args, %a = %t#pop_n %S in"
                     pp_resolver
                     answer_var self#pp_var
                     expected_msg
                 );
            caps |> RO_array.iteri (fun i c ->
                let target = RO_array.get_exn arg_ids i in
                let cr = make_cap_ref ~target c in
                code (fun f -> Fmt.pf f "let %a = RO_array.get_exn args %d in" pp_var cr i);
                WrapArray.add vat.caps cr
              )
        end;
        if seq <> expected_seq then (
          failf "Expecting message call_id=%a:%d, but got %d (target %a)"
            OID.pp sender expected_seq
            seq Direct.pp target;
        );
        WrapArray.add vat.answers_needed (results, answer, answer_var)
    end

  (* Pick a random cap from an answer. *)
  let do_struct state () =
    match WrapArray.pick state.structs with
    | None -> ()
    | Some {Struct_info.sr; direct; var; call = _} ->
      let i = Choose.int 3 in
      Logs.info (fun f -> f ~tags:(tags state) "Get %t/%d" sr#pp i);
      let cap = sr#cap i in
      let target = Direct.cap direct i in
      let cr = make_cap_ref ~target cap in
      code (fun f -> Fmt.pf f "let %a = %a#cap %d in" pp_var cr pp_struct var i);
      WrapArray.add state.caps cr

  (* Release/cancel a question *)
  let do_finish state () =
    match WrapArray.pop state.structs with
    | None -> ()
    | Some {Struct_info.sr; var; call; direct = _} ->
      begin match sr#response with
        | None ->
          Logs.info (fun f -> f ~tags:(tags state) "Cancel %t" sr#pp);
          Msg.Request.mark_cancelled call
        | Some _ ->
          Logs.info (fun f -> f ~tags:(tags state) "Finish %t" sr#pp);
      end;
      code (fun f -> Fmt.pf f "dec_ref %a;" pp_struct var);
      Core_types.dec_ref sr

  let do_release state () =
    match WrapArray.pop state.caps with
    | None -> ()
    | Some cr ->
      let c = cr.cr_cap in
      Logs.info (fun f -> f ~tags:(tags state) "Release %t (%a)" c#pp Direct.pp cr.cr_target);
      code (fun f -> Fmt.pf f "dec_ref %a;" pp_var cr);
      Core_types.dec_ref c

  (* Create a new local service *)
  let do_create state () =
    let target = Direct.make_cap () in
    let ts = test_service ~target state in
    Logs.info (fun f -> f ~tags:(tags state) "Created %t" ts#pp);
    let cr = make_cap_ref ~target ts in
    code (fun f -> Fmt.pf f "let %t = Services.manual () in" ts#pp_var);
    code (fun f -> Fmt.pf f "let %a = %t in" pp_var cr ts#pp_var);
    WrapArray.add state.caps cr

  let add_actions v conn ~target =
    DynArray.add v.actions (fun () ->
        Logs.info (fun f -> f ~tags:(tags v) "Expecting bootstrap reply to be target %a" Direct.pp target);
        let cr = make_cap_ref ~target @@ Endpoint.bootstrap conn "" in
        code (fun f -> Fmt.pf f "let %a = %a.bootstrap %a in"
                 pp_var cr
                 Endpoint.pp_conn_mod conn
                 Endpoint.pp_conn_var conn);
        WrapArray.add v.caps cr
      );
    DynArray.add v.actions (fun () ->
        Endpoint.maybe_handle_msg conn
      )

  (* TODO: return a random object or error at some future time *)
  let restore t k object_id =
    match object_id, t.bootstrap with
    | "", Some (cap, _) -> Core_types.inc_ref cap; k @@ Ok (cap :> Core_types.cap)
    | _ -> k @@ Error (Capnp_rpc.Exception.v "Bad object_id for restore")

  let free_all t =
    WrapArray.free t.caps;
    WrapArray.free t.structs;
    WrapArray.free t.answers_needed

  let next_id = ref 0

  let create () =
    let id = !next_id in
    next_id := succ !next_id;
    let free_cap cr =
      Logs.info (fun f -> f "Freeing replaced cap %a" OID.pp cr.cr_id);
      code (fun f -> Fmt.pf f "dec_ref %a;" pp_var cr);
      Core_types.dec_ref cr.cr_cap
    in
    let free_struct { Struct_info.sr; direct = _; var; call } =
      Logs.info (fun f -> f "Freeing replaced struct %a" OID.pp var);
      Msg.Request.mark_cancelled call;
      code (fun f -> Fmt.pf f "dec_ref %a;" pp_struct var);
      Core_types.dec_ref sr
    in
    let free_answer (ans, _, answer_var) =
      code (fun f ->
          Fmt.pf f "Core_types.resolve_exn %a (Capnp_rpc.Exception.v \"Operation rejected\");" pp_resolver answer_var
        );
      Core_types.resolve_exn ans @@ Capnp_rpc.Exception.v "Operation rejected"
    in
    let t = {
      id;
      bootstrap = None;
      caps = WrapArray.create ~free:free_cap 10;
      structs = WrapArray.create ~free:free_struct 10;
      actions = DynArray.create ignore;
      connections = [];
      answers_needed = WrapArray.create ~free:free_answer 10;
    } in
    let bs_id = Direct.make_cap () in
    let ts = test_service ~target:bs_id t in
    code (fun f -> Fmt.pf f "let %t = Services.manual () in (* Bootstrap for vat %d *)" ts#pp_var id);
    t.bootstrap <- Some (ts, bs_id);
    DynArray.add t.actions (do_call t);
    DynArray.add t.actions (do_struct t);
    DynArray.add t.actions (do_finish t);
    DynArray.add t.actions (do_create t);
    DynArray.add t.actions (do_release t);
    DynArray.add t.actions (do_answer t);
    t

  let try_step t =
    List.fold_left (fun found (_, c) ->
        Endpoint.try_step c || found
      ) false t.connections

  let destroy t =
    begin match t.bootstrap with
    | None -> ()
    | Some (bs, _) -> Core_types.dec_ref bs; t.bootstrap <- None
    end;
    List.iter (fun (_, e) -> Endpoint.disconnect e) t.connections;
    t.connections <- []
end

let make_connection v1 v2 =
  let q1 = Queue.create () in
  let q2 = Queue.create () in
  let bootstrap x =
    match x.Vat.bootstrap with
    | None -> assert false
    | Some (c, _) -> c
  in
  let target = function
    | None -> Direct.null
    | Some (_, id) -> id
  in
  let create_endpoint ~local ~remote xmit_queue recv_queue =
    let v1_tags = Vat.tags local |> Logs.Tag.add Test_utils.peer_tag (actor_for_id remote.Vat.id) in
    Endpoint.create
      ~dump:(fun f -> Vat.pp f local)
      ~local_id:local.id
      ~remote_id:remote.id
      ~tags:v1_tags
      xmit_queue recv_queue
      ~restore:(Vat.restore local)
  in
  let c = create_endpoint ~local:v1 ~remote:v2 q1 q2 in
  let s = create_endpoint ~local:v2 ~remote:v1 q2 q1 in
  code (fun f ->
      Fmt.pf f
        "@[<v2>let %a, %a = CS.create@,\
           ~client_tags:Test_utils.client_tags ~client_bs:%t@,\
           ~server_tags:Test_utils.server_tags %t@]@,\
         in"
        Endpoint.pp_conn_var c
        Endpoint.pp_conn_var s
        (bootstrap v1)#pp_var
        (bootstrap v2)#pp_var
    );
  let open Vat in
  add_actions v1 c ~target:(target v2.bootstrap);
  add_actions v2 s ~target:(target v1.bootstrap);
  v1.connections <- (v2.id, c) :: v1.connections;
  v2.connections <- (v1.id, s) :: v2.connections

let run_test () =
  OID.reset ();
  step := 0;

  let v1 = Vat.create () in
  let v2 = Vat.create () in

  make_connection v1 v2;

  let vats =
    let three_vats = Choose.bool () in
    if three_vats then (
      Logs.info (fun f -> f "Testing with three vats");
      let v3 = Vat.create () in
      make_connection v1 v3;
      [| v1; v2; v3 |]
    ) else (
      Logs.info (fun f -> f "Testing with two vats");
      [| v1; v2 |]
    )
  in

  let free_all () =
    Logs.info (fun f -> f "Freeing everything (for debugging)");
    let rec flush () =
      let progress = Array.fold_left (fun found v ->
          Vat.try_step v || found
        ) false vats
      in
      Array.iter Vat.check vats;
      if progress then flush ()
    in
    flush ();   (* Deliver any pending calls - may add caps *)
    vats |> Array.iter (fun v ->
        Vat.free_all v;
      );
    flush ();
    vats |> Array.iter (fun v ->
        Logs.info (fun f -> f ~tags:(Vat.tags v) "{{{%a}}}" Vat.pp v);
      );
    if stop_after >= 0 then failwith "Everything freed!"
  in

  try
    let rec loop () =
      let v = Choose.array vats in
      if dump_state_at_each_step then
        Logs.info (fun f -> f ~tags:(Vat.tags v) "Pre step %d: {{{%a}}}" !step Vat.pp v);
      Vat.do_action v;
      if dump_state_at_each_step then
        Logs.info (fun f -> f ~tags:(Vat.tags v) "Post step %d: {{{%a}}}}" !step Vat.pp v);
      if sanity_checks then (Gc.full_major (); Vat.check v);
      if !step <> stop_after then (
        incr step;
        loop ()
      ) else Logs.info (fun f -> f "Stopping early due to stop_after");
    in
    begin
      try loop ()
      with Choose.End_of_fuzz_data -> Logs.info (fun f -> f "End of fuzz data")
    end;
    free_all ();
    Array.iter Vat.destroy vats
  with ex ->
    let bt = Printexc.get_raw_backtrace () in
    Logs.err (fun f -> f "{{{%a}}}" Fmt.exn_backtrace (ex, bt));
    Logs.err (fun f -> f "Got error (at step %d) - dumping state:" !step);
    vats |> Array.iter (fun v ->
        Logs.info (fun f -> f ~tags:(Vat.tags v) "{{{%a}}}" Vat.pp v);
      );
    raise ex

let () =
  (* Logs.set_level (Some Logs.Error); *)
  AflPersistent.run @@ fun () ->
  run_test ();
  Gc.full_major ()
