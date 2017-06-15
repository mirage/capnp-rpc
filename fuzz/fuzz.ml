open Testbed.Capnp_direct

module Client_types = struct
  module QuestionId = Capnp_rpc.Id.Make ( )
  module AnswerId = QuestionId
  module ImportId = Capnp_rpc.Id.Make ( )
  module ExportId = ImportId
end

module Proto = Protocol.Make(Client_types)
module Endpoint = Testbed.Connection.Endpoint(Proto)(Proto)

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

type vat = {
  id : int;
  bootstrap : Core_types.cap option;
  caps : Core_types.cap DynArray.t;
  structs : Core_types.struct_ref DynArray.t;
  actions : (unit -> unit) DynArray.t;
  mutable connections : (int * Endpoint.t) list;
}

let pp_vat f t =
  let pp_connection f (id, endpoint) =
    Fmt.pf f "@[<2>Connection to %d@,%a@]" id Endpoint.dump endpoint
  in
  Fmt.Dump.list pp_connection f t.connections

let do_action state () = DynArray.pick state.actions ()

(* Call a random cap, passing random arguments. *)
let do_call state () =
  let cap = DynArray.pick state.caps in
  let n_args = choose_int 3 in
  let rec caps = function
    | 0 -> []
    | i -> DynArray.pick state.caps :: caps (i - 1)
  in
  let args = RO_array.of_list (caps (n_args)) in
  RO_array.iter (fun c -> c#inc_ref) args;
  Logs.info (fun f -> f "Call %t(%a)" cap#pp (RO_array.pp Core_types.pp_cap) args);
  DynArray.add state.structs (cap#call "call" args)

(* Pick a random cap from an answer. *)
let do_struct state () =
  let s = DynArray.pick state.structs in
  let i = choose_int 3 in
  Logs.info (fun f -> f "Get %t/%d" s#pp i);
  DynArray.add state.caps (s#cap i)

(* Finish an answer *)
let do_finish state () =
  let s = DynArray.pop state.structs in
  Logs.info (fun f -> f "Finish %t" s#pp);
  s#finish

let do_release state () =
  let c = DynArray.pop state.caps in
  Logs.info (fun f -> f "Release %t" c#pp);
  c#dec_ref

let test_service = Testbed.Services.echo_service

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
  DynArray.add v1.actions (fun () -> DynArray.add v1.caps (Endpoint.bootstrap c));
  DynArray.add v2.actions (fun () -> DynArray.add v2.caps (Endpoint.bootstrap s));
  DynArray.add v1.actions (fun () -> Logs.info (fun f -> f ~tags:v1_tags "Handle"); Endpoint.maybe_handle_msg c);
  DynArray.add v2.actions (fun () -> Logs.info (fun f -> f ~tags:v2_tags "Handle"); Endpoint.maybe_handle_msg s);
  v1.connections <- (v2.id, c) :: v1.connections;
  v2.connections <- (v1.id, s) :: v2.connections

let next_id = ref 0

let make_vat ?bootstrap () =
  let id = !next_id in
  next_id := succ !next_id;
  let t = {
    id;
    bootstrap;
    caps = DynArray.create Core_types.null;
    structs = DynArray.create (Core_types.broken `Cancelled);
    actions = DynArray.create ignore;
    connections = [];
  } in
  DynArray.add t.actions (do_action t);
  DynArray.add t.actions (do_call t);
  DynArray.add t.actions (do_struct t);
  DynArray.add t.actions (do_finish t);
  DynArray.add t.actions (do_release t);
  t

let step v =
  DynArray.pick v.actions ()

let () =
  (* Logs.set_level (Some Logs.Error); *)
  assert (Array.length (Sys.argv) = 1);
  AflPersistent.run @@ fun () ->

  let v1 = make_vat () in
  let v2 = make_vat ~bootstrap:(test_service ()) () in
  let v3 = make_vat ~bootstrap:(test_service ()) () in

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
  with ex ->
    Logs.err (fun f -> f "Got error - dumping state:");
    vats |> Array.iter (fun v ->
        let tags = tags_for_id v.id in
        Logs.info (fun f -> f ~tags "%a" pp_vat v)
      );
    raise ex
