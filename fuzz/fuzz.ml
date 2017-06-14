open Testbed.Capnp_direct

module CS = Testbed.Connection.Make ( )    (* A client-server pair *)
module RO_array = Capnp_rpc.RO_array
module Test_utils = Testbed.Test_utils

exception End_of_fuzz_data

let () =
  Fmt.set_style_renderer Fmt.stderr `Ansi_tty

let choose_int limit =
  assert (limit < 256);
  try
    let x = Char.code (input_char stdin) in
    x mod limit
  with End_of_file -> raise End_of_fuzz_data

let choose options =
  options.(choose_int (Array.length options)) ()

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

type state = {
  caps : Core_types.cap DynArray.t;
  structs : Core_types.struct_ref DynArray.t;
  actions : (unit -> unit) DynArray.t;
}

let do_action state () = DynArray.pick state.actions ()

let do_cap state () =
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

let do_struct state () =
  let s = DynArray.pick state.structs in
  let i = choose_int 3 in
  Logs.info (fun f -> f "Get %t/%d" s#pp i);
  DynArray.add state.caps (s#cap i)

let do_finish state () =
  let s = DynArray.pop state.structs in
  Logs.info (fun f -> f "Finish %t" s#pp);
  s#finish

let test_service = Testbed.Services.echo_service

let () =
  (* Logs.set_level (Some Logs.Error); *)
  assert (Array.length (Sys.argv) = 1);
  AflPersistent.run @@ fun () ->
  print_endline "New run!";
  let open CS in
  let c, s = create ~client_tags:Test_utils.client_tags ~server_tags:Test_utils.server_tags test_service in
  let state = {
    caps = DynArray.create Core_types.null;
    structs = DynArray.create (Core_types.broken `Cancelled);
    actions = DynArray.create ignore;
  } in
  DynArray.add state.actions (fun () ->
      Logs.info (fun f -> f "Bootstrap");
      DynArray.add state.caps (C.bootstrap c)
    );
  DynArray.add state.actions (fun () -> Logs.info (fun f -> f "Server handle"); S.maybe_handle_msg s);
  DynArray.add state.actions (fun () -> Logs.info (fun f -> f "Client handle"); C.maybe_handle_msg c);
  let actions = [|
      do_action state;
      do_cap state;
      do_struct state;
      do_finish state;
    |]
  in
  let rec loop () = choose actions; loop () in
  try loop ()
  with End_of_fuzz_data -> ()

(* TODO: lots more things to test here. *)
