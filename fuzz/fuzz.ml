open Testbed
open Crowbar

module CS = Connection.Make ( )    (* A client-server pair *)

let () =
  Fmt.set_style_renderer Fmt.stderr `Ansi_tty

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

    let pick t i =
      if t.len = 0 then t.default
      else t.items.((abs i) mod t.len)
end

let op = Choose [
    Map ([int], fun x -> `Do x);
    Map ([int; List int], fun x args -> `Cap (x, args));
  ]

type state = {
  caps : Core.cap DynArray.t;
  structs : Core.promise DynArray.t;
  actions : (unit -> unit) DynArray.t;
}

let perform state = function
  | `Do i -> (DynArray.pick state.actions i) ()
  | `Cap (i, args) ->
    let cap = DynArray.pick state.caps i in
    let args = List.map (DynArray.pick state.caps) args in
    List.iter (fun c -> c#inc_ref) args;
    Logs.info (fun f -> f "Call %t(%a)" cap#pp (Fmt.list ~sep:Fmt.(const string ", ") Core.pp_cap) args);
    DynArray.add state.structs (cap#call "call" args)

let test_service = Services.echo_service

let long_list x =
  Map ([List x; List x; List x], fun a b c -> a @ b @ c)

let () =
  (* Logs.set_level (Some Logs.Error); *)
  add_test ~name:"2-vat" [long_list op] @@ fun ops ->
  print_endline "New run!";
  let open CS in
  let c, s = create ~client_tags:Test_utils.client_tags ~server_tags:Test_utils.server_tags test_service in
  let state = {
    caps = DynArray.create Core.none;
    structs = DynArray.create (Core.broken `Cancelled);
    actions = DynArray.create ignore;
  } in
  DynArray.add state.actions (fun () ->
      Logs.info (fun f -> f "Bootstrap");
      DynArray.add state.caps ((C.bootstrap c)#cap 0)
    );
  DynArray.add state.actions (fun () -> Logs.info (fun f -> f "Server handle"); S.maybe_handle_msg s);
  DynArray.add state.actions (fun () -> Logs.info (fun f -> f "Client handle"); C.maybe_handle_msg c);
  DynArray.add state.actions (fun () -> Logs.info (fun f -> f "Server reply"); S.maybe_reply_to_call s);
  DynArray.add state.actions (fun () -> Logs.info (fun f -> f "Client reply"); C.maybe_reply_to_call c);
  List.iter (perform state) ops;
  Ok ()
