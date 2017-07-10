open Capnp_core
open Schema.Builder
module RO_array = Capnp_rpc.RO_array

type 'a cap = Core_types.cap

type 'a t = {
  msg : Message.t;
  mutable n_exports : int;
  mutable exports_rev : Core_types.cap list;
}

let create init =
  let msg = Message.init_root () in
  let ret = Message.return_init msg in
  let p = Return.results_init ret in
  let content = init (Payload.content_get p) in
  {msg; n_exports = 0; exports_rev = []}, content

let create_empty () =
  let msg = Message.init_root () in
  let _ = Message.return_init msg in
  {msg; n_exports = 0; exports_rev = []}

let export t cap =
  Core_types.inc_ref cap;
  let i = t.n_exports in
  t.n_exports <- i + 1;
  t.exports_rev <- cap :: t.exports_rev;
  Uint32.of_int i

let caps t =
  let caps = List.rev t.exports_rev |> RO_array.of_list in
  t.n_exports <- 0;
  t.exports_rev <- [];
  caps

let finish t =
  match Message.get t.msg with
  | Message.Return r -> Rpc.Builder r, caps t
  | _ -> assert false

let release t =
  List.iter Core_types.dec_ref t.exports_rev;
  t.n_exports <- 0;
  t.exports_rev <- []
