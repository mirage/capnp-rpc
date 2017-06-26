open Capnp_core
open Schema.Builder
module RO_array = Capnp_rpc.RO_array

type 'a t = {
  msg : Message.t;
  mutable n_exports : int;
  mutable exports_rev : Core_types.cap list;
}

let create init =
  let msg = Message.init_root () in
  let call = Message.call_init msg in
  let p = Call.params_get call in
  let content = init (Payload.content_get p) in
  {msg; n_exports = 0; exports_rev = []}, content

let create_no_args () =
  let msg = Message.init_root () in
  ignore (Message.call_init msg);
  {msg; n_exports = 0; exports_rev = []}

let export t cap =
  cap#inc_ref;
  let i = t.n_exports in
  t.n_exports <- i + 1;
  t.exports_rev <- cap :: t.exports_rev;
  Uint32.of_int i

let get_call t =
  match Message.get t.msg with
  | Message.Call c -> c
  | _ -> failwith "Not a call!"

let caps t =
  List.rev t.exports_rev |> RO_array.of_list
