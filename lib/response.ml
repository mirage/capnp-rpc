open Schema.Builder

type 'a cap = Rpc.value

type 'a t = {
  msg : Message.t;
  mutable n_exports : int;
  mutable exports_rev : Rpc.value list;
}

let create init =
  let msg = Message.init_root () in
  let ret = Message.return_init msg in
  let p = Return.results_init ret in
  let content = init (Payload.content_get p) in
  {msg; n_exports = 0; exports_rev = []}, content

let create_no_args () =
  let msg = Message.init_root () in
  let _ = Message.return_init msg in
  {msg; n_exports = 0; exports_rev = []}

let export t cap =
  let i = t.n_exports in
  t.n_exports <- i + 1;
  t.exports_rev <- cap :: t.exports_rev;
  Uint32.of_int i

let caps t =
  List.rev t.exports_rev |> Ro_array.of_list

let finish t =
  match Message.get t.msg with
  | Message.Return r -> Rpc.Builder r, caps t
  | _ -> assert false
