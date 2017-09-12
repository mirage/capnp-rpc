module RO_array = Capnp_rpc.RO_array

let next = ref 0

type 'a target =
  | Id of int * 'a
  | See of 'a base_ref
and 'a base_ref = {
  mutable target : 'a target;
}

type cap = unit base_ref

type struct_data = {
  caps : (int, cap) Hashtbl.t;
  mutable cancelled : bool;
  on_cancel : (unit -> unit) Queue.t;
}

type struct_ref = struct_data base_ref

let make_ref x =
  let id = !next in
  incr next;
  { target = Id (id, x) }

let make_cap () = make_ref ()

let null = make_cap ()

let make_struct () =
  make_ref {
    caps = Hashtbl.create 3;
    cancelled = false;
    on_cancel = Queue.create ();
  }

let cancelled = make_struct ()

let rec target x =
  match x.target with
  | Id (i, v) -> (i, v)
  | See y -> target y

let target_id x = fst (target x)
let target_data x = snd (target x)

let compare_ref a b =
  compare (target_id a) (target_id b)

let compare_cap : cap -> cap -> int = compare_ref
let compare_sr : struct_ref -> struct_ref -> int = compare_ref

let rec pp f t =
  match t.target with
  | See t' -> pp f t'
  | Id (x, _) -> Fmt.pf f "%d" x
let pp f = Fmt.styled `Magenta (Fmt.styled `Bold pp) f

let pp_struct f s = Fmt.pf f "s-%a" pp s
let pp_struct = Fmt.styled `Blue (Fmt.styled `Bold pp_struct)

(* Convert [a] to be a redirect to [b], unless that would create a cycle. *)
let rec unify a b =
  match a.target with
  | See a' -> unify a' b
  | Id (0, _) when target_id b = 0 -> ()        (* null *)
  | Id (old, _) ->
    Logs.info (fun f -> f "Unify: %a is now an alias for %a" pp a pp b);
    let b_id = target_id b in
    if old <> b_id then (
      a.target <- See b;
    )
    (* else cycle *)

let equal a b =
  target_id a = target_id b

let cap s i =
  let data = target_data s in
  match Hashtbl.find data.caps i with
  | c -> c
  | exception Not_found ->
    let c = make_cap () in
    Logs.info (fun f -> f "Pipeline %a/%d -> %a" pp_struct s i pp c);
    Hashtbl.add data.caps i c;
    c

let return s caps =
  let s_data = target_data s in
  s_data.caps |> Hashtbl.iter (fun i c ->
      if i < RO_array.length caps then
        unify c (RO_array.get_exn caps i);
    );
  caps |> RO_array.iteri (Hashtbl.replace s_data.caps)

let is_cancelled s = (target_data s).cancelled

let mark_cancelled s =
  let s = target_data s in
  if not s.cancelled then (
    s.cancelled <- true;
    Queue.iter (fun f -> f ()) s.on_cancel;
    Queue.clear s.on_cancel
  )

let on_cancel s fn =
  let s = target_data s in
  Queue.add fn s.on_cancel

let return_tail s ~src =
  assert (not (is_cancelled src));
  let unify_cap i s_cap =
    let src_cap = cap src i in
    unify s_cap src_cap
  in
  let s_data = target_data s in
  Hashtbl.iter unify_cap s_data.caps;
  Queue.transfer s_data.on_cancel (target_data src).on_cancel;
  unify s src;
  if s_data.cancelled then mark_cancelled src
