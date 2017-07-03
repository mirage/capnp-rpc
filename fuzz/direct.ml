module RO_array = Capnp_rpc.RO_array

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

let compare_cap a b =
  compare (target a) (target b)

let compare_sr a b =
  compare a.struct_id b.struct_id

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
