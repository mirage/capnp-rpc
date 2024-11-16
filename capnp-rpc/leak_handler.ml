module M = Map.Make(Int)

module Log = Capnp_rpc_proto.Debug.Log

(* A map from thread IDs to (n, q) pairs.
   [q] is a queue of callbacks waiting to be run in the thread
   and [n] is the number of loops consuming [q] (typically 1). *)
let handlers : (int * (unit -> unit) Eio.Stream.t) M.t Atomic.t = Atomic.make M.empty

(* [add_handler id] increments the counter for thread [id] and returns the queue.
   If there isn't one yet, it creates a new one. *)
let rec add_handler id =
  let old = Atomic.get handlers in
  let handler =
    match M.find_opt id old with
    | None -> (1, Eio.Stream.create max_int)
    | Some (n, q) -> (n + 1, q)
  in
  let next = M.add id handler old in
  if Atomic.compare_and_set handlers old next then snd handler
  else add_handler id

let rec remove_handler id =
  let old = Atomic.get handlers in
  let n, q = M.find id old in
  let next =
    if n > 1 then M.add id (n - 1, q) old
    else M.remove id old
  in
  if not (Atomic.compare_and_set handlers old next) then remove_handler id

let run () =
  let id = Thread.(id (self ())) in
  let q = add_handler id in
  try
    while true do
      let fn = Eio.Stream.take q in
      try
        fn ()
      with ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Fiber.check ();
        Log.warn (fun f -> f "Uncaught exception handling ref-leak: %a" Fmt.exn_backtrace (ex, bt))
    done
  with ex ->
    remove_handler id;
    raise ex

let ref_leak_detected thread fn =
  match M.find_opt thread (Atomic.get handlers) with
  | Some (_, q) -> Eio.Stream.add q fn
  | None ->
    Capnp_rpc_proto.Debug.Log.debug
      (fun f -> f "Leak detected, but no leak reporter is running so ignoring")
