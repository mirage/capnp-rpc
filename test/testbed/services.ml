open Capnp_direct.Core_types

module RO_array = Capnp_rpc.RO_array

let echo_service () = object
  inherit service
  method call x caps =
    assert (ref_count > 0);
    return ("got:" ^ x, caps)
  method! pp f = Fmt.string f "echo-service"
end

(* A service which just queues incoming messages and lets the test driver handle them. *)
let manual () = object
  inherit service
  val queue = Queue.create ()

  method call x caps =
    assert (ref_count > 0);
    let result = Capnp_direct.Local_struct_promise.make () in
    Queue.add (x, caps, result) queue;
    (result :> struct_ref)

  method pop = Queue.pop queue  (* Caller takes ownership of caps *)

  method! pp f = Fmt.string f "manual"
end

(* Callers can swap their arguments for the slot's contents. *)
let swap_service slot = object
  inherit service

  method call x caps =
    assert (ref_count > 0);
    let old_msg, old_caps = !slot in
    slot := (x, caps);
    return (old_msg, old_caps)

  method! pp f = Fmt.string f "swap-service"
end

let logger () = object
  inherit service

  val log = Queue.create ()

  method call x caps =
    assert (ref_count > 0);
    assert (RO_array.length caps = 0);
    Queue.add x log;
    return ("logged", RO_array.empty)

  method! pp f = Fmt.string f "logger-service"

  method pop =
    try Queue.pop log
    with Queue.Empty -> "(queue empty)"
end
