open Capnp_direct.Core_types

module RO_array = Capnp_rpc.RO_array

let echo_service = object (self : cap)
  inherit ref_counted
  method call x caps =
    assert (ref_count > 0);
    return ("got:" ^ x, caps)
  method private release = ()
  method pp f = Fmt.string f "echo-service"
  method shortest = (self :> cap)
end

(* A service which just queues incoming messages and lets the test driver handle them. *)
let manual () = object (self : #cap)
  inherit ref_counted
  val queue = Queue.create ()

  method call x caps =
    assert (ref_count > 0);
    let result = Capnp_direct.Local_struct_promise.make () in
    Queue.add (x, caps, result) queue;
    (result :> struct_ref)

  method pop = Queue.pop queue

  method private release = ()
  method pp f = Fmt.string f "manual"
  method shortest = (self :> cap)
end

(* Callers can swap their arguments for the slot's contents. *)
let swap_service slot = object (self : cap)
  inherit ref_counted

  method call x caps =
    assert (ref_count > 0);
    let old_msg, old_caps = !slot in
    slot := (x, caps);
    return (old_msg, old_caps)

  method private release = ()

  method pp f = Fmt.string f "swap-service"

  method shortest = (self :> cap)
end

let logger () = object (self : #cap)
  inherit ref_counted

  val log = Queue.create ()

  method call x caps =
    assert (ref_count > 0);
    assert (RO_array.length caps = 0);
    Queue.add x log;
    return ("logged", RO_array.empty)

  method private release = ()

  method pp f = Fmt.string f "logger-service"

  method pop =
    try Queue.pop log
    with Queue.Empty -> "(queue empty)"

  method shortest = (self :> cap)
end
