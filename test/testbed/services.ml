open Capnp_direct.Core_types

module RO_array = Capnp_rpc.RO_array

class virtual test_service = object
  inherit service as super

  val mutable released = false
  val virtual name : string
  val id = Capnp_rpc.Debug.OID.next ()

  method released = released
  method! release = assert (not released); released <- true;

  method! pp f =
    Fmt.pf f "%s(%a, %t)"
      name
      Capnp_rpc.Debug.OID.pp id
      super#pp_refcount
end

let echo_service () = object
  inherit test_service as super
  val name = "echo-service"
  method call x caps =
    super#check_refcount;
    return ("got:" ^ x, caps)
end

(* A service which just queues incoming messages and lets the test driver handle them. *)
let manual () = object
  inherit test_service as super
  val queue = Queue.create ()
  val name = "manual"

  method call x caps =
    super#check_refcount;
    let result = Capnp_direct.Local_struct_promise.make () in
    Queue.add (x, caps, result) queue;
    (result :> struct_ref)

  method pop = Queue.pop queue  (* Caller takes ownership of caps *)

  (* Expect a message with no caps *)
  method pop0 msg =
    match Queue.pop queue with
    | exception Queue.Empty -> Capnp_rpc.Debug.failf "Empty queue (expecting %S)" msg
    | actual, args, answer ->
      Alcotest.(check string) ("Expecting " ^ msg) msg actual;
      Alcotest.(check int) "Has no args" 0 @@ RO_array.length args;
      answer

  (* Expect a message with one cap *)
  method pop1 msg =
    let actual, args, answer = Queue.pop queue in
    Alcotest.(check string) ("Expecting " ^ msg) msg actual;
    Alcotest.(check int) "Has one arg" 1 @@ RO_array.length args;
    RO_array.get args 0, answer

end

(* Callers can swap their arguments for the slot's contents. *)
let swap_service slot = object
  inherit test_service as super
  val name = "swap"

  method call x caps =
    super#check_refcount;
    let old_msg, old_caps = !slot in
    slot := (x, caps);
    return (old_msg, old_caps)
end

let logger () = object
  inherit test_service as super
  val name = "logger"

  val log = Queue.create ()

  method call x caps =
    super#check_refcount;
    assert (RO_array.length caps = 0);
    Queue.add x log;
    return ("logged", RO_array.empty)

  method pop =
    try Queue.pop log
    with Queue.Empty -> "(queue empty)"
end
