open Capnp_direct.Core_types

module Msg = Capnp_direct.String_content

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
  method call results msg =
    super#check_refcount;
    resolve_ok results {msg with Msg.data = "got:" ^ msg.Msg.data}
end

(* A service which just queues incoming messages and lets the test driver handle them. *)
let manual () = object (self)
  inherit test_service as super
  val queue = Queue.create ()
  val name = "manual"

  method call results x =
    super#check_refcount;
    Queue.add (x, results) queue

  method pop_n msg =
    match Queue.pop queue with
    | exception Queue.Empty -> Capnp_rpc.Debug.failf "Empty queue (expecting %S)" msg
    | actual, answer ->
      Alcotest.(check string) ("Expecting " ^ msg) msg actual.Msg.data;
      let args = Request_payload.snapshot_caps actual in
      args, answer

  (* Expect a message with no caps *)
  method pop0 msg =
    let args, answer = self#pop_n msg in
    Alcotest.(check int) "Has no args" 0 @@ RO_array.length args;
    answer

  (* Expect a message with one cap *)
  method pop1 msg =
    let args, answer = self#pop_n msg in
    Alcotest.(check int) "Has one arg" 1 @@ RO_array.length args;
    RO_array.get_exn args 0, answer
end

(* Callers can swap their arguments for the slot's contents. *)
let swap_service slot = object
  inherit test_service as super
  val name = "swap"

  method call results x =
    super#check_refcount;
    let old_msg = !slot in
    slot := x;
    resolve_ok results old_msg
end

let logger () = object
  inherit test_service as super
  val name = "logger"

  val log = Queue.create ()

  method call results x =
    super#check_refcount;
    let caps = Request_payload.snapshot_caps x in
    assert (RO_array.length caps = 0);
    Queue.add x.Msg.data log;
    resolve_ok results (Msg.Response.v "logged")

  method pop =
    try Queue.pop log
    with Queue.Empty -> "(queue empty)"
end
