let src = Logs.Src.create "capnp-rpc" ~doc:"Cap'n Proto RPC"
module Log = (val Logs.src_log src: Logs.LOG)

module RO_array = Capnp_rpc.RO_array

type ('a, 'b) sharable =
  | Builder of 'a
  | Readonly of 'b
(* We can sometimes avoid a copy by returning the builder directly. *)

let writable ~copy = function
  | Builder x -> x
  | Readonly x -> copy x

let readable read = function
  | Builder x -> read x
  | Readonly x -> x

type req_msg = (Schema.Builder.Call.t, Schema.Reader.Call.t) sharable
(* Message with interface_id, method_id and payload.content filled in *)

let copy_req rcall = (* todo: set init size from src; also for copy_resp *)
  let module B = Schema.Builder in
  let module R = Schema.Reader in
  let msg = B.Message.init_root () in
  let call = B.Message.call_init msg in
  B.Call.interface_id_set call (R.Call.interface_id_get rcall);
  B.Call.method_id_set_exn call (R.Call.method_id_get rcall);
  (* Only copy the contents, not the caps. *)
  let payload = B.Call.params_init call in
  let rpayload = R.Call.params_get rcall in
  B.Payload.content_set_reader payload (R.Payload.content_get rpayload) |> ignore;
  call

let writable_req = writable ~copy:copy_req
let readable_req = readable Schema.Reader.Call.of_builder

type resp_msg = (Schema.Builder.Return.t, Schema.Reader.Return.t) sharable

let copy_resp (rret : Schema.Reader.Return.t) =
  let module B = Schema.Builder in
  let module R = Schema.Reader in
  let msg = B.Message.init_root () in
  let ret = B.Message.return_init msg in
  begin match R.Return.get rret with
  | R.Return.Results rpayload ->
    (* Only copy the contents, not the caps. *)
    let payload = B.Return.results_init ret in
    B.Payload.content_set_reader payload (R.Payload.content_get rpayload) |> ignore
  | _ -> failwith "todo: copy_resp"
  end;
  ret

let writable_resp = writable ~copy:copy_resp
let readable_resp = readable Schema.Reader.Return.of_builder
