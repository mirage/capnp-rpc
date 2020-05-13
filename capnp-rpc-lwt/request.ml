open Capnp_core
open Schema.Builder
module RO_array = Capnp_rpc.RO_array
module StructStorage = Capnp.Message.BytesMessage.StructStorage

type 'a t = Message.t

let create ?message_size init =
  let msg =
    Message.init_root ?message_size ()
    |> StructStorage.with_attachments (Msg.wrap_attachments (Core_types.Attachments.builder ())) in
  let call = Message.call_init msg in
  let p = Call.params_get call in
  let content = init (Payload.content_get p) in
  msg, content

let create_no_args () =
  let msg = Message.init_root ~message_size:200 () in
  ignore (Message.call_init msg);
  msg

let finish m t =
  match Message.get t with
  | Message.Call c ->
    let msg = Msg.Request.of_builder c in
    Call.interface_id_set c @@ Capnp.RPC.MethodID.interface_id m;
    Call.method_id_set_exn c @@ Capnp.RPC.MethodID.method_id m;
    msg
  | _ -> assert false

let release t =
  Core_types.Attachments.release_caps (Msg.unwrap_attachments (StructStorage.get_attachments t))
