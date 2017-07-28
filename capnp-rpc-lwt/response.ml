open Capnp_core
open Schema.Builder
module RO_array = Capnp_rpc.RO_array

type 'a cap = Core_types.cap

type 'a t = Message.t

let create init =
  let msg = Message.init_root () |> with_attachments (Msg.wrap_attachments (Core_types.Attachments.builder ())) in
  let ret = Message.return_init msg in
  let p = Return.results_init ret in
  let content = init (Payload.content_get p) in
  msg, content

let create_empty () =
  let msg = Message.init_root () in
  let _ = Message.return_init msg in
  msg

let finish t =
  match Message.get t with
  | Message.Return r -> Msg.Response.of_builder r
  | _ -> assert false

let release t =
  Core_types.Attachments.release_caps (Msg.unwrap_attachments (get_attachments t))
