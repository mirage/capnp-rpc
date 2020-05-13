module Log = Capnp_rpc.Debug.Log
module B = Schema.Builder
module R = Schema.Reader
module RO_array = Capnp_rpc.RO_array
module StructStorage = Capnp.Message.BytesMessage.StructStorage

type Capnp.MessageSig.attachments += RPC_attachments of Capnp_rpc.S.attachments

module Path = struct
  type t = Xform.t list
  let compare = compare
  let pp = Fmt.Dump.list Xform.pp
  let root = []
end

type request = [`Call_836a53ce789d4cd4]
type response = [`Return_9e19b28d3db3573a]
type 'a msg =
  | Builder of 'a StructStorage.builder_t
  | Readonly of 'a StructStorage.reader_t
(* We can sometimes avoid a copy by returning the builder directly.
   e.g. the application code builds a call and passes it to a proxy, which transmits
   it over the network. A message can only be transmitted once. *)

let with_attachments a t =
  match t with
  | Builder x -> Builder (StructStorage.with_attachments (RPC_attachments a) x)
  | Readonly None -> Readonly None
  | Readonly (Some x) -> Readonly (Some (StructStorage.with_attachments (RPC_attachments a) x))

let unwrap_attachments = function
  | RPC_attachments x -> x
  | Capnp.MessageSig.No_attachments -> Capnp_rpc.S.No_attachments
  | _ -> failwith "Unknown attachment type!"

let attachments = function
  | Readonly None -> Capnp_rpc.S.No_attachments
  | Readonly (Some ss) -> unwrap_attachments @@ StructStorage.get_attachments ss
  | Builder ss -> unwrap_attachments @@ StructStorage.get_attachments ss

let wrap_attachments a = RPC_attachments a

module Request = struct
  type t = request msg
  (* Call message with interface_id, method_id and payload.content filled in *)

  let copy rcall = (* todo: set init size from src; also for copy_resp *)
    let msg = B.Message.init_root () in
    let call = B.Message.call_init msg in
    B.Call.interface_id_set call (R.Call.interface_id_get rcall);
    B.Call.method_id_set_exn call (R.Call.method_id_get rcall);
    (* Only copy the contents, not the caps. *)
    let payload = B.Call.params_init call in
    let rpayload = R.Call.params_get rcall in
    B.Payload.content_set_reader payload (R.Payload.content_get rpayload) |> ignore;
    call

  let writable : t -> _ = function
    | Readonly call -> copy call
    | Builder call -> call

  let readable = function
    | Readonly call -> call
    | Builder call -> R.Call.of_builder call

  let of_builder x = Builder x
  let of_reader x = Readonly x

  let cap_index t path =
    let call = readable t in
    Xform.resolve (R.Call.params_get call) path

  let pp f _ = Fmt.string f "(request content)"

  let with_attachments = with_attachments
  let attachments = attachments
end

module Response = struct
  type t = response msg

  let copy (rret : R.Return.t) =
    let msg = B.Message.init_root () in
    let ret = B.Message.return_init msg in
    begin match R.Return.get rret with
    | R.Return.Results rpayload ->
      (* Only copy the contents, not the caps. *)
      let payload = B.Return.results_init ret in
      B.Payload.content_set_reader payload (R.Payload.content_get rpayload) |> ignore
    | _ -> failwith "Not a results message!"
    end;
    ret

  let writable = function
    | Readonly x -> copy x
    | Builder ret -> ret

  let readable resp =
    let resp =
      match resp with
      | Readonly x -> x
      | Builder x -> R.Return.of_builder x
    in
    match R.Return.get resp with
    | R.Return.Results r -> r
    | _ -> assert false

  let bootstrap () =
    let msg = B.Message.init_root ~message_size:150 () in
    let ret = B.Message.return_init msg in
    let p = B.Return.results_init ret in
    B.Payload.content_set_interface p (Some Stdint.Uint32.zero);   (* Cap index 0 *)
    Builder ret

  let of_builder x = Builder x
  let of_reader x = Readonly x

  let cap_index t path =
    let results = readable t in
    Xform.resolve results path

  let pp f _ = Fmt.string f "(response content)"

  let with_attachments = with_attachments
  let attachments = attachments
end
