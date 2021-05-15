open Utils

class type [+'a] t = Capnp_core.sturdy_ref

let connect t =
  Fut.make
    (fun () -> t#connect)

let connect_exn t =
  Fut.make
    (fun () ->
      match t#connect with
      | Ok x -> x
      | Error e -> failwith (Fmt.to_string Capnp_rpc.Exception.pp e))

let reader fn s =
  fn s |> Schema.ReaderOps.string_of_pointer |> Uri.of_string

let builder fn (s : 'a Capnp.BytesMessage.StructStorage.builder_t) (sr : 'a t) =
  sr#to_uri_with_secrets |> Uri.to_string |> Schema.BuilderOps.write_string (fn s)

let cast t = t
