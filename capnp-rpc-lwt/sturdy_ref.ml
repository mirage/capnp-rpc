open Lwt.Infix

class type [+'a] t = Capnp_core.sturdy_ref

let connect t = t#connect

let connect_exn t =
  connect t >>= function
  | Ok x -> Lwt.return x
  | Error e -> Lwt.fail_with (Fmt.to_to_string Capnp_rpc.Exception.pp e)

let reader fn s =
  fn s |> Schema.ReaderOps.string_of_pointer |> Uri.of_string

let builder fn (s : 'a Capnp.BytesMessage.StructStorage.builder_t) (sr : 'a t) =
  sr#to_uri_with_secrets |> Uri.to_string |> Schema.BuilderOps.write_string (fn s)

let cast t = t

let with_cap t f =
  connect t >>= function
  | Ok x -> Capability.with_ref x f
  | Error e -> Lwt_result.fail (`Capnp e)

let with_cap_exn t f =
  connect_exn t >>= fun x ->
  Capability.with_ref x f
