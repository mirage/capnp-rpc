open Eio.Std
open Capnp_rpc_lwt
open Capnp_rpc_net

module File_store = Capnp_rpc_unix.File_store
module Store = Store.Make(Capnp.BytesMessage)

type loader = [`Logger_beacebd78653e9af] Sturdy_ref.t -> label:string -> Restorer.resolution

type t = {
  store : Store.Reader.SavedService.struct_t File_store.t;
  loader : loader Promise.t;
  make_sturdy : Restorer.Id.t -> Uri.t;
}

let hash _ = `SHA256

let make_sturdy t = t.make_sturdy

let save t ~digest label =
  let open Store.Builder in
  let service = SavedService.init_root () in
  let logger = SavedService.logger_init service in
  SavedLogger.label_set logger label;
  File_store.save t.store ~digest @@ SavedService.to_reader service

let save_new t ~label =
  let id = Restorer.Id.generate () in
  let digest = Restorer.Id.digest (hash t) id in
  save t ~digest label;
  id

let load t sr digest =
  match File_store.load t.store ~digest with
  | None -> Restorer.unknown_service_id
  | Some saved_service ->
    let logger = Store.Reader.SavedService.logger_get saved_service in
    let label = Store.Reader.SavedLogger.label_get logger in
    let sr = Capnp_rpc_lwt.Sturdy_ref.cast sr in
    let loader = Promise.await t.loader in
    loader sr ~label

let create ~make_sturdy dir =
  let loader, set_loader = Promise.create () in
  if not (Eio.Path.is_directory dir) then
    Eio.Path.mkdir dir ~perm:0o755;
  let store = File_store.create dir in
  {store; loader; make_sturdy}, set_loader
