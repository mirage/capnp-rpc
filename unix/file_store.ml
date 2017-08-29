open Capnp_rpc_lwt

module ReaderOps = Capnp.Runtime.ReaderInc.Make(Capnp_rpc_lwt)

type 'a t = {
  dir : string;
  hash : Capnp_rpc_lwt.Auth.hash;
  vat_config : Vat_config.t;
}

let create ~dir ~hash vat_config =
  { dir; hash; vat_config }

let path_of_digest t digest =
  let filename = B64.encode ~alphabet:B64.uri_safe_alphabet ~pad:false digest in
  Filename.concat t.dir filename

let segments_of_reader = function
  | None -> []
  | Some ss -> Message.to_storage ss.StructStorage.data.Slice.msg

let update t ~digest data =
  let path = path_of_digest t digest in
  let tmp_path = path ^ ".new" in
  let ch = open_out tmp_path in
  let segments = segments_of_reader data in
  segments |> List.iter (fun {Message.segment; bytes_consumed} ->
      output ch segment 0 bytes_consumed
    );
  close_out ch;
  Unix.rename tmp_path path

let save t data =
  let id = Restorer.Id.generate () in
  let digest = Restorer.Id.digest t.hash id in
  update t ~digest data;
  Vat_config.sturdy_ref t.vat_config id

let remove t ~digest =
  let path = path_of_digest t digest in
  Unix.unlink path

let load t ~digest =
  let path = path_of_digest t digest in
  if Sys.file_exists path then (
    let ch = open_in path in
    let len = in_channel_length ch in
    let segment = really_input_string ch len in
    close_in ch;
    let msg = Message.of_storage [segment] in
    let reader = ReaderOps.get_root_struct (Message.readonly msg) in
    Some reader
  ) else (
    Logs.info (fun f -> f "File %S not found" path);
    None
  )

let hash t = t.hash
