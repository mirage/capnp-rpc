open Capnp_rpc

module ReaderOps = Capnp.Runtime.ReaderInc.Make(Capnp_rpc)

type 'a t = {
  dir : string;
}

let create dir = { dir }

let path_of_digest t digest =
  match Base64.encode ~alphabet:Base64.uri_safe_alphabet ~pad:false digest with
  | Ok filename -> Filename.concat t.dir filename
  | Error (`Msg m) -> failwith m  (* Encoding can't really fail *)

let segments_of_reader = function
  | None -> []
  | Some ss -> Message.to_storage ss.StructStorage.data.Slice.msg

let save t ~digest data =
  let path = path_of_digest t digest in
  let tmp_path = path ^ ".new" in
  let ch = open_out_bin tmp_path in
  Fun.protect ~finally:(fun () -> close_out ch) (fun () ->
    let segments = segments_of_reader data in
    segments |> List.iter (fun {Message.segment; bytes_consumed} ->
        output ch segment 0 bytes_consumed
      );
    );
  Unix.rename tmp_path path

let remove t ~digest =
  let path = path_of_digest t digest in
  Unix.unlink path

let load t ~digest =
  let path = path_of_digest t digest in
  if Sys.file_exists path then (
    let ch = open_in_bin path in
    let segment =
      Fun.protect ~finally:(fun () -> close_in ch) (fun () ->
          let len = in_channel_length ch in
          let segment = Bytes.create len in
          really_input ch segment 0 len;
          segment
        )
    in
    let msg = Message.of_storage [segment] in
    let reader = ReaderOps.get_root_struct (Message.readonly msg) in
    Some reader
  ) else (
    Logs.info (fun f -> f "File %S not found" path);
    None
  )
