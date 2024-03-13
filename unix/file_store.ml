open Capnp_rpc_lwt

module ReaderOps = Capnp.Runtime.ReaderInc.Make(Capnp_rpc_lwt)

let ( / ) = Eio.Path.( / )

type 'a t = {
  dir : Eio.Fs.dir_ty Eio.Path.t;
}

let create dir = { dir = (dir :> Eio.Fs.dir_ty Eio.Path.t) }

let leaf_of_digest digest =
  Base64.encode_exn ~alphabet:Base64.uri_safe_alphabet ~pad:false digest

let segments_of_reader = function
  | None -> []
  | Some ss -> Message.to_storage ss.StructStorage.data.Slice.msg

let save t ~digest data =
  let leaf = leaf_of_digest digest in
  let tmp_leaf = leaf ^ ".new" in
  Eio.Path.with_open_out ~create:(`Exclusive 0o644) (t.dir / tmp_leaf) (fun flow ->
    let segments = segments_of_reader data in
    segments |> List.iter (fun {Message.segment; bytes_consumed} ->
        let buf = Cstruct.of_bytes segment ~len:bytes_consumed in
        Eio.Flow.copy (Eio.Flow.cstruct_source [buf]) flow
      );
    );
  Eio.Path.rename (t.dir / tmp_leaf) (t.dir / leaf)

let remove t ~digest =
  Eio.Path.unlink (t.dir / leaf_of_digest digest)

let load t ~digest =
  let leaf = leaf_of_digest digest in
  match Eio.Path.load (t.dir / leaf) with
  | segment ->
    let msg = Message.of_storage [Bytes.unsafe_of_string segment] in
    let reader = ReaderOps.get_root_struct (Message.readonly msg) in
    Some reader
  | exception Eio.Io (Eio.Fs.E Not_found _, _) ->
    Logs.info (fun f -> f "File %S not found" leaf);
    None
