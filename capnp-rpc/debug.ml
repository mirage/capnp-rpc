let src = Logs.Src.create "capnp-rpc" ~doc:"Cap'n Proto RPC"
module Log = (val Logs.src_log src: Logs.LOG)

let qid_tag = Logs.Tag.def "qid" Uint32.printer

exception Invariant_broken of (Format.formatter -> unit)

let pp_exn f = function
  | Invariant_broken pp -> pp f
  | Failure msg -> Fmt.string f msg
  | ex -> Fmt.exn f ex

let failf msg = Fmt.kstrf failwith msg

let invariant_broken f = raise (Invariant_broken f)

let () =
  Printexc.register_printer @@ function
  | Invariant_broken pp -> Some (Fmt.strf "%t" pp)
  | _ -> None
