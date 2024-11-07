let src = Logs.Src.create "capnp-rpc" ~doc:"Cap'n Proto RPC"
module Log = (val Logs.src_log src: Logs.LOG)

let qid_tag = Logs.Tag.def "qid" Stdint.Uint32.printer

exception Invariant_broken of (Format.formatter -> unit)

let pp_exn f = function
  | Invariant_broken pp -> pp f
  | Failure msg -> Fmt.string f msg
  | ex -> Fmt.exn f ex

let failf msg = Fmt.kstr failwith msg

let invariant_broken f = raise (Invariant_broken f)

let () =
  Printexc.register_printer @@ function
  | Invariant_broken pp -> Some (Fmt.str "%t" pp)
  | _ -> None

module OID = struct
  type t = int

  let last_id = ref 0

  let next () =
    incr last_id;
    !last_id

  let pp f id =
    Fmt.(styled `Bold (styled `Cyan int)) f id

  let reset () =
    last_id := 0
end
