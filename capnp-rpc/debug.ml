let src = Logs.Src.create "capnp-rpc" ~doc:"Cap'n Proto RPC"
module Log = (val Logs.src_log src: Logs.LOG)

let qid_tag = Logs.Tag.def "qid" Uint32.printer
