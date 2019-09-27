type actor = Fmt.style * string

let pp_actor f (style, name) = Fmt.(styled style string) f name

let pp_peer f = function
  | None -> Fmt.string f "       "
  | Some peer -> Fmt.pf f "(%a)" pp_actor peer

let unknown = `Black, "-----"

let actor_tag = Logs.Tag.def "actor" pp_actor
let peer_tag = Logs.Tag.def "peer" pp_actor

let pp_qid f = function
  | None -> ()
  | Some x ->
    let s = Stdint.Uint32.to_string x in
    Fmt.(styled `Magenta (fun f x -> Fmt.pf f " (qid=%s)" x)) f s

let reporter =
  let report src level ~over k msgf =
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?(tags=Logs.Tag.empty) fmt ->
    let actor =
      match Logs.Tag.find actor_tag tags with
      | Some x -> x
      | None -> unknown
    in
    let peer = Logs.Tag.find peer_tag tags in
    let qid = Logs.Tag.find Capnp_rpc.Debug.qid_tag tags in
    let print _ =
      Fmt.(pf stdout) "%a@." pp_qid qid;
      over ();
      k ()
    in
    Fmt.kpf print Fmt.stdout ("%a %a %a%a: @[" ^^ fmt ^^ "@]")
      Fmt.(styled `Magenta string) (Printf.sprintf "%11s" src)
      Logs_fmt.pp_header (level, header)
      pp_actor actor
      pp_peer peer
  in
  { Logs.report = report }

let () =
  Logs.set_reporter reporter;
  Logs.set_level (Some Logs.Info)

let server_tags = Logs.Tag.(empty |> add actor_tag (`Red, "vat-S"))
let client_tags = Logs.Tag.(empty |> add actor_tag (`Green, "vat-C"))
