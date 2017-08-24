open Capnp_core
open Lwt.Infix

module Log = Capnp_rpc.Debug.Log

let error fmt =
  fmt |> Fmt.kstrf @@ fun msg ->
  Error (`Msg msg)

module Make (Network : S.NETWORK) (Underlying : Mirage_flow_lwt.S) = struct
  module Sturdy_ref = Sturdy_ref.Make (Network)
  module CapTP = CapTP_capnp.Make (Network)

  type t = {
    switch : Lwt_switch.t option;
    address : Network.Address.t option;
    mutable bootstrap : Core_types.cap option;
    mutable connections : CapTP.t list; (* todo: should be a map, once we have Vat IDs *)
  }

  let create ?switch ?bootstrap ?address () =
    let t = {
      switch;
      address;
      bootstrap;
      connections = [];
    } in
    Lwt_switch.add_hook switch (fun () ->
        begin match t.bootstrap with
          | Some x -> Core_types.dec_ref x; t.bootstrap <- None
          | None -> ()
        end;
        let ex = Capnp_rpc.Exception.v ~ty:`Disconnected "Vat shut down" in
        Lwt_list.iter_p (fun c -> CapTP.disconnect c ex) t.connections >|= fun () ->
        t.connections <- []
      );
    t

  let connect t endpoint =
    let switch = Lwt_switch.create () in
    Lwt_switch.add_hook t.switch (fun () -> Lwt_switch.turn_off switch);
    let conn = CapTP.connect ~switch ?offer:t.bootstrap endpoint in
    t.connections <- conn :: t.connections;
    conn

  let public_address t = t.address

  let bootstrap_ref t =
    match t.address with
    | None -> failwith "bootstrap_ref: vat was not configured with an address"
    | Some address ->
      Sturdy_ref.v ~address ~service:`Bootstrap

  let pp_bootstrap_uri f t =
    if t.bootstrap = None then Fmt.string f "(vat has no bootstrap service)"
    else if t.address = None then Fmt.string f "(vat has no public address)"
    else Sturdy_ref.pp_with_secrets f (bootstrap_ref t)

  let plain_endpoint ~switch flow =
    Endpoint.of_flow ~switch (module Underlying) flow

  let live t sr =
    let addr = Sturdy_ref.address sr in
    (* todo: check if already connected to vat *)
    Network.connect addr >|= function
    | Error _ as e -> e
    | Ok ep -> Ok (CapTP.bootstrap @@ connect t ep)

  let pp_vat_id f = function
    | None -> Fmt.string f "Client-only vat"
    | Some addr -> Fmt.pf f "Vat at %a" Network.Address.pp addr

  let dump f t =
    Fmt.pf f "@[<v2>%a@,%a@]"
      pp_vat_id t.address
      (Fmt.Dump.list CapTP.dump) t.connections
end
