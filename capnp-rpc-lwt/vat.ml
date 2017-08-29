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
    restore : Restorer.t;
    mutable connections : CapTP.t list; (* todo: should be a map, once we have Vat IDs *)
  }

  let create ?switch ?(restore=Restorer.none) ?address () =
    let t = {
      switch;
      address;
      restore;
      connections = [];
    } in
    Lwt_switch.add_hook switch (fun () ->
        let ex = Capnp_rpc.Exception.v ~ty:`Disconnected "Vat shut down" in
        Lwt_list.iter_p (fun c -> CapTP.disconnect c ex) t.connections >|= fun () ->
        t.connections <- []
      );
    t

  let add_connection t endpoint =
    let switch = Lwt_switch.create () in
    Lwt_switch.add_hook t.switch (fun () -> Lwt_switch.turn_off switch);
    let conn = CapTP.connect ~switch ~restore:t.restore endpoint in
    t.connections <- conn :: t.connections;
    conn

  let public_address t = t.address

  let sturdy_ref t service =
    match t.address with
    | None -> failwith "sturdy_ref: vat was not configured with an address"
    | Some address ->
      Sturdy_ref.v ~address ~service

  let plain_endpoint ~switch flow =
    Endpoint.of_flow ~switch (module Underlying) flow

  let connect t sr =
    let addr = Sturdy_ref.address sr in
    let service = Sturdy_ref.service sr in
    (* todo: check if already connected to vat *)
    Network.connect addr >|= function
    | Error _ as e -> e
    | Ok ep -> Ok (CapTP.bootstrap (add_connection t ep) service)

  let connect_exn t sr =
    connect t sr >>= function
    | Ok x -> Lwt.return x
    | Error (`Msg msg) -> Lwt.fail_with msg

  let pp_vat_id f = function
    | None -> Fmt.string f "Client-only vat"
    | Some addr -> Fmt.pf f "Vat at %a" Network.Address.pp addr

  let dump f t =
    Fmt.pf f "@[<v2>%a@,%a@]"
      pp_vat_id t.address
      (Fmt.Dump.list CapTP.dump) t.connections
end
