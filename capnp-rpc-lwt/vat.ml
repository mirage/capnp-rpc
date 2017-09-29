open Lwt.Infix

module Log = Capnp_rpc.Debug.Log

module ID_map = Auth.Digest.Map

module Make (Network : S.NETWORK) (Underlying : Mirage_flow_lwt.S) = struct
  module CapTP = CapTP_capnp.Make (Network)

  let hash = `SHA256 (* Only support a single hash for now *)

  type connection_attempt = (CapTP.t, Capnp_rpc.Exception.t) result Lwt.t

  type t = {
    network : Network.t;
    switch : Lwt_switch.t option;
    secret_key : Auth.Secret_key.t Lazy.t;
    address : Network.Address.t option;
    restore : Restorer.t;
    tags : Logs.Tag.set;
    connection_removed : unit Lwt_condition.t;   (* Fires when a connection is removed *)
    mutable connecting : connection_attempt ID_map.t; (* Out-going connections being attempted. *)
    mutable connections : CapTP.t ID_map.t;      (* Accepted connections *)
    mutable anon_connections : CapTP.t list;     (* Connections not using TLS. *)
  }

  let create ?switch ?(tags=Logs.Tag.empty) ?(restore=Restorer.none) ?address ~secret_key network =
    let t = {
      network;
      switch;
      secret_key;
      address;
      restore;
      tags;
      connection_removed = Lwt_condition.create ();
      connecting = ID_map.empty;
      connections = ID_map.empty;
      anon_connections = [];
    } in
    Lwt_switch.add_hook switch (fun () ->
        let ex = Capnp_rpc.Exception.v ~ty:`Disconnected "Vat shut down" in
        ID_map.bindings t.connections |> Lwt_list.iter_p (fun (_, c) -> CapTP.disconnect c ex) >>= fun () ->
        t.connections <- ID_map.empty;
        Lwt_list.iter_p (fun c -> CapTP.disconnect c ex) t.anon_connections >|= fun () ->
        t.anon_connections <- [];
        ID_map.iter (fun _ th -> Lwt.cancel th) t.connecting;
        t.connecting <- ID_map.empty;
      );
    t

  let add_tls_connection t ~switch endpoint =
    let conn = CapTP.connect ~tags:t.tags ~restore:t.restore endpoint in
    let peer_id = Endpoint.peer_id endpoint in
    t.connections <- ID_map.add peer_id conn t.connections;
    Lwt_switch.add_hook (Some switch) (fun () ->
        begin match ID_map.find peer_id t.connections with
        | Some x when x == conn -> t.connections <- ID_map.remove peer_id t.connections
        | Some _        (* Already replaced by a new one? *)
        | None -> ()
        end;
        CapTP.disconnect conn (Capnp_rpc.Exception.v ~ty:`Disconnected "Switch turned off") >|= fun () ->
        Lwt_condition.broadcast t.connection_removed ()
      );
    conn

  let add_connection t ~switch ~(mode:[`Accept|`Connect]) endpoint =
    let tags = t.tags in
    let peer_id = Endpoint.peer_id endpoint in
    if peer_id = Auth.Digest.insecure then (
      let conn = CapTP.connect ~tags ~restore:t.restore endpoint in
      t.anon_connections <- conn :: t.anon_connections;
      Lwt_switch.add_hook (Some switch) (fun () ->
          t.anon_connections <- List.filter ((!=) conn) t.anon_connections;
          CapTP.disconnect conn (Capnp_rpc.Exception.v ~ty:`Disconnected "Switch turned off") >|= fun () ->
          Lwt_condition.broadcast t.connection_removed ()
        );
      Lwt.return conn
    ) else match ID_map.find peer_id t.connections with
      | None -> Lwt.return @@ add_tls_connection t ~switch endpoint
      | Some existing ->
        Log.info (fun f -> f ~tags:t.tags "Trying to add a connection, but we already have one for this vat");
        (* This can happen if two vats call each other at exactly the same time.
           Ideally, we would seamlessly merge the connections, but for now just
           reject one of them and wait for the user to retry.

           We must ensure that both ends discard the same connection though!
           The connection to keep is the one initiated by the peer with the
           highest vat ID. i.e.

           - we are connecting and have the greater ID =>
               [conn] was initiated by us, the endpoint with the higher ID

           - we are accepting and don't have the greater ID =>
               [conn] was initiated by the peer, which has the higher ID

           In those cases, we keep the new [conn].
           Otherwise, we keep the existing one.

           There are several cases to consider:

           - We decide to drop our new out-bound connection [conn] and use the existing
             in-bound one we have already accepted. The peer knows that it has accepted
             [conn] but might not know that [existing] was accepted too yet.
             If it gets confirmation of [existing] first, it will drop [conn] and just use
             that. If it gets the rejection of [conn] first, it will attempt to reconnect
             and join its existing connection attempt with [existing], which will soon
             succeed.

           - We decide to drop our existing out-bound connection and use the new
             in-bound one [conn]. When our user tries to reconnect, they will
             find the new in-bound one and succeed. The peer knows it accepted
             [conn] but may not be sure of [existing] yet. Either way, it will
             continue with [conn] without trouble.

           - We decide to drop our newly accepted in-bound connection [conn]
             and use the existing out-bound one. Since the peer has already
             approved the [existing], it no longer cares about this one.

           - We decide to drop our previously-accepted in-bound connection and
             use our new out-bound one [conn]. When the peer accepted our out-bound
             connection it already switched away from the old one.

           (it could also happen if the peer initiated two connections with the
           same digest, but a good peer won't do that)
        *)
        let my_id = Auth.Secret_key.digest ~hash (Lazy.force t.secret_key) in
        let keep_new = (my_id > peer_id) = (mode = `Connect) in
        if keep_new then (
          let conn = add_tls_connection t ~switch endpoint in
          let reason = Capnp_rpc.Exception.v "Closing duplicate connection" in
          CapTP.disconnect existing reason >|= fun () ->
          conn
        ) else (
          Lwt_switch.turn_off switch >|= fun () ->
          existing
        )

  let public_address t = t.address

  let connect_anon t addr ~service =
    let switch = Lwt_switch.create () in
    Network.connect t.network ~switch ~secret_key:t.secret_key addr >>= function
    | Error (`Msg m) -> Lwt.return @@ Error (Capnp_rpc.Exception.v m)
    | Ok ep ->
      add_connection t ~switch ep ~mode:`Connect >|= fun conn ->
      Ok (CapTP.bootstrap conn service)

  let initiate_connection t remote_id addr service =
    (* We need to start a new connection attempt. *)
    let switch = Lwt_switch.create () in
    let conn =
      Network.connect t.network ~switch ~secret_key:t.secret_key addr >>= function
      | Error (`Msg m) -> Lwt.return @@ Error (Capnp_rpc.Exception.v m)
      | Ok ep -> add_connection t ~switch ep ~mode:`Connect >|= fun conn -> Ok conn
    in
    t.connecting <- ID_map.add remote_id conn t.connecting;
    conn >|= fun conn ->
    t.connecting <- ID_map.remove remote_id t.connecting;
    match conn with
    | Ok conn -> Ok (CapTP.bootstrap conn service)
    | Error _  as e -> e

  let rec connect_auth t remote_id addr ~service =
    let my_id = Auth.Secret_key.digest ~hash (Lazy.force t.secret_key) in
    if Auth.Digest.equal remote_id my_id then
      Restorer.restore t.restore service
    else match ID_map.find remote_id t.connections with
      | Some conn when CapTP.disconnecting conn ->
        Lwt_condition.wait t.connection_removed >>= fun () ->
        connect_auth t remote_id addr ~service
      | Some conn ->
        (* Already connected; use that. *)
        Lwt.return @@ Ok (CapTP.bootstrap conn service)
      | None ->
        match ID_map.find remote_id t.connecting with
        | None -> initiate_connection t remote_id addr service
        | Some conn ->
          (* We're already trying to establish a connection, wait for that. *)
          conn >|= function
          | Ok conn -> Ok (CapTP.bootstrap conn service)
          | Error _ as e -> e

  let make_sturdy_ref t sr =
    object
      method connect =
        let (addr, service) = sr in
        let remote_id = Network.Address.digest addr in
        if remote_id = Auth.Digest.insecure then connect_anon t addr ~service
        else connect_auth t remote_id addr ~service

      method to_uri_with_secrets = Network.Address.to_uri sr
    end

  let sturdy_ref t service : Capnp_core.sturdy_ref =
    match t.address with
    | None -> failwith "sturdy_ref: vat was not configured with an address"
    | Some address -> make_sturdy_ref t (address, service)

  let export _t sr =
    (* [t] isn't used currently. However, requiring it does emphasise that importing/exporting
       is a somewhat privileged operation (as it reveals the secret tokens in the sturdy ref). *)
    sr#to_uri_with_secrets

  let sturdy_uri t id = sturdy_ref t id |> export t

  let import t uri =
    match Network.Address.parse_uri uri with
    | Error _ as e -> e
    | Ok sr -> Ok (make_sturdy_ref t sr)

  let import_exn t uri =
    match import t uri with
    | Ok x -> x
    | Error (`Msg m) -> failwith m

  let pp_vat_id f = function
    | None -> Fmt.string f "Client-only vat"
    | Some addr -> Fmt.pf f "Vat at %a" Network.Address.pp addr

  let dump_connecting f (id, _) =
    Fmt.pf f "%a => (pending)"
      Auth.Digest.pp id

  let dump_id_conn f (id, conn) =
    Fmt.pf f "%a => %a"
      Auth.Digest.pp id
      CapTP.dump conn

  let dump f t =
    Fmt.pf f "@[<v2>%a@,Connecting: %a@,Connected: %a@,Anonymous: %a@]"
      pp_vat_id t.address
      (ID_map.dump dump_connecting) t.connecting
      (ID_map.dump dump_id_conn) t.connections
      (Fmt.Dump.list CapTP.dump) t.anon_connections
end
