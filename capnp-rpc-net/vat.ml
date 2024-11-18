open Eio.Std
open Capnp_rpc.Std

module Log = Capnp_rpc.Debug.Log

module ID_map = Auth.Digest.Map

module Make (Network : S.NETWORK) = struct
  module CapTP = CapTP_capnp.Make (Network)

  let hash = `SHA256 (* Only support a single hash for now *)

  type connection_attempt = (CapTP.t, Capnp_rpc.Exception.t) result Eio.Promise.t

  type t = {
    sw : Eio.Switch.t;
    network : Network.t;
    secret_key : Auth.Secret_key.t Lazy.t;
    address : Network.Address.t option;
    restore : Restorer.t;
    tags : Logs.Tag.set;
    connection_removed : Eio.Condition.t;        (* Fires when a connection is removed *)
    mutable connecting : connection_attempt ID_map.t; (* Out-going connections being attempted. *)
    mutable connections : CapTP.t ID_map.t;      (* Accepted connections *)
    mutable anon_connections : CapTP.t list;     (* Connections not using TLS. *)
  }

  let create ?(tags=Logs.Tag.empty) ?(restore=Restorer.none) ?address ~sw ~secret_key network =
    Fiber.fork_daemon ~sw Capnp_rpc.Leak_handler.run;
    {
      sw;
      network;
      secret_key;
      address;
      restore;
      tags;
      connection_removed = Eio.Condition.create ();
      connecting = ID_map.empty;
      connections = ID_map.empty;
      anon_connections = [];
    }

  let run_connection_generic t ~add ~remove endpoint =
    let conn = CapTP.connect ~sw:t.sw ~tags:t.tags ~restore:t.restore endpoint in
    add conn;
    Fun.protect (fun () ->
        Fiber.both
          (fun () -> Endpoint.run_writer ~tags:t.tags endpoint)
          (fun () -> CapTP.listen conn)
      )
      ~finally:(fun () ->
          remove conn;
          Eio.Condition.broadcast t.connection_removed
        )

  let run_connection_tls t endpoint r =
    let peer_id = Endpoint.peer_id endpoint in
    run_connection_generic t endpoint
      ~add:(fun conn -> t.connections <- ID_map.add peer_id conn t.connections; r conn)
      ~remove:(fun conn ->
          match ID_map.find_opt peer_id t.connections with
          | Some x when x == conn -> t.connections <- ID_map.remove peer_id t.connections
          | Some _        (* Already replaced by a new one? *)
          | None -> ()
        )

  (* Run CapTP on [endpoint], calling [r conn] with the connection (possibly reusing an existing one).
     If a new connection is used, it is also stored in [t] while running. *)
  let run_connection t ~(mode:[`Accept|`Connect]) endpoint r =
    let peer_id = Endpoint.peer_id endpoint in
    if peer_id = Auth.Digest.insecure then (
      run_connection_generic t endpoint
        ~add:(fun conn -> t.anon_connections <- conn :: t.anon_connections; r conn)
        ~remove:(fun conn -> t.anon_connections <- List.filter ((!=) conn) t.anon_connections)
    ) else match ID_map.find_opt peer_id t.connections with
      | None -> run_connection_tls t endpoint r
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
          let reason = Capnp_rpc.Exception.v "Closing duplicate connection" in
          CapTP.disconnect existing reason;
          run_connection_tls t endpoint r
        ) else (
          Endpoint.disconnect endpoint;
          r existing
        )

  let public_address t = t.address

  (* Make a new connection to remote service [addr] and request [service] from it. *)
  let initiate_connection t addr service =
    let remote_id = Network.Address.digest addr in
    let p, r = Promise.create () in
    let tracked = remote_id <> Auth.Digest.insecure in
    if tracked then t.connecting <- ID_map.add remote_id p t.connecting;
    Fun.protect
      ~finally:(fun () ->
          if tracked then t.connecting <- ID_map.remove remote_id t.connecting;
          if not (Promise.is_resolved p) then Promise.resolve_error r Capnp_rpc.Exception.cancelled
        )
      (fun () ->
         Fiber.fork_daemon ~sw:t.sw (fun () ->
             Switch.run (fun sw ->
                 match Network.connect ~sw t.network ~secret_key:t.secret_key addr with
                 | Error (`Msg m) -> Promise.resolve_error r (Capnp_rpc.Exception.v m)
                 | Ok ep -> run_connection t ep (Promise.resolve_ok r) ~mode:`Connect
               );
             `Stop_daemon
           );
         Promise.await p
      )
    |> Result.map (fun conn -> CapTP.bootstrap conn service)

  (* Get a connection to [addr] and request [service] from it. *)
  let rec connect t (addr, service) =
    let remote_id = Network.Address.digest addr in
    let my_id = Auth.Secret_key.digest ~hash (Lazy.force t.secret_key) in
    if Auth.Digest.equal remote_id my_id then
      Restorer.restore t.restore service
    else match ID_map.find_opt remote_id t.connections with
      | Some conn when CapTP.disconnecting conn ->
        Eio.Condition.await_no_mutex t.connection_removed;
        connect t (addr, service)
      | Some conn ->
        (* Already connected; use that. *)
        Ok (CapTP.bootstrap conn service)
      | None ->
        match ID_map.find_opt remote_id t.connecting with
        | None -> initiate_connection t addr service
        | Some conn ->
          (* We're already trying to establish a connection, wait for that. *)
          Promise.await conn |> Result.map (fun conn -> CapTP.bootstrap conn service)

  let make_sturdy_ref t sr =
    Capnp_rpc.Cast.sturdy_of_raw @@ object (_ : Capnp_rpc.Private.Capnp_core.sturdy_ref)
      method connect = Result.map Capnp_rpc.Cast.cap_to_raw (connect t sr)
      method to_uri_with_secrets = Network.Address.to_uri sr
    end

  let sturdy_ref t service : 'a Sturdy_ref.t =
    match t.address with
    | None -> failwith "sturdy_ref: vat was not configured with an address"
    | Some address -> make_sturdy_ref t (address, service)

  let export _t sr =
    (* [t] isn't used currently. However, requiring it does emphasise that importing/exporting
       is a somewhat privileged operation (as it reveals the secret tokens in the sturdy ref). *)
    (Capnp_rpc.Cast.sturdy_to_raw sr)#to_uri_with_secrets

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

  let dump_id_map pp f m =
    Fmt.pf f "{@[<v0>%a@]}" (Fmt.seq pp) (ID_map.to_seq m)

  let dump f t =
    Fmt.pf f "@[<v2>%a@,Connecting: %a@,Connected: %a@,Anonymous: %a@]"
      pp_vat_id t.address
      (dump_id_map dump_connecting) t.connecting
      (dump_id_map dump_id_conn) t.connections
      (Fmt.Dump.list CapTP.dump) t.anon_connections
end
