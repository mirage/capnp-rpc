open Capnp_core
open Lwt.Infix

module Make (Network : Capnp_core.NETWORK) = struct

  module CapTP = CapTP_capnp.Make (Network)

  type t = {
    switch : Lwt_switch.t option;
    mutable bootstrap : Core_types.cap option;
    mutable connections : CapTP.t list; (* todo: should be a map, once we have Vat IDs *)
  }

  let create ?switch ?bootstrap () =
    let t = {
      switch;
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

end
