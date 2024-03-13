module Api = Persistent.Make(Capnp.BytesMessage)

class type ['a] persistent = object
  method save : ('a Sturdy_ref.t, Capnp_rpc.Exception.t) result
end

let with_persistence
    (persistent:'b #persistent)
    (_:(#Service.generic as 'a) -> 'b Capability.t)
    (impl : 'a) =
  (* We ignore the second argument. It's just to force the user to prove that [impl]
     really does have type ['a]. *)
  let dispatch_persistent method_id _params release_params =
    if method_id = Capnp.RPC.MethodID.method_id Api.Client.Persistent.Save.method_id then (
      let open Api.Service.Persistent.Save in
      release_params ();
      match persistent#save with
      | Error e -> Service.error (`Exception e)
      | Ok sr ->
        let resp, results = Service.Response.create Results.init_pointer in
        Sturdy_ref.builder Results.sturdy_ref_get results sr;
        Service.return resp
    ) else (
      release_params ();
      Service.fail ~ty:`Unimplemented "Unknown persistence method %d" method_id
    )
  in
  let wrapper = object (_ : #Service.generic)
    method release = impl#release
    method pp = impl#pp
    method dispatch ~interface_id ~method_id =
      if interface_id = Api.Service.Persistent.interface_id then dispatch_persistent method_id
      else impl#dispatch ~interface_id ~method_id
  end in
  Service.local wrapper

let with_sturdy_ref sr local impl =
  let persistent = object
    method save = Ok sr
  end in
  with_persistence persistent local impl

let save cap =
  let open Api.Client.Persistent.Save in
  let request = Capability.Request.create_no_args () in
  match Capability.call_for_value cap method_id request with
  | Error _ as e -> e
  | Ok response -> Ok (Sturdy_ref.reader Results.sturdy_ref_get response)

let save_exn cap =
  match save cap with
  | Error (`Capnp e) -> failwith (Fmt.to_to_string Capnp_rpc.Error.pp e)
  | Ok x -> x
