open Eio.Std
open Astring

module Log = Capnp_rpc.Debug.Log

let () = Mirage_crypto_rng_lwt.initialize (module Mirage_crypto_rng.Fortuna)

module CapTP = Vat_network.CapTP
module Vat = Vat_network.Vat
module Network = Network
module Vat_config = Vat_config
module File_store = File_store
module Sturdy_ref = Capnp_rpc_lwt.Sturdy_ref

let error fmt =
  fmt |> Fmt.kstr @@ fun msg ->
  Error (`Msg msg)

let parse_uri s =
  match Uri.of_string s with
  | exception ex -> error "Failed to parse URI %S: %a" s Fmt.exn ex
  | uri ->
    match Network.Address.parse_uri uri with
    | Ok _ -> Ok uri    (* (just check it parses) *)
    | Error _ as e -> e

module Cap_file = struct
  let load_uri path =
    try
      let ch = open_in_bin path in
      let len = in_channel_length ch in
      let data = really_input_string ch len in
      close_in ch;
      parse_uri (String.trim data)
    with ex ->
      if Sys.file_exists path then
        error "Error loading %S: %a" path Fmt.exn ex
      else
        error "File %S does not exist" path

  let load vat path =
    match load_uri path with
    | Ok uri -> Vat.import vat uri
    | Error _ as e -> e

  let save_uri uri path =
    try
      let data = Uri.to_string uri ^ "\n" in
      let oc = open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o600 path in
      output_string oc data;
      close_out oc;
      Ok ()
    with ex ->
      error "Error saving to %S: %a" path Fmt.exn ex

  let save_sturdy vat sr path =
    save_uri (Vat.export vat sr) path

  let save_service vat id path =
    let uri = Vat.sturdy_uri vat id in
    save_uri uri path
end

let sturdy_uri =
  let of_string s =
    if String.is_prefix s ~affix:"capnp://" then parse_uri s
    else if Sys.file_exists s then Cap_file.load_uri s
    else error "Expected a URI starting with \"capnp://\" \
                or the path to a file containing such a URI, but got %S." s
  in
  Cmdliner.Arg.conv (of_string, Uri.pp_hum)

module Console = struct
  (* The first item in this list is what is currently displayed on screen *)
  let messages = ref []

  let clear () =
    match !messages with
    | [] -> ()
    | msg :: _ ->
      let blank = Stdlib.String.make (String.length msg) ' ' in
      Printf.fprintf stderr "\r%s\r%!" blank

  let show () =
    match !messages with
    | [] -> ()
    | msg :: _ ->
      prerr_string msg;
      flush stderr

  let with_msg msg f =
    clear ();
    messages := msg :: !messages;
    show ();
    Fun.protect f
      ~finally:(fun () ->
         clear ();
         let rec remove_first = function
           | [] -> assert false
           | x :: xs when x = msg -> xs
           | x :: xs -> x :: remove_first xs
         in
         messages := remove_first !messages;
         show ()
      )
end

let addr_of_sr sr =
  match Capnp_rpc_net.Capnp_address.parse_uri (Capnp_rpc_lwt.Cast.sturdy_to_raw sr)#to_uri_with_secrets with
  | Ok ((addr, _auth), _id) -> addr
  | Error (`Msg m) -> failwith m

let rec connect_with_progress ?(mode=`Auto) sr =
  let pp = Fmt.using addr_of_sr Capnp_rpc_net.Capnp_address.Location.pp in
  match mode with
  | `Auto
  | `Log ->
    let did_log = ref false in
    Log.info (fun f -> did_log := true; f "Connecting to %a..." pp sr);
    if !did_log then (
      match Sturdy_ref.connect sr with
      | Ok _ as x -> Log.info (fun f -> f "Connected to %a" pp sr); x
      | Error _ as e -> e
    ) else (
      if Unix.(isatty stderr) then
        connect_with_progress ~mode:`Console sr
      else
        connect_with_progress ~mode:`Batch sr
    )
  | `Batch ->
    Fmt.epr "Connecting to %a... %!" pp sr;
    begin match Sturdy_ref.connect sr with
      | Ok _ as x -> Fmt.epr "OK@."; x
      | Error _ as x -> Fmt.epr "ERROR@."; x
    end
  | `Console ->
    Switch.run @@ fun sw ->
    let x = Fiber.fork_promise ~sw (fun () -> Sturdy_ref.connect sr) in
    Fiber.first
      (fun () -> Promise.await_exn x)
      (fun () ->
        Eio_unix.sleep 0.5;
        Console.with_msg (Fmt.str "[ connecting to %a ]" pp sr)
          (fun () -> Promise.await_exn x)
      )
  | `Silent -> Sturdy_ref.connect sr

let with_cap_exn ?progress sr f =
  match connect_with_progress ?mode:progress sr with
  | Error ex -> Fmt.failwith "%a" Capnp_rpc.Exception.pp ex
  | Ok x -> Capnp_rpc_lwt.Capability.with_ref x f

let handle_connection ?tags ~secret_key vat client =
  match Network.accept_connection ~secret_key client with
  | Error (`Msg msg) ->
    Log.warn (fun f -> f ?tags "Rejecting new connection: %s" msg)
  | Ok ep ->
    let _ : CapTP.t = Vat.add_connection vat ~mode:`Accept ep in
    ()

let create_server ?tags ?restore ~sw ~net config =
  let {Vat_config.backlog; secret_key = _; serve_tls; listen_address; public_address} = config in
  let vat =
    let auth = Vat_config.auth config in
    let secret_key = lazy (fst (Lazy.force config.secret_key)) in
    Vat.create ?tags ?restore ~sw ~address:(public_address, auth) ~secret_key net
  in
  let socket =
    match listen_address with
    | `Unix _ as addr -> Eio.Net.listen ~sw ~backlog ~reuse_addr:true net addr
    | `TCP (host, port) ->
      let addr = Network.addr_of_host host in
      let socket = Eio.Net.listen ~sw ~backlog ~reuse_addr:true net (`Tcp (addr, port)) in
      let unix_socket = Eio_unix.Resource.fd_opt socket |> Option.get in
      Eio_unix.Fd.use_exn "keep-alive" unix_socket @@ fun unix_socket ->
      Unix.setsockopt unix_socket Unix.SO_KEEPALIVE true;
      Keepalive.try_set_idle unix_socket 60;
      socket
  in
  Log.info (fun f -> f ?tags "Waiting for %s connections on %a"
                (if serve_tls then "(encrypted)" else "UNENCRYPTED")
                Vat_config.Listen_address.pp listen_address);
  vat, socket

let listen ?tags ~sw (config, vat, socket) =
  while true do
    let client, addr = Eio.Net.accept ~sw socket in
    Log.info (fun f -> f ?tags "Accepting new connection from %a" Eio.Net.Sockaddr.pp addr);
    let secret_key = if config.Vat_config.serve_tls then Some (Vat_config.secret_key config) else None in
    Fiber.fork ~sw (fun () ->
        (* We don't use [Net.accept_fork] here because this returns immediately after connecting. *)
        handle_connection ?tags ~secret_key vat client
      )
  done

let serve ?tags ?restore ~sw ~net config =
  let net = (net :> [`Generic] Eio.Net.ty r) in
  let (vat, socket) = create_server ?tags ?restore ~sw ~net config in
  Fiber.fork ~sw (fun () ->
      listen ?tags ~sw (config, vat, socket)
    );
  vat

let client_only_vat ?tags ?restore ~sw net =
  let net = (net :> [`Generic] Eio.Net.ty r) in
  let secret_key = lazy (Capnp_rpc_net.Auth.Secret_key.generate ()) in
  Vat.create ?tags ?restore ~secret_key ~sw net

let manpage_capnp_options = Vat_config.docs
