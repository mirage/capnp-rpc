open Astring
open Lwt.Infix

module Log = Capnp_rpc.Debug.Log
module Unix_flow = Unix_flow

let () = Mirage_crypto_rng_lwt.initialize ()

type flow = Unix_flow.flow

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
    Lwt.finalize f
      (fun () ->
         clear ();
         let rec remove_first = function
           | [] -> assert false
           | x :: xs when x = msg -> xs
           | x :: xs -> x :: remove_first xs
         in
         messages := remove_first !messages;
         show ();
         Lwt.return_unit
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
      Sturdy_ref.connect sr >|= function
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
    begin Sturdy_ref.connect sr >|= function
      | Ok _ as x -> Fmt.epr "OK@."; x
      | Error _ as x -> Fmt.epr "ERROR@."; x
    end
  | `Console ->
    let x = Sturdy_ref.connect sr in
    Lwt.choose [Lwt_unix.sleep 0.5; Lwt.map ignore x] >>= fun () ->
    if Lwt.is_sleeping x then (
      Console.with_msg (Fmt.str "[ connecting to %a ]" pp sr)
        (fun () -> x)
    ) else x
  | `Silent -> Sturdy_ref.connect sr

let with_cap_exn ?progress sr f =
  connect_with_progress ?mode:progress sr >>= function
  | Error ex -> Fmt.failwith "%a" Capnp_rpc.Exception.pp ex
  | Ok x -> Capnp_rpc_lwt.Capability.with_ref x f

let handle_connection ?tags ~secret_key vat client =
  Lwt.catch (fun () ->
      let switch = Lwt_switch.create () in
      let raw_flow = Unix_flow.connect ~switch client in
      Network.accept_connection ~switch ~secret_key raw_flow >>= function
      | Error (`Msg msg) ->
        Log.warn (fun f -> f ?tags "Rejecting new connection: %s" msg);
        Lwt.return_unit
      | Ok ep ->
        Vat.add_connection vat ~switch ~mode:`Accept ep >|= fun (_ : CapTP.t) ->
        ()
    )
    (fun ex ->
       Log.err (fun f -> f "Uncaught exception handling connection: %a" Fmt.exn ex);
       Lwt.return_unit
    )

let addr_of_host host =
  match Unix.gethostbyname host with
  | exception Not_found ->
    Capnp_rpc.Debug.failf "Unknown host %S" host
  | addr ->
    if Array.length addr.Unix.h_addr_list = 0 then
      Capnp_rpc.Debug.failf "No addresses found for host name %S" host
    else
      addr.Unix.h_addr_list.(0)

let serve ?switch ?tags ?restore config =
  let {Vat_config.backlog; secret_key = _; serve_tls; listen_address; public_address} = config in
  let vat =
    let auth = Vat_config.auth config in
    let secret_key = lazy (fst (Lazy.force config.secret_key)) in
    Vat.create ?switch ?tags ?restore ~address:(public_address, auth) ~secret_key ()
  in
  let socket =
    match listen_address with
    | `Unix path ->
      begin match Unix.lstat path with
        | { Unix.st_kind = Unix.S_SOCK; _ } -> Unix.unlink path
        | _ -> ()
        | exception Unix.Unix_error(Unix.ENOENT, _, _) -> ()
      end;
      let socket = Unix.(socket PF_UNIX SOCK_STREAM 0) in
      Unix.bind socket (Unix.ADDR_UNIX path);
      socket
    | `TCP (host, port) ->
      let socket = Unix.(socket PF_INET SOCK_STREAM 0) in
      Unix.setsockopt socket Unix.SO_REUSEADDR true;
      Unix.setsockopt socket Unix.SO_KEEPALIVE true;
      Keepalive.try_set_idle socket 60;
      Unix.bind socket (Unix.ADDR_INET (addr_of_host host, port));
      socket
  in
  Unix.listen socket backlog;
  Log.info (fun f -> f ?tags "Waiting for %s connections on %a"
                (if serve_tls then "(encrypted)" else "UNENCRYPTED")
                Vat_config.Listen_address.pp listen_address);
  let lwt_socket = Lwt_unix.of_unix_file_descr socket in
  let rec loop () =
    Lwt_switch.check switch;
    Lwt_unix.accept lwt_socket >>= fun (client, _addr) ->
    Log.info (fun f -> f ?tags "Accepting new connection");
    let secret_key = if serve_tls then Some (Vat_config.secret_key config) else None in
    Lwt.async (fun () -> handle_connection ?tags ~secret_key vat client);
    loop ()
  in
  Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
           let th = loop () in
           Lwt_switch.add_hook switch (fun () -> Lwt.cancel th; Lwt.return_unit);
           th
        )
        (function
          | Lwt.Canceled -> Lwt.return_unit
          | ex -> Lwt.fail ex
        )
      >>= fun () ->
      Lwt_unix.close lwt_socket
    );
  Lwt.return vat

let client_only_vat ?switch ?tags ?restore () =
  let secret_key = lazy (Capnp_rpc_net.Auth.Secret_key.generate ()) in
  Vat.create ?switch ?tags ?restore ~secret_key ()

let manpage_capnp_options = Vat_config.docs
