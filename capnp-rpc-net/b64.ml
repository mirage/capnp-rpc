let error fmt =
  fmt |> Fmt.kstr @@ fun msg ->
  Error (`Msg msg)

let encode ~alphabet ~pad s =
  match Base64.encode ~alphabet ~pad s with
  | Ok x -> x
  | Error (`Msg m) -> failwith m    (* Encoding can't really fail *)

let decode ~alphabet ~pad s =
  match Base64.decode ~alphabet ~pad s with
  | Ok _ as x -> x
  | Error (`Msg m) -> Error (`Msg m)  (* Re-add polymorphism *)
  | exception ex -> error "Bad base64 digest %S: %a" s Fmt.exn ex
