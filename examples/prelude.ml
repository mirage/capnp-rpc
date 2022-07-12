(* Pre-build demos to avoid capnp compiler warnings about PWD *)

#require "unix";;

let examples = [
  "v1";
  "v2";
  "v3";
  "v4";
  "pipelining";
  "sturdy-refs";
  "sturdy-refs-2";
  "sturdy-refs-3";
  "sturdy-refs-4";
]

let () =
  examples |> List.iter (fun ex ->
      let cmd = Printf.sprintf "cd 'examples/%s' && dune build ./main.exe" ex in
      ignore (Unix.system cmd : Unix.process_status)
    )
