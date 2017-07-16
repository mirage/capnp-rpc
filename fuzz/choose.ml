exception End_of_fuzz_data

let int limit =
  try
    let x = Char.code (input_char stdin) in
    if limit < 0x100 then x mod limit
    else (
      let y = Char.code (input_char stdin) in
      assert (limit < 0x10000);
      (x lor (y lsl 8)) mod limit
    )
  with End_of_file -> raise End_of_fuzz_data

let array options =
  options.(int (Array.length options))

let bool () =
  Char.code (input_char stdin) land 1 = 1
