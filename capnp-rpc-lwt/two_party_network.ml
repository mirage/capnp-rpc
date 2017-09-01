module Types = struct
  type provision_id
  type recipient_id
  type third_party_cap_id = [`Two_party_only]
  type join_key_part
end

module Address = struct
  type t

  let pp f _ = Fmt.string f "<two-party-address>"

  let to_uri _ = assert false
  let parse_uri _ = failwith "Can't use of_uri wih Two_party_network"

  let equal _ _ = assert false
end

let parse_third_party_cap_id _ = `Two_party_only

let connect ~secret_key:_ _ = assert false
