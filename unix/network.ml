module Types = struct
  type provision_id
  type recipient_id
  type third_party_cap_id = [`Two_party_only]
  type join_key_part
end

let parse_third_party_cap_id _ = `Two_party_only

module Address = struct
  type t = [
    | `Unix of string
  ]
end
