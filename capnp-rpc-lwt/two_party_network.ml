(** A network where the is only one other addressable party. *)

module Types = struct
  type sturdy_ref
  type provision_id
  type recipient_id
  type third_party_cap_id = [`Two_party_only]
  type join_key_part
end

let parse_third_party_cap_id _ = `Two_party_only
