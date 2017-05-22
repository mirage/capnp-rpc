module S = S
module RO_array = RO_array
module Stats = Stats
module Id = Id
module Debug = Debug
module Error = Error

module Make (C : S.CONCRETE) (N : S.NETWORK_TYPES) = struct
  module Core_types = Core_types.Make(C)
  module Local_struct_promise = Local_struct_promise.Make(Core_types)
  module Protocol = Protocol.Make(Core_types)(N)
  module CapTP = CapTP.Make(Core_types)(N)
end
