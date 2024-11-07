module S = S
module RO_array = RO_array
module Stats = Stats
module Id = Id
module Debug = Debug
module Error = Error
module Exception = Exception
module Core_types(C : S.WIRE) = Core_types.Make(C)
module Local_struct_promise = Local_struct_promise
module Cap_proxy = Cap_proxy

module Message_types = Message_types
module CapTP = CapTP
module RC = RC
