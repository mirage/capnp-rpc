open Capnp_rpc_lwt

module S = S

module Endpoint = Endpoint
module Restorer = Restorer

module type VAT_NETWORK = S.VAT_NETWORK with
  type 'a capability := 'a Capability.t and
  type restorer := Restorer.t and
  type service_id := Restorer.Id.t and
  type 'a sturdy_ref := 'a Sturdy_ref.t

module Networking (N : S.NETWORK) = struct
  module Network = N
  module Vat = Vat.Make (N)
  module CapTP = Vat.CapTP
end

module Capnp_address = Capnp_address
module Persistence = Persistence
module Two_party_network = Two_party_network
module Auth = Auth
module Tls_wrapper = Tls_wrapper
