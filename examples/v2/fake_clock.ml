(* We don't want delays while running the tests, so we replace the clock with this fake one. *)

open Eio.Std

module Impl = struct
  type t = unit
  type time = float
  let sleep_until () _ = Fiber.yield ()
  let now () = 0.0
end

let v : float Eio.Time.clock_ty r =
  Eio.Resource.T ((), Eio.Time.Pi.clock (module Impl))
