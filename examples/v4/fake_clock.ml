open Eio.Std

(* We don't want delays while running the tests, so we replace the clock with this fake one. *)
let v =
  object
    inherit Eio.Time.clock
    method sleep_until _ = Fiber.yield ()
    method now = 0.0
  end
