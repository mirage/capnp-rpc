(** Handle references that got GC'd with a non-zero ref-count.

    If an application forgets to release a resource and it gets GC'd then we want to
    log a warning and clean up (so forgotten refs don't build up over time).

    Because GC finalizers can run at any time and from any thread,
    we need to pass the cleanup callback to a fiber running in the owning thread. *)

val run : unit -> 'a
(** [run ()] registers a leak handler for the current thread and
    runs a loop that waits for callbacks and runs them.
    If the fiber is cancelled, the handler is removed.

    Each vat runs this in a daemon fiber.
    It is safe to have multiple such fibers running in a single systhread. *)

val ref_leak_detected : int -> (unit -> unit) -> unit
(** [ref_leak_detected thread_id fn] should be called from a GC finalizer if
    the resource was not properly released.

    If a handler for [thread_id] is running (see {!run}) then it will schedule
    [fn] to run at a safe point in that thread. If not, [fn] is ignored. *)
