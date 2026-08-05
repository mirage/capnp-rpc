module Fmt = struct
  include CCFormat
  let exn = of_to_string Printexc.to_string
end

(** Thread pool for managing concurrency *)
module ThreadPool = CCPool.Make(struct
    let max_size = 16
    end)

module Fut = ThreadPool.Fut

module SyncPoint : sig
  type t
  val create : unit -> t
  val wait : t -> unit
  val signal : t -> unit
end = struct
  type t = {
    mutex: Mutex.t;
    condition: Condition.t;
  }
  let create() = {
    mutex=Mutex.create();
    condition=Condition.create();
  }

  let with_lock_ self f =
    Mutex.lock self.mutex;
    CCFun.finally1 f self ~h:(fun () -> Mutex.unlock self.mutex)

  let wait self =
    with_lock_ self (fun self -> Condition.wait self.condition self.mutex)

  let signal self =
    with_lock_ self (fun self -> Condition.broadcast self.condition)
end

module MVar : sig
  type 'a t
  val create : unit -> 'a t * ('a -> unit)
  val wait : 'a t -> 'a
end = struct
  type 'a t = {
    mutable x: 'a option;
    sync: SyncPoint.t;
  }
  let rec wait self = match self.x with
    | Some v -> v
    | None ->
      SyncPoint.wait self.sync;
      wait self
  let create() =
    let self = { x=None; sync=SyncPoint.create() } in
    self, (fun v ->
        match self.x with
        | None ->
          self.x <- Some v;
          SyncPoint.signal self.sync
        | Some _ -> invalid_arg "MVar: already resolved"
      )
end
