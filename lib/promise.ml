type call_handler =
  Xform.t list ->
  Rpc.req_msg ->
  Rpc.value Ro_array.t ->
  Rpc.value * (Rpc.resp_msg * Rpc.value Ro_array.t) Lwt.t

type state =
  | Settled of Rpc.value
  | Unsettled of call_handler * unit Lwt_condition.t

let rec apply xs (underlying : #Rpc.value) : Rpc.value =
  object (_ : Rpc.value)
    method apply ys = apply (xs @ ys) underlying
    method call ys c caps = underlying#call (xs @ ys) c caps
  end

class promise (init : call_handler) =
  object (self)
    val mutable state = Unsettled (init, Lwt_condition.create ())

    method apply xs =
      match state with
      | Settled x -> x#apply xs
      | Unsettled _ -> apply xs self

    method call xs c caps =
      match state with
      | Settled handler -> handler#call xs c caps
      | Unsettled (handle, _) -> handle xs c caps

    method set_handler h =
      match state with
      | Settled _ -> failwith "Already settled!"
      | Unsettled (_, cond) ->
        state <- Unsettled (h, cond);
        Lwt_condition.broadcast cond ()

    method resolve h =
      match state with
      | Settled _ -> failwith "Already settled!"
      | Unsettled (_, cond) ->
        state <- Settled h;
        Lwt_condition.broadcast cond ()

    method await_new_handler =
      match state with
      | Settled _ -> failwith "Already settled!"  (* (not sure what the best response is here) *)
      | Unsettled (_, cond) -> Lwt_condition.wait cond
  end

let broken msg =
  object (self : Rpc.value)
    method apply _ = self
    method call _ _ _ = self, Lwt.fail_with msg
  end
