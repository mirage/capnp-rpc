open Asetmap

module Log = Debug.Log
module IntMap = Map.Make(struct type t = int let compare (a:int) b = compare a b end)

let rec filter_map f = function
  | [] -> []
  | x :: xs ->
    match f x with
    | None -> filter_map f xs
    | Some y -> y :: filter_map f xs

let pp_check check f (k, v) =
  try check k v
  with ex ->
    Fmt.pf f "@,[%a] %a"
      Fmt.(styled `Red string) "ERROR"
      Debug.pp_exn ex

let pp_weak f = function
  | None -> Fmt.pf f "(GC'd weak pointer)"
  | Some x -> x#pp f

let hashtbl_dump ~key pp f items =
  let add k v results = (k, v) :: results in
  Hashtbl.fold add items []
  |> List.sort (fun a b -> compare (key (snd a)) (key (snd b)))
  |> Fmt.Dump.list pp f

module EmbargoId = Message_types.EmbargoId

module Make (EP : Message_types.ENDPOINT) = struct
  module Core_types = EP.Core_types
  module Wire = Core_types.Wire
  module Cap_proxy = Cap_proxy.Make(Core_types)
  module Struct_proxy = Struct_proxy.Make(Core_types)
  module Local_struct_promise = Local_struct_promise.Make(Core_types)
  module Out = EP.Out
  module In = EP.In

  let inc_ref = Core_types.inc_ref
  let dec_ref = Core_types.dec_ref

  let with_inc_ref x =
    Core_types.inc_ref x;
    x

  open EP.Table

  module PathSet = Set.Make(Wire.Path)
  module Embargoes = Table.Allocating(EmbargoId)

  (* Maps used cap indexes to their paths *)
  let caps_used ~msg paths_used =
    PathSet.elements paths_used
    |> filter_map (fun path ->
        match Wire.Response.cap_index msg path with
        | None -> None
        | Some i -> Some (i, path)
      )
    |> IntMap.of_list

  module Questions = Table.Allocating(QuestionId)
  module Answers = Table.Tracking(AnswerId)
  module Exports = Table.Allocating(ExportId)
  module Imports = Table.Tracking(ImportId)

  module Import = struct
    (* An entry in the imports table has a corresponding switchable, which the user of the library holds.
       There are three possible events:

       - The user reduces the switchable's ref-count to zero, indicating that
         they don't need the import any longer.

       - The peer resolves the import to something else (or the connection is
         lost, resolving it to an exception).

       - The peer quotes the same import ID again.
         If we hadn't sent a release before this, it must be for the same object.
         If we had sent a release then it might or might not be the same object.

       We hold a weak-ref to the switchable so that if the user leaks it we will notice.

       [ref_count] is zero iff [count] is zero:
       - Initially, both are one.
       - If we set [ref_count] to zero, we send a release and set [count] to zero.
       - There is no other way for [count] to become zero.
     *)

    type t = {
      id : ImportId.t;
      mutable ref_count : RC.t; (* The switchable holds one until resolved, plus each [resolve_target] adds one. *)
      mutable count : int;     (* Number of times remote sent us this. *)
      mutable used : bool;     (* We have sent a message to this target (embargo needed on resolve). *)
      mutable settled : bool;  (* This was a SenderHosted - it can't resolve except to an exception. *)
      mutable resolution : disembargo_info;
      proxy : Cap_proxy.resolver_cap Weak_ptr.t; (* Our switchable ([Weak_ptr.t] is mutable). *)
      strong_proxy : < > option ref;    (* Keeps the switchable alive if there are callbacks registered. *)
    }
    and disembargo_info = [
      | `Unresolved
      | `Local
      | `Import of t       (* Holds ref *)
      | `Error
    ]

    let id t = t.id

    let pp f t =
      Fmt.pf f "i%a" ImportId.pp t.id

    let dump f t =
      Fmt.pf f "%a -> %a"
        RC.pp t.ref_count
        pp_weak (Weak_ptr.get t.proxy)

    let get_proxy t = Weak_ptr.get t.proxy

    (* A new export from the peer. *)
    let inc_count t =
      assert (t.count > 0);
      t.count <- t.count + 1

    (* A new local reference (resolve_target). *)
    let inc_ref t =
      assert (t.count > 0);
      let pp f = pp f t in
      t.ref_count <- RC.succ ~pp t.ref_count

    let clear_proxy t =
      Weak_ptr.clear t.proxy

    (* A [resolve_target] or our switchable no longer needs us. *)
    let rec dec_ref t =
      let pp f = pp f t in
      t.ref_count <- RC.pred ~pp t.ref_count;
      if RC.is_zero t.ref_count then (
        assert (t.count > 0);
        let count = t.count in
        t.used <- false;
        t.count <- 0;
        let free_resolution =
          match t.resolution with
          | `Import i -> dec_ref i
          | `Unresolved | `Local | `Error -> []
        in
        `Release (t, count) :: free_resolution
      ) else []

    let mark_used t =
      t.used <- true

    let used t = t.used

    let mark_resolved t ~get_import result =
      if t.resolution <> `Unresolved then
        Debug.failf "Got Resolve for already-resolved import %a" pp t
      else match result with
        | Error _ -> t.resolution <- `Error
        | Ok desc ->
          let info =
            match desc with
            | `None -> `Error           (* Probably shouldn't happen *)
            | `Error _ -> `Error        (* Don't need to embargo errors *)
            | `SenderPromise id | `SenderHosted id -> `Import (get_import id)
            | `ThirdPartyHosted _ -> failwith "todo: disembargo_reply: ThirdPartyHosted"
            | `ReceiverAnswer _ | `ReceiverHosted _ -> `Local
          in
          t.resolution <- info

    let message_target t =
      `ReceiverHosted t.id

    let embargo_path t =
      if t.used then Some (message_target t) else None

    (* We have a reference pointing at this target and may need to perform an embargo.
       This could be either because we pipelined messages to this import, or we pipelined
       messages to another remote object which we know forwarded them to this one. *)
    let disembargo_target t =
      match t.resolution with
      | `Unresolved -> None         (* No embargoes needed yet *)
      | `Error -> None        (* Don't need to embargo errors *)
      | `Import _ as i -> Some i
      | `Local ->
        (* The import resolved to a local service. Send a loopback disembargo to flush the path. *)
        Some (message_target t)

    let init_proxy t proxy =
      assert (get_proxy t = None);
      Weak_ptr.set t.proxy proxy

    let check t =
      let pp f = pp f t in
      RC.check ~pp t.ref_count;
      (* Count starts at one and is only incremented, or set to zero when ref_count is zero. *)
      if t.count < 1 then
        Debug.invariant_broken (fun f -> Fmt.pf f "Import remote count < 1, but still in table: %a" dump t);
      match get_proxy t with
      | Some x -> x#check_invariants
      | None -> ()

    let lost_connection t ~broken_cap =
      match get_proxy t with
      | Some switchable when switchable#problem = None -> switchable#resolve broken_cap
      | _ -> ()

    let v ~mark_dirty ~settled id = {
      ref_count = RC.one;
      count = 1;
      id = id;
      proxy = Weak_ptr.empty ();
      strong_proxy = ref None;
      settled;
      resolution = `Unresolved;
      used = mark_dirty;
    }
  end

  module Question = struct
    (* State model for questions:

       1. waiting: returned=false, rc>0 (initial state)
          We're waiting for some results. (Initially rc=1, from our remote promise)
          On rc=0: send Finish, goto cancelled
          On return: goto lingering

       2. cancelled: returned=false, rc=0
          We told the peer we don't care about the results.
          It may return them anyway or return "cancelled".
          We will never send another message referring to this question.
          On return: goto complete (with results=cancelled)

       3. lingering: returned=true, rc>0
          We've got the results and want to finish, but we can't yet because rc>0.
          On rc=0: send Finish, goto complete

       4: complete: returned=true, rc=0 (final state)
          Finish has been sent and we are no longer in the questions table.
          We will never send or receive another message referring to this question.

       The user releases the remote promise to indicate that they no longer care
       about the result. The remote promise will then decrement rc.

       The "return" event means the peer sent us a Return message.

       rc>0 means that we may still want to send messages about this question.
       This could be either messages sent via the remote promise, or disembargo
       replies.

       The disembargo case happens when e.g. we told the peer that a capability
       in the answer to a question they asked us will come from this question
       that we asked them.

       Initially, our [answer] for the peer's question holds a reference to us
       via our remote promise so that it can forward any calls. This will
       prevent us from reaching rc=0. Once we get the results, the answer will
       update to point directly at the new target and the remote promise may be
       released then.

       However, the peer may still need to send a disembargo request to the
       answer, which must be sent back to its *original* target (this
       questions's answer). For this case, we must keep our question alive a
       little longer, until the peer finishes their question.

       We expect this to happen soon because there's no point in the peer
       holding on to their question now that they've got the answer.

       Every path contains exactly one Finish, exactly one Return, and exactly
       one release (rc=0) event.
     *)

    type disembargo_info = [
      | `Error
      | `Elsewhere
      | `Local
      | `Results of Wire.Response.t * [
            | `Local
            | `Import of Import.t       (* Holds ref *)
            | `None
          ] RO_array.t
    ]

    type state =
      | Waiting
      | Cancelled
      | Lingering of disembargo_info
      | Complete

    type t = {
      id : QuestionId.t;
      mutable remote_promise : [`Resolver of Core_types.struct_resolver | `Released];
      mutable ref_count : RC.t;
      mutable state : state;
      params_for_release : ExportId.t list; (* For level 0 peers *)
      mutable pipelined_fields : PathSet.t; (* Fields used while unresolved; will need embargoes *)
    }

    let id t = t.id

    let pp f q =
      Fmt.pf f "q%a" QuestionId.pp q.id

    let pp_disembargo_desc f = function
      | `Local -> Fmt.string f "local"
      | `Import i -> Import.pp f i
      | `None -> Fmt.string f "none"

    let pp_disembargo_info f : disembargo_info -> unit = function
      | `Error -> Fmt.string f "error"
      | `Elsewhere -> Fmt.string f "elsewhere"
      | `Local -> Fmt.string f "local"
      | `Results (_, descs) ->
        (RO_array.pp pp_disembargo_desc) f descs

    let free_disembargo_info = function
      | `Error -> []
      | `Elsewhere -> []
      | `Local -> []
      | `Results (_, descs) ->
        RO_array.fold_left (fun acc -> function
            | `None | `Local -> acc
            | `Import i -> `Release_import i :: acc
          ) [] descs

    let pp_promise f q =
      match q.remote_promise with
      | `Released -> Fmt.string f "(released)"
      | `Resolver p -> Fmt.pf f "%t" p#pp

    let pp_state f = function
      | Waiting     -> Fmt.string f "waiting"
      | Cancelled   -> Fmt.string f "cancelled"
      | Lingering i -> Fmt.pf f "lingering:%a" pp_disembargo_info i
      | Complete    -> Fmt.string f "complete"

    let dump f t =
      Fmt.pf f "(%a) %a" pp_state t.state pp_promise t

    let sent_finish t =
      match t.state with
      | Waiting | Lingering _ -> false
      | Cancelled | Complete -> true

    let inc_ref t =
      assert (not (sent_finish t));
      let pp f = pp f t in
      t.ref_count <- RC.succ t.ref_count ~pp

    let dec_ref t =
      let pp f = pp f t in
      t.ref_count <- RC.pred t.ref_count ~pp;
      if RC.is_zero t.ref_count then match t.state with
        | Waiting -> t.state <- Cancelled; [`Send_finish]
        | Lingering ret -> t.state <- Complete; `Send_finish :: `Release_table_entry :: free_disembargo_info ret
        | Complete -> []        (* (only happens with lost_connection) *)
        | Cancelled -> failwith "Can't hold refs while cancelled!"
      else
        []

    let release_proxy t =
      t.remote_promise <- `Released;
      dec_ref t

    let set_cap_used t path =
      t.pipelined_fields <- PathSet.add path t.pipelined_fields

    let cap_used t path =
      PathSet.mem path t.pipelined_fields

    let message_target t path = `ReceiverAnswer (t.id, path)

    (* Extract some useful parts of the response that we may need for embargoes. *)
    let extract_resolution ~get_import = function
      | #Error.t -> `Error
      | `ResultsSentElsewhere -> `Elsewhere (* We don't care about the result, so should never embargo *)
      | `TakeFromOtherQuestion _ -> `Local  (* Remote will forward back to our answer *)
      | `AcceptFromThirdParty -> failwith "todo: answer_cap_needs_disembargo: AcceptFromThirdParty"
      | `Results (msg, descs) ->
        let extract_cap = function
          | `ReceiverAnswer _ | `ReceiverHosted _ -> `Local (* Remote will forward back to us *)
          | `SenderPromise id | `SenderHosted id -> `Import (get_import id) (* Disembargo via import *)
          | `ThirdPartyHosted _ -> failwith "todo: answer_cap_needs_disembargo: ThirdPartyHosted"
          | `None -> `None
        in
        `Results (msg, RO_array.map extract_cap descs)

    (* Something resolved to a promised answer, and we sent a disembargo (because the promise was local).
       When the disembargo response arrived, the local promise had resolved to this question.
       Do we need another embargo? Return the path down which to send the disembargo request, if so. *)
    let answer_cap_disembargo t path =
      if cap_used t path then (
        match t.state with
        | Waiting | Cancelled -> None  (* Can't disembargo if not returned *)
        | Complete -> failwith "Already finished!" (* Previous disembargo should have kept us alive *)
        | Lingering ret ->
          match ret with
          | `Error -> None
          | `Results (msg, descs) ->
            (* If the answer was local, we need to embargo. *)
            begin match Core_types.Wire.Response.cap_index msg path with
              | None -> None
              | Some i ->
                match RO_array.get ~oob:`None descs i with
                | `Local -> Some (message_target t path)
                | `Import i -> Some (`Import i)
                | `None -> None
            end;
          | `Elsewhere -> None      (* We don't care about the result (except for forwarding) *)
          | `Local -> Some (message_target t path)
      ) else None       (* Not used, so no embargo needed *)

    (* A [Return] message has arrived. *)
    let return t ret =
      match t.state with
      | Waiting   -> t.state <- Lingering ret; []
      | Cancelled -> t.state <- Complete; `Release_table_entry :: free_disembargo_info ret
      | Lingering _
      | Complete  -> failwith "Already returned!"

    let paths_used t =
      t.pipelined_fields

    let check t =
      let pp f = pp f t in
      match t.state with
      | Waiting | Lingering _ -> RC.check ~pp t.ref_count (* rc > 0 *)
      | Cancelled -> assert (RC.is_zero t.ref_count)
      | Complete -> Debug.invariant_broken (fun f -> Fmt.pf f "Complete question %a still in table!" dump t)

    let lost_connection t ~ex =
      (* The resolve might get a "release" event, but we'll ignore it in the Complete state.
         We don't care about the final state, as the whole table is being dropped anyway. *)
      t.state <- Complete;
      match t.remote_promise with
      | `Released -> ()
      | `Resolver p -> Core_types.resolve_payload p (Error (`Exception ex))

    let resolve t payload =
      match t.remote_promise with
      | `Resolver p -> Core_types.resolve_payload p payload
      | `Released ->
        match payload with
        | Ok payload -> Core_types.Response_payload.release payload
        | Error _ -> ()

    let v ~params_for_release ~remote_promise id =
      {
        id;
        remote_promise = `Resolver remote_promise;
        ref_count = RC.one; (* Held by [remote_promise] *)
        state = Waiting;
        params_for_release;
        pipelined_fields = PathSet.empty
      }
  end

  type message_target_cap = [
    | `Import of Import.t
    | `QuestionCap of Question.t * Wire.Path.t
  ]

  let pp_message_target_cap f = function
    | `Import i -> Import.pp f i
    | `QuestionCap (q, p) -> Fmt.pf f "%a[%a]" Question.pp q Wire.Path.pp p

  (* When we answer a question or provide an export, we may tell the peer that the
     resource is local to it. In that case, we need to keep a reference to the remote
     object so that we can forward disembargo requests. *)
  type resolve_target = [
    message_target_cap
    | `Local                    (* Resolved to a local object, not one at the peer. *)
    | `None                     (* This capability is not resolved yet *)
  ]

  module Answer = struct
    (* State model for answers:

       When a call (or bootstrap) arrives, we add a new answer to the answers table.
       It is removed from the table when we receive a Finish message for it.

       While in the table, any of the following may happen:

       - We calculate the answer. We send it back to the peer, and also keep it locally
         so that we can forward any pipelined messages or disembargo requests.

       - We get a Finish from the peer. The question is cancelled and removed from the table.
         We reply with a cancelled message.

       - We tell the peer to take the answer from some question we asked.
         We forward any pipelined messages to the new question until we get a Finish.
         We respond to disembargo requests by sending the response back to the new question.
     *)

    type active = {
      answer : Core_types.struct_ref;

      resolution : [
        | `Unresolved
        | `Resolved of
            resolve_target RO_array.t * (* Targets for disembargo replies and forwarding *)
            ExportId.t list             (* Caps to release if we get releaseResultCaps *)
        | `Forwarded of Question.t      (* We replied with TakeFromOtherQuestion *)
      ];
    }

    type t = {
      id : AnswerId.t;
      mutable state : [
        | `Active of active             (* No Finish message has arrived yet. *)
        | `Finished                     (* We got a Finish. A return has been sent. *)
      ]
    }

    let id t = t.id

    let answer_struct t =
      match t.state with
      | `Finished -> `Finished
      | `Active x -> `Promise x.answer

    let pp f t =
      Fmt.pf f "a%a" AnswerId.pp t.id

    let pp_resolution f = function
      | `Unresolved -> Fmt.string f "(unresolved)"
      | `Resolved _ -> Fmt.string f "(resolved)"
      | `Forwarded q -> Fmt.pf f "(forwarded to %a)" Question.pp q

    let dump f t =
      match t.state with
      | `Finished -> Fmt.pf f "(finished)"
      | `Active {resolution; answer} -> Fmt.pf f "%a %t" pp_resolution resolution answer#pp

    let check t =
      match t.state with
      | `Finished -> failwith "Finished answer still in table!"
      | `Active {answer; resolution} ->
        answer#check_invariants;
        match resolution with
        | `Forwarded q -> Question.check q
        | `Unresolved | `Resolved _ -> ()

    let needs_return t =
      t.state <> `Finished

    (* We're sending a return message. *)
    let return_resolved t ~exports_for_release ~resolve_targets =
      match t.state with
      | `Finished -> Debug.failf "Can't return finished answer %a!" pp t
      | `Active x ->
        assert (x.resolution = `Unresolved);
        t.state <- `Active {x with resolution = `Resolved (resolve_targets, exports_for_release)}

    (* We're sending a TakeFromOtherQuestion return message. *)
    let return_take_from_question t question =
      match t.state with
      | `Finished -> Debug.failf "Can't return finished answer %a!" pp t
      | `Active x ->
        assert (x.resolution = `Unresolved);
        Question.inc_ref question;
        t.state <- `Active {x with resolution = `Forwarded question}

    (* Remove from Answers table after calling this. *)
    let finish t ~release_result_caps =
      match t.state with
      | `Finished -> Debug.failf "Can't finish already-finished answer %a" pp t
      | `Active {answer; resolution} ->
        t.state <- `Finished;
        dec_ref answer;
        match resolution with
        | `Unresolved -> [`Return_cancelled]
        | `Resolved (resolve_targets, exports_for_release) ->
          `Release_resolve_targets resolve_targets :: (
            if release_result_caps then [`Release_exports exports_for_release]
            else []
          )
        | `Forwarded q ->
          begin match q.Question.remote_promise with
            | `Resolver r ->
              (* Notify anyone waiting on the results. *)
              Core_types.resolve_exn r (Exception.v "Results sent elsewhere")
            | `Released -> ()
          end;
          [`Question_actions (q, Question.dec_ref q)]

    let lost_connection t =
      finish ~release_result_caps:true t |> filter_map (function
          | `Question_actions _
          | `Release_resolve_targets _ as action -> Some action
          | `Return_cancelled -> None
          | `Release_exports (_ : ExportId.t list) -> None (* There are local to the connection anyway *)
        )

    (* The target of [path] that we returned as the answer.
       [None] if we didn't return yet. *)
    let resolve_target t path =
      match t.state with
      | `Finished -> Debug.failf "Answer %a is finished!" pp t
      | `Active {answer; resolution} ->
        match resolution with
        | `Unresolved -> None
        | `Forwarded q -> Some (Ok (`QuestionCap (q, path)))
        | `Resolved (resolve_targets, _) ->
          match answer#response with
          | None -> Debug.failf "Answer %a is resolved, but no response recorded!" pp t
          | Some (Error _) as e -> e
          | Some (Ok msg) ->
            match Core_types.Wire.Response.cap_index msg path with
            | Some i when i >= 0 && i < RO_array.length resolve_targets ->
              Some (Ok (RO_array.get_exn resolve_targets i))
            | Some i -> Some (Error (Error.exn "Invalid answer cap index %d" i))
            | None -> Some (Ok `None)

    let disembargo_target t path =
      match resolve_target t path with
      | None -> Debug.failf "Got disembargo request for unresolved answer %a!" pp t
      | Some (Error _) -> failwith "Got disembargo for an exception!"
      | Some (Ok target) -> target

    let create aid ~answer = {
      id = aid;
      state = `Active { answer; resolution = `Unresolved };
    }

    let create_uninitialised aid = {
      id = aid;
      state = `Finished;
    }

    let init t answer =
      match t.state with
      | `Finished -> t.state <- `Active { answer; resolution = `Unresolved }
      | `Active _ -> Debug.failf "Answer %a already initialised!" pp t
  end

  module Export = struct
    type t = {
      id : ExportId.t;
      mutable count : int; (* Number of times sent to remote and not yet released *)
      mutable service : Core_types.cap;
      mutable resolve_target : resolve_target;   (* [`None] if not yet resolved *)
    }

    let dump f t =
      Fmt.pf f "%t" t.service#pp

    let pp f t =
      Fmt.pf f "e%a" ExportId.pp t.id

    let inc_ref t =
      t.count <- t.count + 1

    let id t = t.id

    let count t = t.count

    let resolve_target t = t.resolve_target

    let service t = t.service

    let resolve t target =
      t.resolve_target <- target

    let released = Core_types.broken_cap (Exception.v "(released)")

    let release t ref_count =
      assert (t.count >= ref_count);
      let count = t.count - ref_count in
      t.count <- count;
      if count > 0 then `Do_nothing
      else (
        let service = t.service in
        t.service <- released;
        `Send_release (service, t.resolve_target)
      )

    let lost_connection t =
      dec_ref t.service;
      t.service <- released;
      t.count <- 0

    let check t = t.service#check_invariants

    let v ~service id =
      { count = 1; service; id; resolve_target = `None }
  end

  type descr = [
    message_target_cap
    | `ThirdPartyHosted of Out.third_party_desc
    | `Local of Core_types.cap
  ]

  let pp_cap : [< descr] Fmt.t = fun f -> function
    | `Import import -> Fmt.pf f "%a" Import.pp import
    | `QuestionCap (question, p) -> Fmt.pf f "%a[%a]" Question.pp question Wire.Path.pp p
    | `ThirdPartyHosted _third_party_desc -> Fmt.pf f "ThirdPartyHosted"
    | `Local local -> Fmt.pf f "local:%t" local#pp

  type restorer = ((Core_types.cap, Exception.t) result -> unit) -> string -> unit

  type t = {
    mutable queue_send : (EP.Out.t -> unit);    (* (mutable for shutdown) *)
    tags : Logs.Tag.set;
    embargoes : (EmbargoId.t * Cap_proxy.resolver_cap) Embargoes.t;
    restore : restorer;
    fork : (unit -> unit) -> unit;

    questions : Question.t Questions.t;
    answers : Answer.t Answers.t;
    exports : Export.t Exports.t;
    imports : Import.t Imports.t;
    exported_caps : (Core_types.cap, ExportId.t) Hashtbl.t;

    mutable disconnected : Exception.t option;  (* If set, this connection is finished. *)
  }

  type 'a S.brand += CapTP : (t * message_target_cap) S.brand
  (* The [CapTP] message asks a capability to tell us its CapTP target, if any. *)

  type 'a S.brand += CapTP_results : (t * Answer.t) S.brand
  (* The [CapTP_results] message asks a struct_resolver to tell us if it's for a CapTP answer.
     If so, we may be able to send the results directly. *)

  let target_of (x : #Core_types.cap) = x#shortest#sealed_dispatch CapTP

  let my_target_of t (x : #Core_types.cap) =
    match target_of x with
    | Some (t', target) when t == t' -> Some target
    | _ -> None

  let stats t =
    { Stats.
      n_questions = Questions.active t.questions;
      n_answers = Answers.active t.answers;
      n_imports = Imports.active t.imports;
      n_exports = Exports.active t.exports;
    }

  let default_restore k _object_id =
    k @@ Error (Exception.v "This vat has no restorer")

  let create ?(restore=default_restore) ~tags ~fork ~queue_send =
    {
      queue_send = (queue_send :> EP.Out.t -> unit);
      tags;
      restore = restore;
      fork;
      questions = Questions.make ();
      answers = Answers.make ();
      imports = Imports.make ();
      exports = Exports.make ();
      embargoes = Embargoes.make ();
      exported_caps = Hashtbl.create 30;
      disconnected = None;
    }
  [@@ocaml.warning "-16"]       (* Too late to change the API now. *)

  let with_qid qid t =
    Logs.Tag.add Debug.qid_tag (QuestionId.uint32 qid) t.tags

  let with_aid aid t =
    Logs.Tag.add Debug.qid_tag (EP.Table.AnswerId.uint32 aid) t.tags

  let tags t = t.tags

  let pp_promise f = function
    | Some q -> Question.pp f q
    | None -> Fmt.string f "(not initialised)"

  let check_connected t =
    match t.disconnected with
    | None -> ()
    | Some ex -> Debug.failf "CapTP connection is disconnected (%a)" Exception.pp ex

  module Send : sig
    (** Converts struct pointers into integer table indexes, ready for sending.
        The indexes are only valid until the next message is sent. *)

    open EP.Core_types

    val bootstrap : t -> struct_resolver -> Question.t

    val call : t -> struct_resolver ->
      message_target_cap -> Wire.Request.t ->
      results_to:EP.Out.send_results_to ->
      Question.t

    val return : t -> Answer.t -> Core_types.Response_payload.t Core_types.or_error -> unit

    val release : t -> Import.t -> int -> unit
    (** [release t i count] tells the peer that [i] is no longer needed by us,
        decreasing the ref-count by [count]. *)

    val finish : t -> Question.t -> unit
    (** [finish t q] sends a Finish message (with [releaseResultCaps=false]). *)
  end = struct

    (** We are sending [cap] to the client, either in a Return or Resolve message.
        Return the [resolve_target] for it, and increment any required
        ref-count to keep it alive. *)
    let get_resolve_target t cap =
      match target_of cap with
      | Some (t', target) when t == t' ->
        begin match target with
          | `QuestionCap (question, _) as target ->
            Question.inc_ref question;
            target
          | `Import import as target ->
            Import.inc_ref import;
            target
        end
      | Some _ (* todo: third-party *)
      | None -> `Local

    (** [export ~broken_caps t cap] is a descriptor for [cap].
        If [cap] is a proxy object for a service at the peer, tell the peer the target directly.
        Otherwise, export it to the peer (reusing an existing export, if any).
        If the cap is broken and needs a fresh export, we queue up a suitable resolve message
        on [broken_caps]. This is needed for return messages. *)
    let rec export ?broken_caps : t -> Core_types.cap -> Out.desc = fun t cap ->
      let cap = cap#shortest in
      match my_target_of t cap with
      | Some (`Import import) -> Import.message_target import
      | Some (`QuestionCap (question, i)) -> Question.message_target question i
      | None ->
        let problem = cap#problem in
        (* Exceptions are considered unsettled because we need to resolve them to
           an error later (due to a protocol limitation). *)
        let settled = problem = None && cap#blocker = None in
        let ex =
          match Hashtbl.find t.exported_caps cap with
          | id ->
            let ex = Exports.find_exn t.exports id in
            Export.inc_ref ex;
            ex
          | exception Not_found ->
            Core_types.inc_ref cap;
            let ex = Exports.alloc t.exports (Export.v ~service:cap) in
            let id = Export.id ex in
            Hashtbl.add t.exported_caps cap id;
            begin match problem, broken_caps with
              | Some problem, Some broken_caps -> Queue.add (ex, problem) broken_caps
              | Some _, _ -> failwith "Cap is broken, but [broken_caps] not provided!"
              | None, _ when settled -> ()
              | None, _ ->
                Log.debug (fun f -> f ~tags:t.tags "Monitoring promise export %a -> %a" Export.pp ex Export.dump ex);
                cap#when_more_resolved (fun x ->
                    if Export.count ex > 0 then (
                      let x = x#shortest in
                      match x#problem with
                      | Some problem ->
                        Log.debug (fun f -> f ~tags:t.tags "Export %a resolved to %t - sending exception"
                                      Export.pp ex
                                      x#pp
                                  );
                        t.queue_send (`Resolve (Export.id ex, Error problem));
                      | None ->
                        let new_export = export t x in
                        Log.debug (fun f -> f ~tags:t.tags "Export %a resolved to %t - sending notification to use %a"
                                      Export.pp ex
                                      x#pp
                                      Out.pp_desc new_export
                                  );
                        Export.resolve ex (get_resolve_target t x);
                        t.queue_send (`Resolve (Export.id ex, Ok new_export));
                    ); (* else: no longer an export *)
                    Core_types.dec_ref x
                  )
            end;
            ex
        in
        let id = Export.id ex in
        if settled then `SenderHosted id
        else `SenderPromise id

    let bootstrap t remote_promise =
      check_connected t;
      Questions.alloc t.questions (Question.v ~params_for_release:[] ~remote_promise)

    (* This is for level 0 implementations, which don't understand about releasing caps. *)
    let exports_of =
      RO_array.fold_left (fun acc -> function
          | `SenderPromise id | `SenderHosted id | `ThirdPartyHosted (_, id) -> id :: acc
          | `None | `ReceiverAnswer _ | `ReceiverHosted _ -> acc
        ) []

    (* For some reason, we can't send broken caps in a payload message. So, if
       any of them are broken, we send a resolve for them immediately afterwards. *)
    let resolve_broken t =
      Queue.iter @@ fun (ex, problem) ->
      Log.debug (fun f -> f ~tags:t.tags "Sending resolve for already-broken export %a : %a"
                    Export.pp ex
                    Export.dump ex
                );
      t.queue_send (`Resolve (Export.id ex, Error problem))

    let call t remote_promise (target : message_target_cap) msg ~results_to =
      let broken_caps = Queue.create () in
      let caps = Core_types.Request_payload.snapshot_caps msg in
      let descs = RO_array.map (export ~broken_caps t) caps in
      let question = Questions.alloc t.questions
          (Question.v ~params_for_release:(exports_of descs) ~remote_promise)
      in
      let message_target =
        match target with
        | `Import import ->
          Import.mark_used import;
          Import.message_target import
        | `QuestionCap (question, i) ->
          Question.set_cap_used question i;
          Question.message_target question i
      in
      let qid = Question.id question in
      Log.debug (fun f -> f ~tags:(with_qid qid t) "Sending: (%a).call %a"
                    pp_cap target
                    Core_types.Request_payload.pp msg);
      t.queue_send (`Call (qid, message_target, msg, descs, results_to));
      resolve_broken t broken_caps;
      question

    let return_results t answer msg =
      let caps = Core_types.Response_payload.snapshot_caps msg in
      let aid = Answer.id answer in
      let caps = RO_array.map (fun c -> c#shortest) caps in
      Log.debug (fun f -> f ~tags:(with_aid aid t) "Returning results: %a"
                    Core_types.Response_payload.pp msg);
      RO_array.iter Core_types.inc_ref caps;        (* Copy everything stored in [msg]. *)
      let broken_caps = Queue.create () in
      let descs = RO_array.map (export ~broken_caps t) caps in
      let exports_for_release = exports_of descs in
      let resolve_targets = RO_array.map (get_resolve_target t) caps in
      Answer.return_resolved answer ~exports_for_release ~resolve_targets;
      RO_array.iter Core_types.dec_ref caps;
      let ret = `Results (msg, descs) in
      Log.debug (fun f -> f ~tags:(with_aid aid t) "Wire results: %a" Out.pp_return ret);
      t.queue_send (`Return (aid, ret, false));
      resolve_broken t broken_caps

    let return t answer ret =
      let aid = Answer.id answer in
      match ret with
      | Ok payload ->
        return_results t answer payload
      | Error err ->
        let ret = (err : Error.t :> Out.return) in
        Log.debug (fun f -> f ~tags:(with_aid aid t) "Returning error: %a" Error.pp err);
        Answer.return_resolved answer ~exports_for_release:[] ~resolve_targets:RO_array.empty;
        t.queue_send (`Return (aid, ret, false))

    let release t import count =
      Imports.release t.imports (Import.id import);
      Log.debug (fun f -> f ~tags:t.tags "Sending release of %a" Import.pp import);
      t.queue_send (`Release (Import.id import, count))

    let finish t question =
      let qid = Question.id question in
      Log.debug (fun f -> f ~tags:(with_qid qid t) "Send finish %a" Question.pp_promise question);
      t.queue_send (`Finish (qid, false))
  end

  let apply_import_actions t =
    List.iter @@ function
    | `Release (i, count) -> Send.release t i count

  let apply_question_actions t q =
    List.iter @@ function
    | `Send_finish         -> Send.finish t q
    | `Release_table_entry -> Questions.release t.questions (Question.id q)
    |` Release_import i    -> Import.dec_ref i |> apply_import_actions t

  let release_resolve_target t = function
    | `None | `Local -> ()
    | `QuestionCap (q, _) -> Question.dec_ref q |> apply_question_actions t q;
    | `Import i           -> Import.dec_ref   i |> apply_import_actions t

  let release t export_id ~ref_count =
    assert (ref_count > 0);
    let export = Exports.find_exn t.exports export_id in
    match Export.release export ref_count with
    | `Do_nothing -> ()
    | `Send_release (service, resolve_target) ->
      Log.debug (fun f -> f ~tags:t.tags "Releasing export %a" Export.pp export);
      Hashtbl.remove t.exported_caps service;
      Exports.release t.exports export_id;
      dec_ref service;
      release_resolve_target t resolve_target

  let apply_answer_actions t answer =
    List.iter @@ function
    | `Return_cancelled ->
      let aid = Answer.id answer in
      Log.debug (fun f -> f ~tags:(with_aid aid t) "Returning cancelled");
      t.queue_send (`Return (aid, `Cancelled, false));
    | `Release_exports exports -> List.iter (release t ~ref_count:1) exports
    | `Release_resolve_targets targets -> RO_array.iter (release_resolve_target t) targets
    | `Question_actions (q, actions) -> apply_question_actions t q actions

  (* Note: takes ownership of [msg] *)
  let rec send_call t target (results:Core_types.struct_resolver) msg =
    match t.disconnected with
    | Some ex -> results#resolve (Core_types.broken_struct (`Exception ex))
    | None ->
      match results#sealed_dispatch CapTP_results with
      | Some (_, answer) when not (Answer.needs_return answer) ->
        (* The only reason for making this call is to answer a cancelled question (we already
           replied to it to confirm the cancellation). No-one cares about the answer, so do nothing. *)
        Core_types.Request_payload.release msg;
        Core_types.resolve_exn results Exception.cancelled (* (might be useful for debugging) *)
      | Some (t', answer) when t == t' ->
        let remote_promise = make_remote_promise t in
        (* [results] is at [target]. We can tell the peer to send the results to itself directly. *)
        let question = Send.call t (remote_promise :> Core_types.struct_resolver)
            target msg ~results_to:`Yourself in
        Core_types.Request_payload.release msg;
        remote_promise#set_question question;
        Answer.return_take_from_question answer question;
        let aid = Answer.id answer in
        Log.debug (fun f -> f ~tags:(with_aid aid t) "Returning take-from-other-question %a" Question.pp question);
        t.queue_send (`Return (aid, `TakeFromOtherQuestion (Question.id question), false));
        (* Hook this up so we still forward any pipelined calls *)
        results#resolve (remote_promise :> Core_types.struct_ref)
      | _ ->
        let remote_promise = make_remote_promise t in
        let question = Send.call t (remote_promise :> Core_types.struct_resolver)
            target msg ~results_to:`Caller in
        Core_types.Request_payload.release msg;
        remote_promise#set_question question;
        results#resolve (remote_promise :> Core_types.struct_ref)

  (* A cap that sends to a promised answer's cap at other *)
  and make_remote_promise t =
    object (self : #Core_types.struct_resolver)
      inherit [Question.t option] Struct_proxy.t None

      val mutable released_question = false

      (* We send Finish in two cases:
         - We are unresolved and the user wants to cancel.
         - We got the result and want the server to free up the question.
         Send Finish on whichever happens first, but not both.
       *)
      method private ensure_released q =
        if not released_question then (
          released_question <- true;
          Question.release_proxy q |> apply_question_actions t q
        )

      method do_pipeline question i results msg =
        match question with
        | Some target_q ->
          let target = `QuestionCap (target_q, i) in
          send_call t target results msg
        | None -> failwith "Not initialised!"

      method on_resolve q _ =
        match q with
        | None -> failwith "Not initialised!"
        | Some q -> self#ensure_released q

      val name = "remote-promise"

      method pp_unresolved = pp_promise

      method set_question q =
        self#update_target (Some q)

      method send_cancel = function
        | None -> failwith "Not initialised!"
        | Some q -> self#ensure_released q

      method field_sealed_dispatch : type a. Wire.Path.t -> a S.brand -> a option = fun path -> function
        | CapTP ->
          begin match state with
            | Unresolved u ->
              begin match u.target with
                | None -> failwith "Not intialised!"
                | Some target_q -> Some (t, `QuestionCap (target_q, path))
              end
            | _ -> failwith "Not a promise!"
          end;
        | _ -> None
    end

  let disembargo t request =
    Log.debug (fun f -> f ~tags:t.tags "Sending disembargo %a" EP.Out.pp_disembargo_request request);
    t.queue_send (`Disembargo_request request)

  let bootstrap t object_id =
    let result = make_remote_promise t in
    let question = Send.bootstrap t (result :> Core_types.struct_resolver) in
    result#set_question question;
    let qid = Question.id question in
    Log.debug (fun f -> f ~tags:(with_qid qid t) "Sending: bootstrap");
    t.queue_send (`Bootstrap (qid, object_id));
    let service = result#cap Wire.Path.root in
    dec_ref result;
    service

  module Switchable = struct
    class type handler = object
      method pp : Format.formatter -> unit
      method sealed_dispatch : 'a. 'a S.brand -> 'a option
      method call : Core_types.struct_resolver -> Wire.Request.t -> unit
    end

    type unset = {
      mutable rc : RC.t;
      handler : handler; (* Will forward calls here until set *)
      on_set : (Core_types.cap -> unit) Queue.t;
      on_release : (unit -> unit) Queue.t;
    }

    type state =
      | Unset of unset
      | Set of Core_types.cap

    let released = Core_types.broken_cap (Exception.v "(released)")

    let pp_state f = function
      | Unset x -> Fmt.pf f "(unset, %a) -> %t" RC.pp x.rc x.handler#pp
      | Set x -> Fmt.pf f "(set) -> %t" x#pp

    let target = function
      | Unset x -> x.handler
      | Set x -> (x :> handler)

    type 'a S.brand += Gc : unit S.brand

    (* Forwards messages to [init] until resolved.
       Forces [release] when resolved or released.
       Stores a reference to itself in [strong_proxy] when unresolved but with queued callbacks. *)
    let make ~(release:unit Lazy.t) ~settled ~strong_proxy init =
      object (self : #Core_types.cap)
        val id = Debug.OID.next ()

        val mutable state =
          Unset { rc = RC.one; handler = init; on_set = Queue.create (); on_release = Queue.create () }

        method call msg caps =
          (target state)#call msg caps

        method update_rc d =
          match state with
          | Unset u ->
            u.rc <- RC.sum u.rc d ~pp:(fun f -> self#pp f);
            if RC.is_zero u.rc then (
              Lazy.force release;
              let old_state = state in
              strong_proxy := None;
              state <- Set released;
              match old_state with
              | Unset u -> Queue.iter (fun f -> f ()) u.on_release
              | Set x -> dec_ref x
            )
          | Set x -> x#update_rc d

        method resolve cap =
          match state with
          | Set _ -> Debug.failf "Can't resolve already-set switchable %t to %t!" self#pp cap#pp
          | Unset {handler = _; rc; on_set; on_release} ->
            let pp f = self#pp f in
            RC.check ~pp rc;
            state <- Set cap;
            strong_proxy := None;
            begin match RC.to_int rc with
              | Some rc -> cap#update_rc (rc - 1);     (* Transfer our ref-count *)
              | None -> ()
            end;
            Queue.iter (fun f -> f (with_inc_ref cap)) on_set;
            Queue.iter (fun f -> cap#when_released f) on_release;
            Lazy.force release

        method break ex =
          self#resolve (Core_types.broken_cap ex)

        method shortest =
          match state with
          | Unset _ -> (self :> Core_types.cap)     (* Can't shorten, as we may change later *)
          | Set x -> x#shortest

        method blocker =
          match state with
          | Unset _ when settled -> None
          | Unset _ -> Some (self :> Core_types.base_ref)
          | Set x -> x#blocker

        method problem =
          match state with
          | Unset _ -> None
          | Set x -> x#problem

        method when_more_resolved fn =
          match state with
          | Unset x ->
            Queue.add fn x.on_set;
            strong_proxy := Some (self :> < >)
          | Set x -> x#when_more_resolved fn

        method when_released fn =
          match state with
          | Unset x ->
            Queue.add fn x.on_release;
            strong_proxy := Some (self :> < >)
          | Set x -> x#when_released fn

        (* When trying to find the target for export, it's OK to expose our current
           target, even though [shortest] shouldn't.
           In particular, we need this to deal with disembargo requests. *)
        method sealed_dispatch : type a. a S.brand -> a option = function
          | CapTP ->
            begin match state with
              | Unset x -> x.handler#sealed_dispatch CapTP
              | Set x -> x#shortest#sealed_dispatch CapTP
            end
          | Gc ->
            begin match state with
              | Unset x ->
                Core_types.Wire.ref_leak_detected (fun () ->
                    if RC.is_zero x.rc then (
                      Log.warn (fun f -> f "@[<v2>Reference GC'd with non-zero ref-count!@,%t@,\
                                            But, ref-count is now zero, so a previous GC leak must have fixed it.@]"
                                   self#pp);
                    ) else (
                      Log.warn (fun f -> f "@[<v2>Reference GC'd with %a!@,%t@]"
                                   RC.pp x.rc self#pp);
                      x.rc <- RC.leaked;
                      state <- Set released; (* Don't call resolve; rc is now invalid *)
                      assert (!strong_proxy = None);    (* Otherwise we wouldn't have been GC'd *)
                      Queue.iter (fun f -> f released) x.on_set;
                      Lazy.force release
                    )
                  );
              | Set _ -> ()
            end;
            Some ()
          | _ -> None

        method check_invariants =
          match state with
          | Unset u ->
            let pp f = self#pp f in
            assert ((!strong_proxy = None) = (Queue.is_empty u.on_set && Queue.is_empty u.on_release));
            RC.check ~pp u.rc
          | Set x ->
            assert (!strong_proxy = None);
            x#check_invariants

        method pp f =
          Fmt.pf f "switchable(%a) %a" Debug.OID.pp id pp_state state

        initializer
          Gc.finalise (fun (self:#Core_types.base_ref) -> ignore (self#sealed_dispatch Gc)) self
      end
  end

  module Input : sig
    open EP.Core_types

    val call : t -> In.QuestionId.t -> In.message_target ->
      Wire.Request.t -> In.desc RO_array.t -> results_to:In.send_results_to ->
      unit

    val bootstrap : t -> In.QuestionId.t -> string -> unit
    val return : t -> In.AnswerId.t -> In.return -> release_param_caps:bool -> unit
    val finish : t -> In.QuestionId.t -> release_result_caps:bool -> unit
    val release : t -> In.ImportId.t -> ref_count:int -> unit
    val disembargo_request : t -> In.disembargo_request -> unit
    val disembargo_reply : t -> In.message_target -> Message_types.EmbargoId.t -> unit
    val resolve : t -> In.ExportId.t -> (In.desc, Exception.t) result -> unit
  end = struct
    let release = release

    let set_import_proxy t ~settled import =
      let message_target = `Import import in
      let cap =
        object (_ : Switchable.handler)
          val id = Debug.OID.next ()

          method call results msg =
            send_call t message_target results msg

          method pp f =
            if settled then
              Fmt.pf f "far-ref(%a) -> %a" Debug.OID.pp id pp_cap message_target
            else
              Fmt.pf f "remote-promise(%a) -> %a" Debug.OID.pp id pp_cap message_target

          method sealed_dispatch : type a. a S.brand -> a option = function
            | CapTP -> Some (t, message_target)
            | _ -> None
        end
      in
      let release = lazy (
        Import.clear_proxy import;
        Import.dec_ref import |> apply_import_actions t;
      ) in
      (* Imports can resolve to another cap (if unsettled) or break. *)
      let switchable = Switchable.make ~release ~settled ~strong_proxy:import.strong_proxy cap in
      Import.init_proxy import switchable;
      switchable

    let import_sender t ~mark_dirty ~settled id =
      let new_import () =
        let import = Import.v id ~mark_dirty ~settled in
        Imports.set t.imports id import;
        (set_import_proxy t ~settled import :> Core_types.cap)
      in
      match Imports.find t.imports id with
      | None -> new_import ()
      | Some import ->
        Import.inc_count import;
        if mark_dirty then Import.mark_used import;
        match Import.get_proxy import with
        | Some proxy ->
          Core_types.inc_ref proxy;
          (proxy :> Core_types.cap)
        | None ->
          (* The switchable got GC'd. It has already dec-ref'd the import.
             Make a new proxy. *)
          Import.inc_ref import;
          (set_import_proxy t ~settled import :> Core_types.cap)

    (* We previously pipelined messages to [old_path], which now turns out to be
       local object [x]. Create an embargo proxy for [x] and send a disembargo
       for it down [old_path]. *)
    let local_embargo t ~old_path x =
      if x#problem <> None then x (* Don't embargo errors *)
      else (
        let embargo = Cap_proxy.local_promise () in
        (* Store in table *)
        inc_ref embargo;
        let (embargo_id, _) = Embargoes.alloc t.embargoes (fun id -> (id, embargo)) in
        (* Send disembargo request *)
        let disembargo_request = `Loopback (old_path, embargo_id) in
        Log.debug (fun f -> f ~tags:t.tags "Embargo %t until %a is delivered"
                      x#pp
                      EP.Out.pp_disembargo_request disembargo_request
                  );
        disembargo t disembargo_request;
        (* We don't store [x], because when the reply comes back further disembargoes
           may be needed. *)
        dec_ref x;
        (embargo :> Core_types.cap)
      )

    let maybe_embargo t ~old_path x =
      match old_path with
      | None -> x
      | Some old_path -> local_embargo t ~old_path x

    (* Turn a connection-scoped cap reference received from our peer into a general-purpose
       cap for users. The caller owns the new reference and should [dec_ref] it when done.

       This is used in several places:

       - When we get a Resolve message (which must be for a SenderPromise or SenderHosted)
       - When we get a Return message (resolving each PromisedAnswer from the question)
       - When we receive a Call (no embargoes needed in that case; you can't pipeline over the
         arguments of a call you haven't yet received)
       - When we get a Disembargo_reply (might need another embargo)

       If [embargo_path] is passed then we have already pipelined messages over this cap, and
       may therefore need to embargo it (sending the disembargo via the old [embargo_path]).
       This can only happen when a local promise at the peer has resolved, and we are given its
       new target. *)
    let import t ?embargo_path : In.desc -> Core_types.cap = function
      | `SenderPromise id ->
        import_sender t id ~settled:false ~mark_dirty:(embargo_path <> None)
      | `SenderHosted id ->
        (* We don't have to worry about embargoes here because anything we've already sent to the peer will
           arrive before anything we send in the future, and it can't change. *)
        import_sender t id ~settled:true ~mark_dirty:false
      | `ReceiverHosted id ->
        let export = Exports.find_exn t.exports id in
        (* We host the target (which may be another promise).
           We need to flush any pipelined calls still in flight before we can use it. *)
        maybe_embargo t ~old_path:embargo_path (with_inc_ref (Export.service export))
      | `ReceiverAnswer (id, path) ->
        let answer = Answers.find_exn t.answers id in
        begin match Answer.answer_struct answer with
        | `Finished -> failwith "Got ReceiverAnswer for a finished promise!"
        | `Promise answer_promise ->
          match answer_promise#response with
          | None ->
            (* We don't know the answer, so we can't have replied yet.
               We can send a disembargo request now and the peer will get it before
               any answer and return it to us. *)
            maybe_embargo t ~old_path:embargo_path (answer_promise#cap path)
          | Some (Error _) -> answer_promise#cap path (* No need to embargo errors *)
          | Some (Ok payload) ->
            (* We've already replied to this question. If we returned a capability at the
               requested path then send a disembargo request to it. If not, we can't embargo,
               because the peer has no way to return the response, but we don't need to for errors. *)
            match Core_types.Response_payload.field payload path with
            | None -> answer_promise#cap path (* Don't embargo None *)
            | Some c -> dec_ref c; maybe_embargo t ~old_path:embargo_path (answer_promise#cap path)
        end
      | `None -> Core_types.null
      | `ThirdPartyHosted _ -> failwith "todo: import"

    let make_answer_promise t answer ~results_to =
      let aid = Answer.id answer in
      let promise, resolver = Local_struct_promise.make () in
      let () =
        promise#when_resolved @@ fun x ->
        if Answer.needs_return answer && t.disconnected = None then (
          match results_to with
          | `Caller -> Send.return t answer x
          | `Yourself ->
            Log.debug (fun f -> f ~tags:(with_aid aid t) "Returning results-sent-elsewhere");
            Answer.return_resolved answer ~exports_for_release:[] ~resolve_targets:RO_array.empty;
            t.queue_send (`Return (aid, `ResultsSentElsewhere, false))
          | `ThirdParty _ -> failwith "todo: handle call by sending results to ThirdParty"
        )
        (* (else cancelled; reply already sent) *)
      in
      let resolver =
        if results_to = `Caller then (
          object (_ : Core_types.struct_resolver)
            method pp f = Fmt.pf f "answer %a <- %t" AnswerId.pp aid resolver#pp

            method resolve = resolver#resolve

            method set_blocker = resolver#set_blocker
            method clear_blocker = resolver#clear_blocker

            method sealed_dispatch : type a. a S.brand -> a option = function
              | CapTP_results -> Some (t, answer)
              | _ -> None
          end
        ) else resolver
      in
      promise, resolver

    let call t aid (message_target : In.message_target) msg descs ~results_to =
      (* todo: allowThirdPartyTailCall *)
      let answer = Answer.create_uninitialised aid in
      let answer_promise, answer_resolver = make_answer_promise t answer ~results_to in
      Answer.init answer answer_promise;
      Answers.set t.answers aid answer;
      let target =
        match message_target with
        | `ReceiverHosted id ->
          let export = Exports.find_exn t.exports id in
          begin match Export.resolve_target export with
            | #message_target_cap as target -> target
            | `Local
            | `None -> `Local (with_inc_ref (Export.service export))
          end
        | `ReceiverAnswer (id, path) ->
          let answer = Answers.find_exn t.answers id in
          match Answer.resolve_target answer path with
          | Some (Ok (#message_target_cap as target)) -> target
          | Some (Ok (`Local | `None))
          | Some (Error _)
          | None ->
            (* Unresolved, error or local - queue on our promise *)
            match Answer.answer_struct answer with
            | `Finished -> failwith "Call to finished answer (shouldn't be in table!)"
            | `Promise answer_promise -> `Local (answer_promise#cap path)
      in
      let caps = RO_array.map (import t) descs in
      let msg = Core_types.Request_payload.with_caps caps msg in
      match target with
      | `Local target ->
        Log.debug (fun f -> f ~tags:t.tags "Handling call: (%t).call %a"
                      target#pp Core_types.Request_payload.pp msg);
        t.fork (fun () ->
            target#call answer_resolver msg;  (* Takes ownership of [caps]. *)
            dec_ref target
          )
      | #message_target_cap as target ->
        Log.debug (fun f -> f ~tags:t.tags "Forwarding call: (%a).call %a"
                      pp_message_target_cap target Core_types.Request_payload.pp msg);
        send_call t target answer_resolver msg

    let bootstrap t id object_id =
      let promise, answer_resolver = Local_struct_promise.make () in
      let answer = Answer.create id ~answer:promise in
      Answers.set t.answers id answer;
      t.fork @@ fun () ->
      object_id |> t.restore @@ fun service ->
      if Answer.needs_return answer && t.disconnected = None then (
        let results =
          match service with
          | Error ex -> Error (`Exception ex)
          | Ok service ->
            let msg =
              Wire.Response.bootstrap ()
              |> Core_types.Response_payload.with_caps (RO_array.of_list [service])
            in
            Ok msg
        in
        Core_types.resolve_payload answer_resolver results;
        Send.return t answer results
      ) else (
        Result.iter dec_ref service
      )

    let return_results t question msg descrs =
      let caps_used = Question.paths_used question |> caps_used ~msg in
      let import_with_embargoes cap_index d =
        let embargo_path =
          match IntMap.find cap_index caps_used with
          | None -> None
          | Some path -> Some (Question.message_target question path)
        in
        import t d ?embargo_path
      in
      RO_array.mapi import_with_embargoes descrs

    (* Turn wire descriptors into local objects, creating embargoes and sending
       disembargo messages where necessary. *)
    let import_return_caps t question ret =
      match ret with
      | `Results (msg, descs) ->
        let caps = return_results t question msg descs in
        `Results (Core_types.Response_payload.with_caps caps msg)
      | #Error.t as err -> err
      | `ResultsSentElsewhere -> `ResultsSentElsewhere
      | `AcceptFromThirdParty -> failwith "todo: AcceptFromThirdParty"
      | `TakeFromOtherQuestion aid ->
        match Answer.answer_struct (Answers.find_exn t.answers aid) with
        | `Finished -> Debug.failf "Can't take from answer %a - it's already finished!" AnswerId.pp aid
        | `Promise other ->
          match question.remote_promise with
          | `Released -> `TakeFromCancelledQuestion
          | `Resolver remote_promise ->
            inc_ref other;
            let paths_used = question.Question.pipelined_fields in
            if PathSet.is_empty paths_used then `TakeFromOtherQuestion (other, remote_promise)
            else (
              (* Embargoes needed for paths that were used when we got the original return. *)
              Question.inc_ref question;
              `TakeFromOtherQuestionAndEmbargo (other, remote_promise, paths_used)      (* Note: ref's question *)
            )

    let return t qid ret ~release_param_caps =
      let question = Questions.find_exn t.questions qid in
      if release_param_caps then List.iter (release t ~ref_count:1) question.params_for_release;
      let ret2 = import_return_caps t question ret in
      (* Any disembargo requests have now been sent, so we may finish the question. *)
      let get_import id =
        let i = Imports.find_exn t.imports id in
        Import.inc_ref i;
        i
      in
      let disembargo_info = Question.extract_resolution ~get_import ret in
      Question.return question disembargo_info |> apply_question_actions t question;
      begin match ret2 with
        | `Results msg ->
          Log.debug (fun f -> f ~tags:(with_qid qid t) "Got results: %a"
                        Core_types.Response_payload.pp msg
                    );
          Question.resolve question (Ok msg)
        | `TakeFromCancelledQuestion -> () (* Ignore response to cancelled Question *)
        | `TakeFromOtherQuestion (other, remote_promise) ->
          (* (we have added a ref on [other]) *)
          remote_promise#resolve (other :> Core_types.struct_ref)
        | `TakeFromOtherQuestionAndEmbargo (other, remote_promise, paths_used) ->
          (* (we have added a ref on [other] and on [question]) *)
          (* Embargoes needed for paths that were used when we got the original return. *)
          (* We must resolve the remote promise to a local embargo now, so nothing else
             gets sent to it. *)
          let embargo, embargo_resolver = Local_struct_promise.make () in
          begin match embargo_resolver#set_blocker (other :> Core_types.base_ref) with
            | Error `Cycle -> assert false (* Can't happen; we just made it! *)
            | Ok () ->
              remote_promise#resolve embargo;
              other#when_resolved (fun payload ->
                  (* XXX: Do we need a when_more_resolved here instead?
                     We might need multiple disembargoes as things resolve further... *)
                  embargo_resolver#clear_blocker;
                  begin match payload with
                    | Error _ as e ->
                      Core_types.resolve_payload embargo_resolver e;
                    | Ok msg ->
                      let embargoes_needed = caps_used ~msg paths_used in
                      let maybe_embargo cap_index cap =
                        inc_ref cap;
                        match IntMap.find cap_index embargoes_needed with
                        | None -> cap
                        | Some path ->
                          let old_path = Question.message_target question path in
                          local_embargo t ~old_path cap
                      in
                      let caps = Core_types.Response_payload.snapshot_caps msg |> RO_array.mapi maybe_embargo in
                      Core_types.resolve_ok embargo_resolver
                        (Core_types.Response_payload.with_caps caps msg)
                  end;
                  dec_ref other;
                  Question.dec_ref question |> apply_question_actions t question
                )
          end;
        | `ResultsSentElsewhere ->
          (* Keep unresolved; we must continue forwarding *)
          ()
        | #Error.t as err -> Question.resolve question (Error err)
      end

    let finish t aid ~release_result_caps =
      let answer = Answers.find_exn t.answers aid in
      Log.debug (fun f -> f ~tags:(with_aid aid t) "Received finish for %a" Answer.dump answer);
      Answers.release t.answers aid;
      Answer.finish answer ~release_result_caps |> apply_answer_actions t answer

    let send_disembargo t embargo_id target =
      let desc =
        match target with
        | `None -> Debug.failf "Protocol error: disembargo request for None cap"
        | `Local -> Debug.failf "Protocol error: disembargo request for local target"
        | `QuestionCap (question, path) -> Question.message_target question path
        | `Import import -> Import.message_target import
      in
      Log.debug (fun f -> f ~tags:t.tags "Sending disembargo response to %a" EP.Out.pp_desc desc);
      t.queue_send (`Disembargo_reply (desc, embargo_id))

    (* If we're trying to disembargo something that resolved to an import, try to disembargo on
       that instead. *)
    let rec disembargo_imports = function
      | None | Some #EP.Out.message_target as x -> x
      | Some (`Import i) -> disembargo_imports (Import.disembargo_target i)

    let disembargo_request t request =
      Log.debug (fun f -> f ~tags:t.tags "Received disembargo request %a" EP.In.pp_disembargo_request request);
      match request with
      | `Loopback (old_path, embargo_id) ->
        match old_path with
        | `ReceiverHosted eid ->
          send_disembargo t embargo_id (Exports.find_exn t.exports eid |> Export.resolve_target);
        | `ReceiverAnswer (aid, path) ->
          let answer = Answers.find_exn t.answers aid in
          send_disembargo t embargo_id (Answer.disembargo_target answer path)

    (* [double_disembargo_path resolve_target] is used to check whether we need a second embargo
       after a previous disembargo arrived back at an answer or export which is now resolved to
       [resolve_target]. If [resolve_target] is set and is remote and we have pipelined messages
       to it then we return the path to use for the next disembargo step.
       Returns [None] if no embargo is needed at the moment. *)
    let double_disembargo_path = function
      | `None ->
        (* We haven't resolved yet. Any pipelined messages have been queued up locally and will
           be forwarded when the answer or export does resolve. We can safely make calls directly
           on the local promise. *)
        None
      | `Local ->
        (* We resolved to a local service (possibly another promise).
           Any pipelined messages have already been delivered to it and we can therefore send it
           messages immediately. *)
        None
      | `QuestionCap (q, path) ->
        (* We resolved to a question. If we have pipelined messages to the question and it has
           resolved to a local service then we may need another embargo before using the new
           local target. *)
        Question.answer_cap_disembargo q path |> disembargo_imports
      | `Import i ->
        (* We resolved to an import. If we have pipelined messages to the remote export and it
           has resolved to a local service then we will need another embargo before using the new
           local target. *)
        if Import.used i then disembargo_imports (Import.disembargo_target i)
        else None

    let disembargo_reply t target embargo_id =
      let embargo = snd (Embargoes.find_exn t.embargoes embargo_id) in
      Log.debug (fun f -> f ~tags:t.tags "Received disembargo response %a -> %t"
                    EP.In.pp_desc target
                    embargo#pp);
      (* A remote export or answer resolved to [target] at some point in the
         past and we sent a disembargo request to it to flush any pipelined
         messages. This is now done - all pipelined messages that we care about
         have been delivered to [target]. However, [target] may have forwarded
         them on in turn, so another disembargo may be called for.

         Release the old embargo, so that the cap now points at [target]'s
         initial resolution. If [target] is already resolved, attempt to update
         to its resolution, performing another embargo if necessary.
       *)
      let embargo_path =
        match target with
        | `ReceiverHosted id ->
          let export = Exports.find_exn t.exports id in
          double_disembargo_path (Export.resolve_target export)
        | `ReceiverAnswer (id, path) ->
          let answer = Answers.find_exn t.answers id in
          match Answer.resolve_target answer path with
          | None -> None                    (* No embargoes as still unresolved (target still remote) *)
          | Some (Error _) -> None          (* No embargoes for errors *)
          | Some (Ok resolve_target) ->
            double_disembargo_path resolve_target
      in
      let cap = import t ?embargo_path (target :> EP.In.desc) in
      Log.debug (fun f -> f "Disembargo target is %t" cap#pp);
      Embargoes.release t.embargoes embargo_id;
      embargo#resolve cap;
      dec_ref embargo

    let resolve t import_id new_target =
      Log.debug (fun f -> f ~tags:t.tags "Received resolve of import %a to %a"
                    ImportId.pp import_id
                    (Fmt.result ~ok:In.pp_desc ~error:Exception.pp) new_target
                );
      let import_new_target ~embargo_path =
        match new_target with
        | Error e -> Core_types.broken_cap e
        | Ok desc -> import t desc ?embargo_path
      in
      match Imports.find t.imports import_id with
      | None ->
        let new_target = import_new_target ~embargo_path:None in
        Log.debug (fun f -> f ~tags:t.tags "Import %a no longer used - releasing new resolve target %t"
                      ImportId.pp import_id new_target#pp);
        dec_ref new_target
      | Some im ->
        (* Check we're not resolving a settled import. *)
        if im.Import.settled then (
          let new_target = import_new_target ~embargo_path:None in
          let msg = Fmt.str "Got a Resolve (to %t) for settled import %a!" new_target#pp Import.dump im in
          dec_ref new_target;
          failwith msg
        );
        let get_import id =
          let i = Imports.find_exn t.imports id in
          Import.inc_ref i;
          i
        in
        match Import.get_proxy im with
        | Some x ->
          (* This will also dec_ref the old remote-promise and the import. *)
          let target = import_new_target ~embargo_path:(Import.embargo_path im) in
          Import.mark_resolved im ~get_import new_target;
          x#resolve target
        | None ->
          (* If we get here:
             - The user released the switchable, but
             - Some [resolve_target] kept the import in the table. *)
          let target = import_new_target ~embargo_path:None in
          Import.mark_resolved im ~get_import new_target;
          Log.debug (fun f -> f ~tags:t.tags "Ignoring resolve of import %a, which we no longer need (to %t)"
                        ImportId.pp import_id target#pp);
          dec_ref target

  end

  let handle_unimplemented t (msg : Out.t) =
    match msg with
    | `Resolve (_, Error _) -> ()
    | `Resolve (_, Ok new_target) ->
      (* If the peer doesn't understand resolve messages, we can just release target. *)
      begin match new_target with
      | `None
      | `ReceiverHosted _
      | `ReceiverAnswer _ -> ()
      | `SenderHosted id
      | `SenderPromise id
      | `ThirdPartyHosted (_, id) -> Input.release t id ~ref_count:1
      end
    | `Bootstrap (qid, _) ->
      (* If the peer didn't understand our question, pretend it returned an exception. *)
      Input.return t qid ~release_param_caps:true
        (Error.exn ~ty:`Unimplemented "Bootstrap message not implemented by peer")
    | `Call (qid, _, _, _, _) ->
      (* This could happen if we asked for the bootstrap object from a peer that doesn't
         offer any services, and then tried to pipeline on the result. *)
      Input.return t qid ~release_param_caps:true
        (Error.exn ~ty:`Unimplemented "Call message not implemented by peer!")
    | _ ->
      failwith "Protocol error: peer unexpectedly responded with Unimplemented"

  let disconnect t ex =
    if t.disconnected = None then (
      t.disconnected <- Some ex;
      t.queue_send <- ignore;
      Exports.drop_all t.exports (fun _ -> Export.lost_connection);
      Hashtbl.clear t.exported_caps;
      Questions.drop_all t.questions (fun _ -> Question.lost_connection ~ex);
      Answers.drop_all t.answers (fun _ a -> Answer.lost_connection a |> apply_answer_actions t a);
      let broken_cap = Core_types.broken_cap ex in
      Imports.drop_all t.imports (fun _ -> Import.lost_connection ~broken_cap);
      Embargoes.drop_all t.embargoes (fun _ (_, e) -> e#break ex; dec_ref e)
    )

  let handle_msg t (msg : [<In.t | `Unimplemented of Out.t]) =
    check_connected t;
    match msg with
    | `Abort ex                       -> disconnect t ex
    | `Call (aid, target,
             msg, descs, results_to)  -> Input.call t aid target msg descs ~results_to
    | `Bootstrap (qid, oid)           -> Input.bootstrap t qid oid
    | `Return (aid, ret, release)     -> Input.return t aid ret ~release_param_caps:release
    | `Finish (aid, release)          -> Input.finish t aid ~release_result_caps:release
    | `Release (id, count)            -> Input.release t id ~ref_count:count
    | `Disembargo_request req         -> Input.disembargo_request t req
    | `Disembargo_reply (target, id)  -> Input.disembargo_reply t target id
    | `Resolve (id, target)           -> Input.resolve t id target
    | `Unimplemented x                -> handle_unimplemented t x

  let dump_embargo f (id, proxy) =
    Fmt.pf f "%a: @[%t@]" EmbargoId.pp id proxy#pp

  let check_embargo x = (snd x)#check_invariants

  let check_exported_cap t cap export_id =
    match Exports.find_exn t.exports export_id with
    | export ->
      if Export.service export <> cap then (
        Debug.invariant_broken @@ fun f ->
        Fmt.pf f "export_caps maps %t to export %a back to different cap %a!"
          cap#pp Export.pp export Export.dump export
      )
    | exception ex ->
      Debug.invariant_broken @@ fun f ->
      Fmt.pf f "exported_caps for %t: %a" cap#pp Debug.pp_exn ex

  let exported_sort_key export_id = export_id

  let pp_exported_cap t f (cap, export_id) =
    Fmt.pf f "%t => export %a%a" cap#pp ExportId.pp export_id (pp_check (check_exported_cap t)) (cap, export_id)

  let dump f t =
    match t.disconnected with
    | Some reason -> Fmt.pf f "Disconnected: %a" Exception.pp reason
    | None ->
      Fmt.pf f "@[<v2>Questions:@,%a@]@,\
                @[<v2>Answers:@,%a@]@,\
                @[<v2>Exports:@,%a@]@,\
                @[<v2>Imports:@,%a@]@,\
                @[<v2>Embargoes:@,%a@]@,\
                @[<v2>Exported caps:@,%a@]@,"
        (Questions.dump ~check:Question.check Question.dump) t.questions
        (Answers.dump   ~check:Answer.check   Answer.dump) t.answers
        (Exports.dump   ~check:Export.check   Export.dump) t.exports
        (Imports.dump   ~check:Import.check   Import.dump) t.imports
        (Embargoes.dump ~check:check_embargo  dump_embargo) t.embargoes
        (hashtbl_dump ~key:exported_sort_key (pp_exported_cap t)) t.exported_caps

  let check t =
    Questions.iter  (fun _ -> Question.check) t.questions;
    Answers.iter    (fun _ -> Answer.check)   t.answers;
    Imports.iter    (fun _ -> Import.check)   t.imports;
    Exports.iter    (fun _ -> Export.check)   t.exports;
    Embargoes.iter  (fun _ -> check_embargo)  t.embargoes;
    Hashtbl.iter    (check_exported_cap t) t.exported_caps
end
