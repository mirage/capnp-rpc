open Asetmap

module Log = Debug.Log
module IntMap = Map.Make(struct type t = int let compare (a:int) b = compare a b end)

let rec filter_map f = function
  | [] -> []
  | x :: xs ->
    match f x with
    | None -> filter_map f xs
    | Some y -> y :: filter_map f xs

let dec_ref x = x#dec_ref

let pp_check check f (k, v) =
  try check k v
  with ex ->
    Fmt.pf f "@,[%a] %a"
      Fmt.(styled `Red string) "ERROR"
      Debug.pp_exn ex

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

  open EP.Table

  module PathSet = Set.Make(Wire.Path)
  module Embargoes = Table.Allocating(EmbargoId)

  module Questions = Table.Allocating(QuestionId)
  module Answers = Table.Tracking(AnswerId)
  module Exports = Table.Allocating(ExportId)
  module Imports = Table.Tracking(ImportId)

  module Question = struct
    (* State model for questions:

       1. waiting: finished=false, returned=false (initial state)
                   released=false
          We're waiting for some results.
          On release: if rc=0 then send Finish, goto cancelled
                      else goto cancelling
          On return: if rc=0 then send Finish, goto complete
                     else goto lingering

       2. cancelling: finished=false, returned=false, rc>0
                      released=true
          We want to cancel but can't yet because rc>0.
          On rc=0: send Finish, goto cancelled
          On return: goto lingering

       3. cancelled: finished=true, returned=false, rc=0
                     released=true
          We told the peer we don't care about the results.
          It may return them anyway or return "cancelled".
          On return: goto complete (with results=cancelled)

       4. lingering: finished=false, returned=true, rc>0
          We've got the results and want to finish, but we
          can't yet because rc>0.
          On rc=0: send Finish, goto complete
          On release: remain in lingering

       5: complete: finished=true, returned=true, rc=0 (final state)
          No longer in questions table.
          On release: ignore

       The "release" event means the user told us they don't care about the result.
       After this, released=true.

       The "return" event means the peer sent us a Return message.

       rc>0 means that we used this answer in an export. e.g. we told the peer
       that a capability in the answer to a question they asked us will come
       from this question that we asked them.

       Initially, our [answer] for the peer's question holds a reference to us
       so that it can forward any calls. This will prevent us from getting the
       "release" event. Once we get the results, the answer will update to
       point directly at the new target and we may get a "release" then.

       However, the peer may still need to send a disembargo request to the
       answer, which must be sent back to its *original* target (this
       questions's answer). For this case, we must keep our question alive a
       little longer, until the peer finishes their question.

       We expect this to happen soon because there's no point in the peer
       holding on to their question now that they've got the answer.

       Every path contains exactly one Finish, exactly one Return, and exactly
       one release event.

       Whenever finished=true:
       - rc=0
       - We have sent a Finish message.
       - We can't send any more messages referring to this question.
     *)

    type state =
      | Waiting
      | Cancelling
      | Cancelled
      | Lingering
      | Complete

    type t = {
      id : QuestionId.t;
      mutable remote_promise : [`Promise of Core_types.struct_resolver | `Released];
      mutable resolve_refs : int;           (* Number of [resolve_target]s using us. *)
      mutable state : state;
      params_for_release : ExportId.t list; (* For level 0 peers *)
      mutable pipelined_fields : PathSet.t; (* Fields used while unresolved; will need embargoes *)
    }

    let id t = t.id

    let sent_finish t =
      match t.state with
      | Waiting | Cancelling | Lingering -> false
      | Cancelled | Complete -> true

    let inc_ref t =
      assert (not (sent_finish t));
      t.resolve_refs <- t.resolve_refs + 1

    let dec_ref t =
      assert (t.resolve_refs > 0);
      t.resolve_refs <- t.resolve_refs - 1;
      if t.resolve_refs > 0 then []
      else match t.state with
        | Waiting -> []
        | Cancelling -> t.state <- Cancelled; [`Send_finish]
        | Lingering ->  t.state <- Complete;  [`Send_finish; `Release_table_entry]
        | Cancelled | Complete -> failwith "Can't hold refs while cancelled or complete!"

    (* A [Return] message has arrived. *)
    let return t =
      match t.state with
      | Waiting when t.resolve_refs = 0 ->
                      t.state <- Complete;  [`Send_finish; `Release_table_entry]
      | Waiting    -> t.state <- Lingering; []
      | Cancelling -> t.state <- Lingering; []
      | Cancelled  -> t.state <- Complete;  [`Release_table_entry]
      | Lingering | Complete -> failwith "Already returned!"

    (* [t.remote_promise] no longer needs [t]. Cancel if possible. *)
    let release t =
      match t.remote_promise with
      | `Released -> failwith "Already released!"
      | `Promise _ ->
        t.remote_promise <- `Released;
        match t.state with
        | Waiting when t.resolve_refs = 0 -> t.state <- Cancelled; [`Send_finish]
        | Waiting -> t.state <- Cancelling; []
        | Lingering | Complete -> []
        | Cancelling | Cancelled -> failwith "Can't release twice!"

    let set_cap_used t path =
      t.pipelined_fields <- PathSet.add path t.pipelined_fields

    (* Maps used cap indexes to their paths *)
    let caps_used t ~msg =
      PathSet.elements t.pipelined_fields
      |> filter_map (fun path ->
          match Wire.Response.cap_index msg path with
          | None -> None
          | Some i -> Some (i, path)
        )
      |> IntMap.of_list

    let pp f q =
      Fmt.pf f "q%a" QuestionId.pp q.id

    let pp_promise f q =
      match q.remote_promise with
      | `Released -> Fmt.string f "(released)"
      | `Promise p -> Fmt.pf f "%t" p#pp

    let pp_state f x =
      Fmt.string f @@ match x with
      | Waiting    -> "waiting"
      | Cancelling -> "cancelling"
      | Cancelled  -> "cancelled"
      | Lingering  -> "lingering"
      | Complete   -> "complete"

    let dump f t =
      Fmt.pf f "(%a) %a" pp_state t.state pp_promise t

    let check t =
      match t.remote_promise with
      | `Released -> ()
      | `Promise p -> p#check_invariants

    let lost_connection t ~ex =
      (* The resolve might get a "release" event, but we'll ignore it in the Complete state.
         We don't care about the final state, as the whole table is being dropped anyway. *)
      t.state <- Complete;
      match t.remote_promise with
      | `Released -> ()
      | `Promise p -> p#resolve (Error (`Exception ex))

    let message_target t path = `ReceiverAnswer (t.id, path)

    let resolve t payload =
      match t.remote_promise with
      | `Promise p -> p#resolve payload
      | `Released ->
        match payload with
        | Ok (_, caps) -> RO_array.iter (fun x -> x#dec_ref) caps
        | Error _ -> ()

    let v ~params_for_release ~remote_promise id =
      {
        id;
        remote_promise = `Promise remote_promise;
        resolve_refs = 0;
        state = Waiting;
        params_for_release;
        pipelined_fields = PathSet.empty
      }
  end

  type import = {
    import_id : ImportId.t;
    mutable import_resolve_refs : int;
    mutable import_count : int; (* Number of times remote sent us this *)
    mutable import_used : bool; (* We have sent a message to this target *)
    mutable import_proxy_released : bool; (* Implies import_proxy = Uninitialised *)
    mutable import_proxy : [
      | `Uninitialised
      | `Settled of Cap_proxy.resolver_cap Weak_ptr.t
      | `Unsettled of Cap_proxy.resolver_cap Weak_ptr.t   (* Note: might be resolved to a settled value *)
    ]
  }

  type message_target_cap = [
    | `Import of import
    | `QuestionCap of Question.t * Wire.Path.t
  ]

  (* When we answer a question or provide an export, we may tell the peer that the
     resource is local to it. In that case, we need to keep a reference to the remote
     object so that we can forward disembargo requests. *)
  type resolve_target = [
    message_target_cap
    | `Local                    (* Resolved to a local object, not one at the peer. *)
    | `None                     (* This capability is not resolved yet *)
  ]

  type answer = {
    answer_id : AnswerId.t;
    mutable exports_for_release : ExportId.t list;
    mutable answer_resolve_targets : resolve_target RO_array.t;
    answer_promise : Core_types.struct_resolver;
    mutable answer_finished : bool;
  }

  type export = {
    export_id : ExportId.t;
    mutable export_count : int; (* Number of times sent to remote and not yet released *)
    export_service : Core_types.cap;
    mutable export_resolve_target : resolve_target;
  }

  type descr = [
    message_target_cap
    | `ThirdPartyHosted of Out.third_party_desc
    | `Local of Core_types.cap
  ]

  let pp_cap : [< descr] Fmt.t = fun f -> function
    | `Import import -> Fmt.pf f "i%a" ImportId.pp import.import_id
    | `QuestionCap (question, p) -> Fmt.pf f "%a[%a]" Question.pp question Wire.Path.pp p
    | `ThirdPartyHosted _third_party_desc -> Fmt.pf f "ThirdPartyHosted"
    | `Local local -> Fmt.pf f "local:%t" local#pp

  type t = {
    mutable queue_send : (EP.Out.t -> unit);    (* (mutable for shutdown) *)
    tags : Logs.Tag.set;
    embargoes : (EmbargoId.t * Cap_proxy.embargo_cap) Embargoes.t;
    mutable bootstrap : Core_types.cap option; (* (mutable for shutdown) *)

    questions : Question.t Questions.t;
    answers : answer Answers.t;
    exports : export Exports.t;
    imports : import Imports.t;
    exported_caps : (Core_types.cap, ExportId.t) Hashtbl.t;

    mutable disconnected : Exception.t option;  (* If set, this connection is finished. *)
  }

  type 'a S.brand += CapTP : (t * message_target_cap) S.brand
  (* The [CapTP] message asks a capability to tell us its CapTP target, if any. *)

  let target_of (x : #Core_types.cap) = x#shortest#sealed_dispatch CapTP

  let my_target_of t (x : #Core_types.cap) =
    match target_of x with
    | Some (t', target) when t == t' -> Some target
    | _ -> None

  let get_import_proxy import =
    match import.import_proxy with
    | `Uninitialised -> None
    | `Settled p | `Unsettled p -> Weak_ptr.get p

  let dump_answer f x =
    Fmt.pf f "%t" x.answer_promise#pp

  let dump_export f x =
    Fmt.pf f "%t" x.export_service#pp

  let pp_weak f = function
    | None -> Fmt.pf f "(GC'd weak pointer)"
    | Some x -> x#pp f

  let dump_import f x =
    Fmt.pf f "%a" pp_weak (get_import_proxy x)

  let stats t =
    { Stats.
      n_questions = Questions.active t.questions;
      n_answers = Answers.active t.answers;
      n_imports = Imports.active t.imports;
      n_exports = Exports.active t.exports;
    }

  let create ?bootstrap ~tags ~queue_send =
    begin match bootstrap with
      | None -> ()
      | Some x -> x#inc_ref
    end;
    {
      queue_send = (queue_send :> EP.Out.t -> unit);
      tags;
      bootstrap = (bootstrap :> Core_types.cap option);
      questions = Questions.make ();
      answers = Answers.make ();
      imports = Imports.make ();
      exports = Exports.make ();
      embargoes = Embargoes.make ();
      exported_caps = Hashtbl.create 30;
      disconnected = None;
    }

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
      message_target_cap -> Wire.Request.t -> Core_types.cap RO_array.t ->
      Question.t

    val return : t -> answer -> unit

    val release : t -> import -> unit
    (** [release t i] tells the peer that [i] is no longer needed by us. *)

    val finish : t -> Question.t -> unit
    (** [finish t q] sends a Finish message (with [releaseResultCaps=false]). *)
  end = struct

    (** We are sending [cap] to the client, either in a Return or Finish message.
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
            import.import_resolve_refs <- import.import_resolve_refs + 1;
            target
        end
      | Some _ (* TODO: third-party *)
      | None -> `Local

    (** [export ~broken_caps t cap] is a descriptor for [cap].
        If [cap] is a proxy object for a service at the peer, tell the peer the target directly.
        Otherwise, export it to the peer (reusing an existing export, if any).
        If the cap is broken and needs a fresh export, we queue up a suitable resolve message
        on [broken_caps]. This is needed for return messages. *)
    let rec export ?broken_caps : t -> Core_types.cap -> Out.desc = fun t cap ->
      let cap = cap#shortest in
      match my_target_of t cap with
      | Some (`Import import) -> `ReceiverHosted import.import_id
      | Some (`QuestionCap (question, i)) -> Question.message_target question i
      | None ->
        let settled = cap#blocker = None in
        let ex =
          match Hashtbl.find t.exported_caps cap with
          | id ->
            let ex = Exports.find_exn t.exports id in
            ex.export_count <- ex.export_count + 1;
            ex
          | exception Not_found ->
            cap#inc_ref;
            let ex = Exports.alloc t.exports (fun export_id ->
                { export_count = 1; export_service = cap; export_id; export_resolve_target = `None }
              )
            in
            let id = ex.export_id in
            Hashtbl.add t.exported_caps cap id;
            begin match cap#problem, broken_caps with
            | Some problem, Some broken_caps -> Queue.add (ex, problem) broken_caps
            | Some _, _ -> failwith "Cap is broken, but [broken_caps] not provided!"
            | None, _ when settled -> ()
            | None, _ ->
              Log.info (fun f -> f ~tags:t.tags "Monitoring promise export %a -> %a" ExportId.pp ex.export_id dump_export ex);
              cap#when_more_resolved (fun x ->
                  if ex.export_count > 0 then (
                    let x = x#shortest in
                    match x#problem with
                    | Some problem ->
                      Log.info (fun f -> f ~tags:t.tags "Export %a resolved to %t - sending exception"
                                   ExportId.pp ex.export_id
                                   x#pp
                               );
                      t.queue_send (`Resolve (ex.export_id, Error problem));
                    | None ->
                      let new_export = export t x in
                      Log.info (fun f -> f ~tags:t.tags "Export %a resolved to %t - sending notification to use %a"
                                   ExportId.pp ex.export_id
                                   x#pp
                                   Out.pp_desc new_export
                               );
                      ex.export_resolve_target <- get_resolve_target t x;
                      t.queue_send (`Resolve (ex.export_id, Ok new_export));
                  ); (* else: no longer an export *)
                  x#dec_ref
                )
            end;
            ex
        in
        let id = ex.export_id in
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
      Log.info (fun f -> f ~tags:t.tags "Sending resolve for already-broken export %a : %t"
                   ExportId.pp ex.export_id
                   ex.export_service#pp
               );
      t.queue_send (`Resolve (ex.export_id, Error problem))

    let call t remote_promise (target : message_target_cap) msg caps =
      let broken_caps = Queue.create () in
      let descs = RO_array.map (export ~broken_caps t) caps in
      let question = Questions.alloc t.questions
          (Question.v ~params_for_release:(exports_of descs) ~remote_promise)
      in
      let message_target =
        match target with
        | `Import import ->
          import.import_used <- true;
          `ReceiverHosted import.import_id
        | `QuestionCap (question, i) ->
          Question.set_cap_used question i;
          Question.message_target question i
      in
      let qid = Question.id question in
      Log.info (fun f -> f ~tags:(with_qid qid t) "Sending: (%a).call %a"
                   pp_cap target
                   Core_types.Request_payload.pp (msg, caps));
      t.queue_send (`Call (qid, message_target, msg, descs, `Caller));
      resolve_broken t broken_caps;
      question

    let return_results t answer (msg, caps) =
      let aid = answer.answer_id in
      let caps = RO_array.map (fun c -> c#shortest) caps in
      Log.info (fun f -> f ~tags:(with_aid aid t) "Returning results: %a"
                   Core_types.Response_payload.pp (msg, caps));
      RO_array.iter (fun c -> c#inc_ref) caps;        (* Copy everything stored in [answer]. *)
      let broken_caps = Queue.create () in
      let descs = RO_array.map (export ~broken_caps t) caps in
      answer.exports_for_release <- exports_of descs;
      answer.answer_resolve_targets <- RO_array.map (get_resolve_target t) caps;
      RO_array.iter dec_ref caps;
      let ret = `Results (msg, descs) in
      Log.info (fun f -> f ~tags:(with_aid aid t) "Wire results: %a" Out.pp_return ret);
      t.queue_send (`Return (aid, ret, false));
      resolve_broken t broken_caps

    let return t answer =
      let answer_promise = answer.answer_promise in
      let aid = answer.answer_id in
      match answer_promise#response with
      | None -> assert false
      | _ when answer.answer_finished ->
        Log.info (fun f -> f ~tags:(with_aid aid t) "Returning cancelled");
        t.queue_send (`Return (aid, `Cancelled, false));
      | Some (Ok payload) ->
        return_results t answer payload
      | Some (Error err) ->
        let ret = (err : Error.t :> Out.return) in
        Log.info (fun f -> f ~tags:(with_aid aid t) "Returning error: %a" Error.pp err);
        t.queue_send (`Return (aid, ret, false))

    let release t import =
      assert (import.import_resolve_refs = 0);
      assert (import.import_count > 0);
      Imports.release t.imports import.import_id;
      let count = import.import_count in
      import.import_count <- 0;       (* Just in case - mark as invalid *)
      Log.info (fun f -> f ~tags:t.tags "Sending release %a" dump_import import);
      t.queue_send (`Release (import.import_id, count))

    let finish t question =
      let qid = Question.id question in
      Log.info (fun f -> f ~tags:(with_qid qid t) "Send finish %a" Question.pp_promise question);
      t.queue_send (`Finish (qid, false))
  end

  let apply_question_actions t q =
    List.iter @@ function
    | `Send_finish         -> Send.finish t q
    | `Release_table_entry -> Questions.release t.questions (Question.id q)

  let maybe_release_import t i =
    if i.import_proxy_released && i.import_resolve_refs = 0 then Send.release t i

  (* Note: takes ownership of [caps] *)
  let rec send_call t target msg caps =
    let result = make_remote_promise t in
    let question = Send.call t (result :> Core_types.struct_resolver) target msg caps in
    RO_array.iter dec_ref caps;
    result#set_question question;
    (result :> Core_types.struct_ref)

  (* A cap that sends to a promised answer's cap at other *)
  and make_remote_promise t =
    object (self : #Core_types.struct_resolver)
      inherit [Question.t option] Struct_proxy.t None

      method do_pipeline question i msg caps =
        match question with
        | Some target_q ->
          let target = `QuestionCap (target_q, i) in
          send_call t target msg caps
        | None -> failwith "Not initialised!"

      method on_resolve _ _ = ()

      method! pp f =
        Fmt.pf f "remote-promise(%a) -> %a" Debug.OID.pp id (Struct_proxy.pp_state ~pp_promise) state

      method set_question q =
        self#update_target (Some q)

      method do_finish = function
        | None -> failwith "Not initialised!"
        | Some q -> Question.release q |> apply_question_actions t q

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
    Log.info (fun f -> f ~tags:t.tags "Sending disembargo %a" EP.Out.pp_disembargo_request request);
    t.queue_send (`Disembargo_request request)

  let bootstrap t =
    let result = make_remote_promise t in
    let question = Send.bootstrap t (result :> Core_types.struct_resolver) in
    result#set_question question;
    let qid = Question.id question in
    Log.info (fun f -> f ~tags:(with_qid qid t) "Sending: bootstrap");
    t.queue_send (`Bootstrap qid);
    let service = result#cap Wire.Path.root in
    result#finish;
    service

  let answer_promise answer = answer.answer_promise

  let reply_to_call t = function
    | `Bootstrap answer ->
      let promise = answer_promise answer in
      begin match t.bootstrap with
        | Some service ->
          service#inc_ref;
          promise#resolve (Ok (Wire.Response.bootstrap, RO_array.of_list [service]));
        | None ->
          promise#resolve (Error (Error.exn "No bootstrap service available"));
      end;
      Send.return t answer
    | `Call (answer, target, msg, caps) ->
      Log.info (fun f -> f ~tags:t.tags "Handling call: (%t).call %a" target#pp Core_types.Request_payload.pp (msg, caps));
      let resp = target#call msg caps in  (* Takes ownership of [caps]. *)
      target#dec_ref;
      (answer_promise answer)#connect resp;
      resp#when_resolved (fun _ -> Send.return t answer)

  class switchable init =
    let released = Core_types.broken_cap (Exception.v "(released)") in
    let pp_state f = function
      | `Unsettled (x, _) -> Fmt.pf f "(unsettled) -> %t" x#pp
      | `Set x -> Fmt.pf f "(set) -> %t" x#pp
    in
    let target = function
      | `Unsettled (x, _)
      | `Set x -> x
    in
    object (self : #Core_types.cap)
      inherit Core_types.ref_counted as super

      val id = Debug.OID.next ()

      val mutable state =
        `Unsettled (init, Queue.create ())

      method call msg caps =
        (target state)#call msg caps

      method resolve cap =
        self#check_refcount;
        match state with
        | `Set _ -> Debug.failf "Can't resolve already-set switchable %t to %t!" self#pp cap#pp
        | `Unsettled (old, q) ->
          state <- `Set cap;
          Queue.iter (fun f -> f (cap#inc_ref; cap)) q;
          old#dec_ref

      method private release =
        (target state)#dec_ref;
        state <- `Set released

      method shortest =
        match state with
        | `Unsettled _ -> (self :> Core_types.cap)     (* Can't shorten, as we may change later *)
        | `Set x -> x#shortest

      method blocker =
        match state with
        | `Unsettled _ -> Some (self :> Core_types.base_ref)
        | `Set x -> x#blocker

      method problem =
        match state with
        | `Unsettled _ -> None
        | `Set x -> x#problem

      method when_more_resolved fn =
        match state with
        | `Unsettled (_, q) -> Queue.add fn q
        | `Set x -> x#when_more_resolved fn

      (* When trying to find the target for export, it's OK to expose our current
         target, even though [shortest] shouldn't.
         In particular, we need this to deal with disembargo requests. *)
      method! sealed_dispatch : type a. a S.brand -> a option = function
        | CapTP -> (target state)#shortest#sealed_dispatch CapTP
        | x -> super#sealed_dispatch x

      method! check_invariants =
        super#check_invariants;
        match state with
        | `Unsettled (x, _) | `Set x -> x#check_invariants

      method pp f =
        Fmt.pf f "switchable(%a, %t) %a" Debug.OID.pp id super#pp_refcount pp_state state
    end

  module Input : sig
    open EP.Core_types

    val call : t -> In.QuestionId.t -> In.message_target ->
      Wire.Request.t -> In.desc RO_array.t -> results_to:In.send_results_to ->
      unit

    val bootstrap : t -> In.QuestionId.t -> unit
    val return : t -> In.AnswerId.t -> In.return -> release_param_caps:bool -> unit
    val finish : t -> In.QuestionId.t -> release_result_caps:bool -> unit
    val release : t -> In.ImportId.t -> ref_count:int -> unit
    val disembargo_request : t -> In.disembargo_request -> unit
    val disembargo_reply : t -> In.message_target -> Message_types.EmbargoId.t -> unit
    val resolve : t -> In.ExportId.t -> (In.desc, Exception.t) result -> unit
(*
    val provide : t -> In.QuestionId.t -> In.message_target -> recipient_id -> unit
    val accept : t -> In.QuestionId.t -> provision_id -> embargo:bool -> unit
    val join : t -> In.QuestionId.t -> In.message_target -> join_key_part -> unit
*)
  end = struct
    let with_inc_ref x =
      x#inc_ref;
      x

    let set_import_proxy t ~settled import =
      let message_target = `Import import in
      let cap =
        object (self : #Core_types.cap)
          inherit Core_types.ref_counted as super

          val id = Debug.OID.next ()

          method call msg caps =
            self#check_refcount;
            send_call t message_target msg caps

          method pp f =
            if settled then
              Fmt.pf f "far-ref(%a, %t) -> %a" Debug.OID.pp id self#pp_refcount pp_cap message_target
            else
              Fmt.pf f "remote-promise(%a, %t) -> %a" Debug.OID.pp id self#pp_refcount pp_cap message_target

          method private release =
            if import.import_proxy = `Uninitialised then (
              Log.info (fun f -> f ~tags:t.tags "Import proxy removed; can be caused by ref-counting bugs; \
                                                check for previous warning messages!");
            ) else (
              import.import_proxy_released <- true;
              maybe_release_import t import;
              import.import_proxy <- `Uninitialised
            )

          method shortest = self

          method blocker =
            if settled then None
            else Some (self :> Core_types.base_ref)

          method problem = None

          method when_more_resolved _ =
            assert settled      (* Otherwise, our switchable should have intercepted this *)

          method! sealed_dispatch : type a. a S.brand -> a option = function
            | CapTP -> Some (t, message_target)
            | x -> super#sealed_dispatch x
        end
      in
      (* Imports can resolve to another cap (if unsettled) or break. *)
      let switchable = new switchable cap in
      assert (import.import_proxy = `Uninitialised);
      if settled then (
        import.import_proxy <- `Settled (Weak_ptr.wrap switchable)
      ) else (
        import.import_proxy <- `Unsettled (Weak_ptr.wrap switchable)
      );
      switchable

    let import_sender t ~mark_dirty ~settled id =
      let new_import () =
        let import = {
          import_resolve_refs = 0;
          import_count = 1;
          import_id = id;
          import_proxy = `Uninitialised;
          import_proxy_released = false;
          import_used = mark_dirty;
        } in
        Imports.set t.imports id import;
        (set_import_proxy t ~settled import :> Core_types.cap)
      in
      match Imports.find t.imports id with
      | None -> new_import ()
      | Some import ->
        import.import_count <- import.import_count + 1;
        if mark_dirty then import.import_used <- true;
        match get_import_proxy import with
        | Some proxy ->
          proxy#inc_ref;
          (proxy :> Core_types.cap)
        | None ->
          (* The user let the proxy get GC'd without dropping the reference,
             but it hasn't been reported yet. It will try to release in a bit,
             (when next entering main loop, so not right now), and will see
             that the old struct's import proxy is Uninitialised and do nothing. *)
          import.import_count <- 0;
          import.import_proxy <- `Uninitialised;
          Imports.release t.imports import.import_id;
          new_import ()

    (* Create an embargo proxy for [x] and send a disembargo for it down [old_path]. *)
    let local_embargo t ~old_path x =
      let embargo = Cap_proxy.embargo x in
      (* Store in table *)
      embargo#inc_ref;
      let (embargo_id, _) = Embargoes.alloc t.embargoes (fun id -> (id, embargo)) in
      (* Send disembargo request *)
      let disembargo_request = `Loopback (old_path, embargo_id) in
      Log.info (fun f -> f ~tags:t.tags "Embargo %t until %a is delivered"
                   x#pp
                   EP.Out.pp_disembargo_request disembargo_request
               );
      (* We previously pipelined messages to [old_path], which now turns out to be
         local service [x]. We need to send a disembargo to clear the pipeline before
         using [x]. *)
      disembargo t disembargo_request;
      (embargo :> Core_types.cap)

    let maybe_embargo t ~old_path x =
      match old_path with
      | None -> x
      | Some old_path -> local_embargo t ~old_path x

    (* Turn a connection-scoped cap reference received from our peer into a general-purpose
       cap for users. The caller owns the new reference and should [dec_ref] it when done.
       If [embargo_path] is passed then we have already pipelined messages over this cap, and
       may therefore need to embargo it (sending the disembargo via the old [embargo_path]). *)
    let import t ?embargo_path : In.desc -> Core_types.cap = function
      | `SenderPromise id ->
        import_sender t id ~settled:false ~mark_dirty:(embargo_path <> None)
      | `SenderHosted id ->
        (* Spec says this is "newly exported", so how does the remote indicate an existing, settled export?
           We don't have to worry about embargoes here because anything we've already send to the peer will
           arrive before anything we send in the future, and it can't change. *)
        import_sender t id ~settled:true ~mark_dirty:false
      | `ReceiverHosted id ->
        let export = Exports.find_exn t.exports id in
        (* The sender's best guess at the time of sending was that we hosted the target.
           We may have since resolved it to point elsewhere, but the remote will return any disembargo
           request back to us. *)
        maybe_embargo t ~old_path:embargo_path (with_inc_ref export.export_service)
      | `ReceiverAnswer (id, path) ->
        let answer = Answers.find_exn t.answers id in
        let answer_promise = answer.answer_promise in
        begin match answer_promise#response with
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
            | Error (`Invalid_index _) -> answer_promise#cap path (* Don't embargo errors *)
            | Ok _ -> maybe_embargo t ~old_path:embargo_path (answer_promise#cap path)
        end
      | `None -> Core_types.null
      | `ThirdPartyHosted _ -> failwith "TODO: import"

    let call t aid (message_target : In.message_target) msg descs ~results_to =
      assert (results_to = `Caller);    (* TODO *)
      (* TODO: allowThirdPartyTailCall *)
      Log.info (fun f -> f ~tags:(with_aid aid t) "Received call to %a with args %a"
                   EP.In.pp_desc message_target
                   (RO_array.pp EP.In.pp_desc) descs
               );
      let answer_promise = Local_struct_promise.make () in
      let answer = {
        answer_id = aid;
        exports_for_release = [];
        answer_resolve_targets = RO_array.empty;
        answer_promise;
        answer_finished = false;
      } in
      Answers.set t.answers aid answer;
      let target =
        match message_target with
        | `ReceiverHosted id ->
          let export = Exports.find_exn t.exports id in
          with_inc_ref export.export_service
        | `ReceiverAnswer (id, path) ->
          let answer = Answers.find_exn t.answers id in
          answer.answer_promise#cap path
      in
      let caps = RO_array.map (import t) descs in
      reply_to_call t (`Call (answer, target, msg, caps))

    let bootstrap t id =
      let answer_promise = Local_struct_promise.make () in
      let answer = {
        answer_id = id;
        exports_for_release = [];
        answer_resolve_targets = RO_array.empty;
        answer_promise;
        answer_finished = false;
      } in
      Answers.set t.answers id answer;
      reply_to_call t (`Bootstrap answer)

    let release_resolve_target t = function
      | `None | `Local -> ()
      | `QuestionCap (q, _) -> Question.dec_ref q |> apply_question_actions t q;
      | `Import i ->
        i.import_resolve_refs <- i.import_resolve_refs - 1;
        maybe_release_import t i

    let release t export_id ~ref_count =
      assert (ref_count > 0);
      let export = Exports.find_exn t.exports export_id in
      assert (export.export_count >= ref_count);
      let count = export.export_count - ref_count in
      export.export_count <- count;
      if count = 0 then (
        Log.info (fun f -> f ~tags:t.tags "Releasing export %a" ExportId.pp export_id);
        Hashtbl.remove t.exported_caps export.export_service;
        Exports.release t.exports export_id;
        export.export_service#dec_ref;
        release_resolve_target t export.export_resolve_target
      )

    let return_results t question msg descrs =
      let caps_used = Question.caps_used ~msg question in
      let import_with_embargoes cap_index d =
        let embargo_path =
          match IntMap.find cap_index caps_used with
          | None -> None
          | Some path -> Some (Question.message_target question path)
        in
        import t d ?embargo_path
      in
      RO_array.mapi import_with_embargoes descrs

    let return t qid ret ~release_param_caps =
      let question = Questions.find_exn t.questions qid in
      let actions = Question.return question in
      if release_param_caps then List.iter (release t ~ref_count:1) question.params_for_release;
      begin match ret with
        | `Results (msg, descs) ->
          Log.info (fun f -> f ~tags:(with_qid qid t) "Received return results: %a"
                       (RO_array.pp In.pp_desc) descs
                   );
          let caps = return_results t question msg descs in
          Log.info (fun f -> f ~tags:(with_qid qid t) "Got results: %a"
                       Core_types.Response_payload.pp (msg, caps)
                   );
          apply_question_actions t question actions;
          Question.resolve question (Ok (msg, caps))
        | #Error.t as err ->
          Log.info (fun f -> f ~tags:(with_qid qid t) "Got error: %a" Error.pp err);
          apply_question_actions t question actions;
          Question.resolve question (Error err)
        | _ -> failwith "TODO: other return"
      end

    let finish t aid ~release_result_caps =
      let answer = Answers.find_exn t.answers aid in
      Log.info (fun f -> f ~tags:(with_aid aid t) "Received finish for %t" answer.answer_promise#pp);
      assert (not answer.answer_finished);
      answer.answer_finished <- true;
      Answers.release t.answers aid;
      if release_result_caps then List.iter (release t ~ref_count:1) answer.exports_for_release;
      answer.answer_promise#finish;
      RO_array.iter (release_resolve_target t) answer.answer_resolve_targets

    let send_disembargo t embargo_id target =
      let desc =
        match target with
        | `None -> Debug.failf "Protocol error: disembargo request for None cap"
        | `Local -> Debug.failf "Protocol error: disembargo request for local target"
        | `QuestionCap (question, path) -> Question.message_target question path
        | `Import import -> `ReceiverHosted import.import_id
      in
      Log.info (fun f -> f ~tags:t.tags "Sending disembargo response to %a" EP.Out.pp_desc desc);
      t.queue_send (`Disembargo_reply (desc, embargo_id))

    let disembargo_request t request =
      Log.info (fun f -> f ~tags:t.tags "Received disembargo request %a" EP.In.pp_disembargo_request request);
      match request with
      | `Loopback (old_path, embargo_id) ->
        match old_path with
        | `ReceiverHosted eid ->
          send_disembargo t embargo_id (Exports.find_exn t.exports eid).export_resolve_target;
        | `ReceiverAnswer (aid, path) ->
          let answer = Answers.find_exn t.answers aid in
          let answer_promise = answer.answer_promise in
          begin match answer_promise#response with
            | None -> failwith "Got disembargo for unresolved promise!"
            | Some (Error _) -> failwith "Got disembargo for exception!"
            | Some (Ok (msg, _)) ->
              match Core_types.Wire.Response.cap_index msg path with
              | Some i when i >= 0 && i < RO_array.length answer.answer_resolve_targets ->
                send_disembargo t embargo_id (RO_array.get answer.answer_resolve_targets i)
              | _ ->
                failwith "Got disembargo for invalid answer cap"
          end

    let disembargo_reply t target embargo_id =
      let embargo = snd (Embargoes.find_exn t.embargoes embargo_id) in
      Log.info (fun f -> f ~tags:t.tags "Received disembargo response %a -> %t"
                   EP.In.pp_desc target
                   embargo#pp);
      Log.info (fun f ->
          (* This is mainly to test that the target still exists, to catch bugs while fuzzing. *)
          f "Disembargo target is %t" @@
          match target with
          | `ReceiverHosted export_id -> (Exports.find_exn t.exports export_id).export_service#pp
          | `ReceiverAnswer (aid, _path) -> (Answers.find_exn t.answers aid).answer_promise#pp
        );
      Embargoes.release t.embargoes embargo_id;
      embargo#disembargo;
      embargo#dec_ref

    let resolve t import_id new_target =
      Log.info (fun f -> f ~tags:t.tags "Received resolve of import %a to %a"
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
        Log.info (fun f -> f ~tags:t.tags "Import %a no longer used - releasing new resolve target %t"
                     ImportId.pp import_id new_target#pp);
        new_target#dec_ref
      | Some im ->
        let import () =
          if im.import_used
          then import_new_target ~embargo_path:(Some (`ReceiverHosted import_id))
          else import_new_target ~embargo_path:None
        in
        match im.import_proxy, new_target with
        | `Uninitialised, _ -> assert false
        | `Settled x, Ok _ ->
          let new_target = import () in
          Debug.failf "Got a Resolve (to %t) for settled import %a!"
            new_target#pp pp_weak (Weak_ptr.get x)
        | `Settled x, _
        | `Unsettled x, _ ->
          (* This will also dec_ref the old remote-promise, releasing the import. *)
          match Weak_ptr.get x with
          | Some x -> x#resolve (import ())
          | None ->
            (* We can only get here if the user messed up their ref-counting, but try to handle it. *)
            let new_target = import_new_target ~embargo_path:None in
            Log.info (fun f -> f ~tags:t.tags "Import %a was GC'd! Releasing new resolve target %t"
                         ImportId.pp import_id new_target#pp);
            new_target#dec_ref

(* TODO:
    let provide _t _question_id _message_target _recipient_id = ()
    let accept _t _question_id _provision_id ~embargo:_ = ()
    let join _t _question_id _message_target _join_key_part = ()
*)

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
    | `Bootstrap qid ->
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

  let handle_msg t (msg : [<In.t | `Unimplemented of Out.t]) =
    check_connected t;
    match msg with
    | `Call (aid, target,
             msg, descs, results_to)  -> Input.call t aid target msg descs ~results_to
    | `Bootstrap x                    -> Input.bootstrap t x
    | `Return (aid, ret, release)     -> Input.return t aid ret ~release_param_caps:release
    | `Finish (aid, release)          -> Input.finish t aid ~release_result_caps:release
    | `Release (id, count)            -> Input.release t id ~ref_count:count
    | `Disembargo_request req         -> Input.disembargo_request t req
    | `Disembargo_reply (target, id)  -> Input.disembargo_reply t target id
    | `Resolve (id, target)           -> Input.resolve t id target
    | `Unimplemented x                -> handle_unimplemented t x

  let dump_embargo f (id, proxy) =
    Fmt.pf f "%a: @[%t@]" EmbargoId.pp id proxy#pp

  let check_import x =
    if not x.import_proxy_released then (
      match get_import_proxy x with
      | Some x -> x#check_invariants
      | None -> failwith "Import proxy GC'd, but not removed from table!"
    )

  let check_export   x = x.export_service#check_invariants

  let check_answer   x = x.answer_promise#check_invariants

  let check_embargo  x = (snd x)#check_invariants

  let check_exported_cap t cap export_id =
    match Exports.find_exn t.exports export_id with
    | export ->
      if export.export_service <> cap then (
        Debug.invariant_broken @@ fun f ->
        Fmt.pf f "export_caps maps %t to export %a back to different cap %t!"
          cap#pp ExportId.pp export_id export.export_service#pp
      )
    | exception ex ->
      Debug.invariant_broken @@ fun f ->
      Fmt.pf f "exported_caps for %t: %a" cap#pp Debug.pp_exn ex

  let exported_sort_key export_id = export_id

  let pp_exported_cap t f (cap, export_id) =
    Fmt.pf f "%t => export %a%a" cap#pp ExportId.pp export_id (pp_check (check_exported_cap t)) (cap, export_id)

  let dump f t =
    Fmt.pf f "@[<v2>Questions:@,%a@]@,\
              @[<v2>Answers:@,%a@]@,\
              @[<v2>Exports:@,%a@]@,\
              @[<v2>Imports:@,%a@]@,\
              @[<v2>Embargoes:@,%a@]@,\
              @[<v2>Exported caps:@,%a@]@,"
      (Questions.dump ~check:Question.check Question.dump) t.questions
      (Answers.dump   ~check:check_answer   dump_answer) t.answers
      (Exports.dump   ~check:check_export   dump_export) t.exports
      (Imports.dump   ~check:check_import   dump_import) t.imports
      (Embargoes.dump ~check:check_embargo  dump_embargo) t.embargoes
      (hashtbl_dump ~key:exported_sort_key (pp_exported_cap t)) t.exported_caps

  let check t =
    Questions.iter  (fun _ -> Question.check) t.questions;
    Answers.iter    (fun _ -> check_answer)   t.answers;
    Imports.iter    (fun _ -> check_import)   t.imports;
    Exports.iter    (fun _ -> check_export)   t.exports;
    Embargoes.iter  (fun _ -> check_embargo)  t.embargoes;
    Hashtbl.iter    (check_exported_cap t) t.exported_caps

  let disconnect t ex =
    check_connected t;
    t.disconnected <- Some ex;
    begin match t.bootstrap with
    | None -> ()
    | Some b -> t.bootstrap <- None; b#dec_ref
    end;
    t.queue_send <- ignore;
    Exports.drop_all t.exports (fun _ e -> e.export_service#dec_ref);
    Hashtbl.clear t.exported_caps;
    Questions.drop_all t.questions (fun _ -> Question.lost_connection ~ex);
    Answers.drop_all t.answers (fun _ a -> a.answer_promise#finish);
    let broken_cap = Core_types.broken_cap ex in
    Imports.drop_all t.imports (fun _ i ->
        begin match get_import_proxy i with
          | Some switchable when switchable#problem = None -> switchable#resolve broken_cap
          | _ -> ()
        end;
        i.import_proxy <- `Uninitialised
      );
    Embargoes.drop_all t.embargoes (fun _ (_, e) -> e#break ex; e#dec_ref);
    (* TODO: break existing caps *)
    ()
end
