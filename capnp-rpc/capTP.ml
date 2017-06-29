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

  type question = {
    question_id : QuestionId.t;
    question_data : Core_types.struct_resolver;         (* TODO: should be a weak ref *)
    mutable question_flags : int;
    mutable params_for_release : ExportId.t list;
    mutable question_pipelined_fields : PathSet.t; (* Fields used while unresolved; will need embargoes *)
  }

  type answer = {
    answer_id : AnswerId.t;
    mutable exports_for_release : ExportId.t list;
    answer_promise : Core_types.struct_resolver;

    mutable answer_state : [`Finished | `Not_finished of Cap_proxy.embargo_cap Queue.t];
    (* If not finished, there may be some embargoes that need to be lifted when finished. *)
  }

  let flag_returned = 1
  let flag_finished = 2

  type export = {
    export_id : ExportId.t;
    mutable export_count : int; (* Number of times sent to remote and not yet released *)
    export_service : Core_types.cap;
  }

  type import = {
    import_id : ImportId.t;
    mutable import_count : int; (* Number of times remote sent us this *)
    mutable import_used : bool; (* We have sent a message to this target *)
    mutable import_proxy : [    (* TODO: should be a weak ref *)
      | `Uninitialised
      | `Settled of Core_types.cap
      | `Unsettled of Cap_proxy.resolver_cap   (* Note: might be resolved to a settled value *)
    ]
  }

  type message_target_cap = [
    | `Import of import
    | `QuestionCap of question * Wire.Path.t
  ]

  type descr = [
    message_target_cap
    | `ThirdPartyHosted of Out.third_party_desc
    | `Local of Core_types.cap
  ]

  let pp_cap : [< descr] Fmt.t = fun f -> function
    | `Import import -> Fmt.pf f "Import:%a" ImportId.pp import.import_id
    | `QuestionCap (question, p) -> Fmt.pf f "QuestionCap:%a[%a]" QuestionId.pp question.question_id Wire.Path.pp p
    | `ThirdPartyHosted _third_party_desc -> Fmt.pf f "ThirdPartyHosted"
    | `Local local -> Fmt.pf f "Local:%t" local#pp

  type t = {
    mutable queue_send : (EP.Out.t -> unit);    (* (mutable for shutdown) *)
    tags : Logs.Tag.set;
    embargoes : (EmbargoId.t * Cap_proxy.embargo_cap) Embargoes.t;
    mutable bootstrap : Core_types.cap option; (* (mutable for shutdown) *)

    questions : question Questions.t;
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
    | `Uninitialised -> assert false
    | `Settled p -> p
    | `Unsettled p -> (p :> Core_types.cap)

  let pp_question f q =
    Fmt.pf f "q%a" QuestionId.pp q.question_id

  let dump_question f q =
    Fmt.pf f "%t" q.question_data#pp

  let dump_answer f x =
    Fmt.pf f "%t" x.answer_promise#pp

  let dump_export f x =
    Fmt.pf f "%t" x.export_service#pp

  let dump_import f x =
    Fmt.pf f "%t" (get_import_proxy x)#pp

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
      queue_send;
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

  let tags ?qid ?aid t =
    match qid, aid with
    | None, None -> t.tags
    | Some qid, None -> Logs.Tag.add Debug.qid_tag (QuestionId.uint32 qid) t.tags
    | None, Some aid -> Logs.Tag.add Debug.qid_tag (EP.Table.AnswerId.uint32 aid) t.tags
    | Some _, Some _ -> assert false

  let to_cap_desc t (cap : Core_types.cap) =
    let cap = cap#shortest in
    match my_target_of t cap with
    | None -> `Local cap
    | Some x -> (x :> [`Local of Core_types.cap | message_target_cap])

  let pp_promise f = function
    | Some (q, _) -> pp_question f q
    | None -> Fmt.string f "(not initialised)"

  let maybe_release_question t question =
    if question.question_flags - flag_returned - flag_finished = 0 then (
      Questions.release t.questions question.question_id
    )

  let check_connected t =
    match t.disconnected with
    | None -> ()
    | Some ex -> Debug.failf "CapTP connection is disconnected (%a)" Exception.pp ex

  module Send : sig
    (** Converts struct pointers into integer table indexes, ready for sending.
        The indexes are only valid until the next message is sent. *)

    open EP.Core_types

    val bootstrap : t -> struct_resolver -> question * Out.QuestionId.t
    val call : t -> struct_resolver -> message_target_cap -> [< descr] RO_array.t ->
      question * Out.QuestionId.t * Out.message_target * Out.desc RO_array.t

    val return_results : t -> answer -> Wire.Response.t -> [< descr] RO_array.t -> Out.AnswerId.t * Out.return
    val return_error : t -> answer -> Error.t -> Out.AnswerId.t * Out.return

    val release : t -> import -> Out.ImportId.t * int
    (** [release t i] indicates that [i] is no longer used by the client.
        Returns the [referenceCount] for the Release message. *)

    val finish : t -> question -> Out.QuestionId.t
    (** [finish t qid] tells the system that we're about to send a Finish message
        (with [releaseResultCaps=false]). *)

    val disembargo_reply : t -> message_target_cap -> Out.message_target
  end = struct

    (** [export t target] is a descriptor for [target], plus a list of exports that
        should be freed if we get a request to free all capabilities associated with
        the request.
        TODO: do we need this? The [answer] record presumably holds the things we need. *)
    let rec export : 'a. t -> ([< descr] as 'a) -> Out.desc * ExportId.t list = fun t -> function
      | `Import import ->
        (* Any ref-counting needed here? *)
        `ReceiverHosted import.import_id, []
      | `QuestionCap (question, i) ->
        `ReceiverAnswer (question.question_id, i), []
      | `ThirdPartyHosted _ -> failwith "TODO: export ThirdPartyHosted"
      | `Local service ->
        let service = service#shortest in
        let settled = service#blocker = None in
        let ex =
          match Hashtbl.find t.exported_caps service with
          | id ->
            let ex = Exports.find_exn t.exports id in
            ex.export_count <- ex.export_count + 1;
            ex
          | exception Not_found ->
            service#inc_ref;
            let ex = Exports.alloc t.exports (fun export_id ->
                { export_count = 1; export_service = service; export_id }
              )
            in
            let id = ex.export_id in
            Hashtbl.add t.exported_caps service id;
            if not settled then (
              Log.info (fun f -> f ~tags:t.tags "Monitoring promise export %a -> %a" ExportId.pp ex.export_id dump_export ex);
              service#when_more_resolved (fun x ->
                  if ex.export_count > 0 then (
                    (* TODO: resolves to broken? *)
                    let new_export, _ = export t (to_cap_desc t x) in
                    Log.info (fun f -> f ~tags:t.tags "Export %a resolved to %t - sending notification to use %a"
                                 ExportId.pp ex.export_id
                                 x#pp
                                 Out.pp_desc new_export
                             );
                    t.queue_send (`Resolve (ex.export_id, Ok new_export));
                  ); (* else: no longer an export *)
                  x#dec_ref
                );
            );
            ex
        in
        let id = ex.export_id in
        let descr =
          if settled then `SenderHosted id
          else `SenderPromise id
        in
        descr, [id]

    let bootstrap t question_data =
      check_connected t;
      let question =
        Questions.alloc t.questions (fun question_id ->
            {question_flags = 0; params_for_release = []; question_id; question_data; question_pipelined_fields = PathSet.empty}
          )
      in
      question, question.question_id

    let call t question_data (target : message_target_cap) caps =
      let question = Questions.alloc t.questions (fun question_id ->
          {question_flags = 0; params_for_release = []; question_id; question_data; question_pipelined_fields = PathSet.empty}
        )
      in
      let descrs =
        caps |> RO_array.map (fun cap ->
            let descr, to_release = export t cap in
            question.params_for_release <- to_release @ question.params_for_release;
            descr
          )
      in
      let target =
        match target with
        | `Import import ->
          import.import_used <- true;
          `ReceiverHosted import.import_id
        | `QuestionCap (question, i) ->
          question.question_pipelined_fields <- PathSet.add i question.question_pipelined_fields;
          `ReceiverAnswer (question.question_id, i)
      in
      question, question.question_id, target, descrs

    let return_results t answer msg (caps : [< descr] RO_array.t) =
      let result =
        if answer.answer_state = `Finished then `Cancelled
        else (
          let descrs =
            caps |> RO_array.map (fun cap ->
                let descr, to_release = export t cap in
                answer.exports_for_release <- to_release @ answer.exports_for_release;
                descr
              )
          in
          `Results (msg, descrs)
        )
      in
      answer.answer_id, result

    let return_error _t answer err =
      answer.answer_id, (err : Error.t :> Out.return)

    let release t import =
      Imports.release t.imports import.import_id;
      let count = import.import_count in
      import.import_count <- 0;       (* Just in case - mark as invalid *)
      import.import_id, count

    let finish t question =
      let flags = question.question_flags in
      assert (flags land flag_finished = 0);
      let flags = flags + flag_finished in
      question.question_flags <- flags;
      maybe_release_question t question;
      question.question_id

    let disembargo_reply _t = function
      | `Import import -> `ReceiverHosted import.import_id
      | `QuestionCap (question, path) -> `ReceiverAnswer (question.question_id, path)
  end

  type target = (question * unit Lazy.t) option  (* question, finish *)

  (* Note: takes ownership of [caps] *)
  let rec send_call t target msg caps =
    let result = make_remote_promise t in
    let con_caps = RO_array.map (to_cap_desc t) caps in
    let question, qid, message_target, descs = Send.call t (result :> Core_types.struct_resolver) target con_caps in
    Log.info (fun f -> f ~tags:(tags ~qid t) "Sending: (%a).call %a"
                 pp_cap target
                 Core_types.Request_payload.pp (msg, caps));
    result#set_question question;
    t.queue_send (`Call (qid, message_target, msg, descs));
    RO_array.iter dec_ref caps;
    (result :> Core_types.struct_ref)

  (* A cap that sends to a promised answer's cap at other *)
  and make_remote_promise t =
    object (self : #Core_types.struct_resolver)
      inherit [target] Struct_proxy.t None

      method do_pipeline question i msg caps =
        match question with
        | Some (target_q, _) ->
          let target = `QuestionCap (target_q, i) in
          send_call t target msg caps
        | None -> failwith "Not initialised!"

      method on_resolve q _ =
        match q with
        | Some (_target_q, finish) -> Lazy.force finish
        | None -> failwith "Not initialised!"

      method! pp f =
        Fmt.pf f "remote-promise(%a) -> %a" Debug.OID.pp id (Struct_proxy.pp_state ~pp_promise) state

      method set_question q =
        let finish = lazy (
          let qid = Send.finish t q in
          Log.info (fun f -> f ~tags:(tags ~qid t) "Send finish %t" self#pp);
          t.queue_send (`Finish (qid, false));
        ) in
        self#update_target (Some (q, finish))

      method do_finish = function
        | Some (_, finish) -> Lazy.force finish
        | None -> failwith "Not initialised!"

      method field_sealed_dispatch : type a. Wire.Path.t -> a S.brand -> a option = fun path -> function
        | CapTP ->
          begin match state with
            | Unresolved u ->
              begin match u.target with
                | None -> failwith "Not intialised!"
                | Some (target_q, _) -> Some (t, `QuestionCap (target_q, path))
              end
            | _ -> failwith "Not a promise!"
          end;
        | _ -> None
    end

  let reply_to_disembargo t target embargo_id =
    let target = Send.disembargo_reply t target in
    Log.info (fun f -> f ~tags:t.tags "Sending disembargo response to %a" EP.Out.pp_desc target);
    t.queue_send (`Disembargo_reply (target, embargo_id))

  let disembargo t request =
    Log.info (fun f -> f ~tags:t.tags "Sending disembargo %a" EP.Out.pp_disembargo_request request);
    t.queue_send (`Disembargo_request request)

  let bootstrap t =
    let result = make_remote_promise t in
    let question, qid = Send.bootstrap t (result :> Core_types.struct_resolver) in
    result#set_question question;
    Log.info (fun f -> f ~tags:(tags ~qid t) "Sending: bootstrap");
    t.queue_send (`Bootstrap qid);
    let service = result#cap Wire.Path.root in
    result#finish;
    service

  let answer_promise answer = answer.answer_promise

  let return_results t answer =
    let aid, ret =
      let answer_promise = answer_promise answer in
      match answer_promise#response with
      | None -> assert false
      | Some (Ok (msg, caps)) ->
        RO_array.iter (fun c -> c#inc_ref) caps;        (* Copy everything stored in [answer]. *)
        let con_caps = RO_array.map (to_cap_desc t) caps in
        let aid, ret = Send.return_results t answer msg con_caps in
        Log.info (fun f -> f ~tags:(tags ~aid t) "Returning results: %a"
                     Core_types.Response_payload.pp (msg, caps));
        RO_array.iter dec_ref caps;
        Log.info (fun f -> f ~tags:(tags ~aid t) "Wire results: %a" Out.pp_return ret);
        aid, ret
      | Some (Error err) ->
        let aid, ret = Send.return_error t answer err in
        Log.info (fun f -> f ~tags:(tags ~aid t) "Returning error: %a" Error.pp err);
        aid, ret
    in
    t.queue_send (`Return (aid, ret))

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
      return_results t answer
    | `Call (answer, target, msg, caps) ->
      Log.info (fun f -> f ~tags:t.tags "Handling call: (%t).call %a" target#pp Core_types.Request_payload.pp (msg, caps));
      let resp = target#call msg caps in  (* Takes ownership of [caps]. *)
      target#dec_ref;
      (answer_promise answer)#connect resp;
      resp#when_resolved (fun _ -> return_results t answer)

  class switchable init =
    (* We initially forward to an unsettled promise (otherwise there's no point using a switchable).
       Later, we resolve and become set. The resolution may be settled or unsettled. *)
    let released = Core_types.broken_cap (Exception.v "(released)") in
    let is_settled x = (x#blocker = None) in
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
        assert (not (is_settled init));
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

    val call : t -> In.QuestionId.t -> In.message_target -> Wire.Request.t -> In.desc RO_array.t -> unit
    val bootstrap : t -> In.QuestionId.t -> unit
    val return : t -> In.AnswerId.t -> In.return -> unit
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
               Fmt.pf f "remote-promise(%a, %t) -> %a"Debug.OID.pp id self#pp_refcount pp_cap message_target

          method private release =
            Log.info (fun f -> f ~tags:t.tags "Sending release %t" self#pp);
            let id, count = Send.release t import in
            t.queue_send (`Release (id, count))

          method shortest = self

          method blocker =
            if settled then None
            else Some (self :> Core_types.base_ref)

          method when_more_resolved _ =
            assert settled      (* Otherwise, our switchable should have intercepted this *)

          method! sealed_dispatch : type a. a S.brand -> a option = function
            | CapTP -> Some (t, message_target)
            | x -> super#sealed_dispatch x
        end
      in
      assert (import.import_proxy = `Uninitialised);
      if settled then
        import.import_proxy <- `Settled cap
      else
        import.import_proxy <- `Unsettled (new switchable cap)

    let import_sender t ~mark_dirty ~settled id =
      match Imports.find t.imports id with
      | Some import ->
        import.import_count <- import.import_count + 1;
        if mark_dirty then import.import_used <- true;
        let proxy = get_import_proxy import in
        proxy#inc_ref;
        proxy
      | None ->
        let import = { import_count = 1; import_id = id; import_proxy = `Uninitialised; import_used = mark_dirty } in
        Imports.set t.imports id import;
        set_import_proxy t ~settled import;
        get_import_proxy import

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
       may therefore need to embargo it (sending the disembargo via the old [embargo_path]).
    *)
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
           However, we may have since resolved it to point elsewhere. We only send one
           Resolve per export, so the cases are:
           - We didn't send a Resolve yet -> send a local embargo
           - We did send a Resolve -> find out what we said the new target was TODO
         *)
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
            (* We've already replied to this question. Decide what to do about embargoes.
               The cases are:
               - The field is hosted here. Treat as for ReceiverHosted. TODO
               - The field we want isn't in the payload. We can't embargo, because
                 the peer has no way to return the response, but we don't need to for errors.
               - The field is hosted at the peer. There might be messages we pipelined to it
                 currently heading back to us. Embargo until we get the finish message - nothing
                 can arrive after that.
               - The field is hosted elsewhere (level 3 only). TODO
            *)
            match Core_types.Response_payload.field payload path with
            | Error (`Invalid_index _) -> answer_promise#cap path (* Don't embargo errors *)
            | Ok cap ->
              let cap = with_inc_ref cap#shortest in
              match my_target_of t cap with
              | None ->
                (* Hosted locally *)
                maybe_embargo t ~old_path:embargo_path cap
              | Some _ ->
                (* Hosted at peer *)
                match answer.answer_state with
                | `Finished -> failwith "Bug: finished answer in answers table!"
                | `Not_finished disembargo_on_finish ->
                  let embargo = Cap_proxy.embargo cap in
                  embargo#inc_ref; Queue.add embargo disembargo_on_finish;
                  (embargo :> Core_types.cap)
        end
      | `None -> Core_types.null
      | `ThirdPartyHosted _ -> failwith "TODO: import"

    (* Embargo [x] if [cap_index] is in [caps_used], sending the disembargo request via [qid]. *)
    let call t aid (message_target : In.message_target) msg descs =
      (* TODO: allowThirdPartyTailCall, sendResultsTo *)
      Log.info (fun f -> f ~tags:(tags ~aid t) "Received call to %a with args %a"
                   EP.In.pp_desc message_target
                   (RO_array.pp EP.In.pp_desc) descs
               );
      let answer_promise = Local_struct_promise.make () in
      let answer = {
        answer_id = aid;
        exports_for_release = [];
        answer_promise;
        answer_state = `Not_finished (Queue.create ());
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
        answer_promise;
        answer_state = `Not_finished (Queue.create ());
      } in
      Answers.set t.answers id answer;
      reply_to_call t (`Bootstrap answer)

    let return_generic t id ~release_param_caps =
      ignore release_param_caps; (* TODO *)
      let question = Questions.find_exn t.questions id in
      let flags = question.question_flags in
      assert (flags land flag_returned = 0);
      let flags = flags + flag_returned in
      question.question_flags <- flags;
      maybe_release_question t question;
      question

    let return_error t id ~release_param_caps =
      let question = return_generic t id ~release_param_caps in
      question.question_data

    let return_results t qid ~release_param_caps msg descrs =
      let question = return_generic t qid ~release_param_caps in
      let caps_used = (* Maps used cap indexes to their paths *)
        PathSet.elements question.question_pipelined_fields
        |> filter_map (fun path ->
            match Wire.Response.cap_index msg path with
            | None -> None
            | Some i -> Some (i, path)
          )
        |> IntMap.of_list
      in
      let import_with_embargoes cap_index d =
        let embargo_path =
          match IntMap.find cap_index caps_used with
          | None -> None
          | Some path -> Some (`ReceiverAnswer (qid, path))
        in
        import t d ?embargo_path
      in
      let caps = RO_array.mapi import_with_embargoes descrs in
      question.question_data, caps

    let return t qid ret =
      match ret with
      | `Results (msg, descs) ->
        Log.info (fun f -> f ~tags:(tags ~qid t) "Received return results: %a"
                     (RO_array.pp In.pp_desc) descs
                 );
        let result, caps = return_results t qid msg descs ~release_param_caps:false in
        Log.info (fun f -> f ~tags:(tags ~qid t) "Got results: %a"
                     Core_types.Response_payload.pp (msg, caps)
                 );
        result#resolve (Ok (msg, caps))
      | #Error.t as err ->
        let result = return_error t qid ~release_param_caps:false in
        Log.info (fun f -> f ~tags:(tags ~qid t) "Got error: %a" Error.pp err);
        result#resolve (Error err)
      | _ -> failwith "TODO: other return"

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
        export.export_service#dec_ref
      )

    let finish t aid ~release_result_caps =
      let answer = Answers.find_exn t.answers aid in
      Log.info (fun f -> f ~tags:(tags ~aid t) "Received finish for %t" answer.answer_promise#pp);
      match answer.answer_state with
      | `Finished -> assert false
      | `Not_finished disembargo_on_finish ->
        answer.answer_state <- `Finished;
        Queue.iter (fun e -> e#disembargo; e#dec_ref) disembargo_on_finish;
        Answers.release t.answers aid;
        if release_result_caps then (
          (* This is very unclear. It says "all capabilities that were in the
             results should be considered released". However, only imports can
             be released, not capabilities in general.
             Also, assuming we decrement the ref count by one in this case. *)
          List.iter (release t ~ref_count:1) answer.exports_for_release
        );
        answer.answer_promise#finish

    let disembargo_request t request =
      Log.info (fun f -> f ~tags:t.tags "Received disembargo request %a" EP.In.pp_disembargo_request request);
      match request with
      | `Loopback (old_path, id) ->
        let cap =
          match old_path with
          | `ReceiverHosted eid -> (Exports.find_exn t.exports eid).export_service
          | `ReceiverAnswer (aid, path) ->
            let answer = Answers.find_exn t.answers aid in
            let answer_promise = answer.answer_promise in
            begin match answer_promise#response with
              | None -> failwith "Got disembargo for unresolved promise!"
              | Some (Error _) -> failwith "Got disembargo for exception!"
              | Some (Ok payload) ->
                match Core_types.Response_payload.field payload path with
                | Ok cap -> cap
                | Error (`Invalid_index i) ->
                  (* The peer must have sent the answer before the disembargo request. *)
                  Debug.failf "Protocol error: peer asked to disembargo a capability index (%d) knew didn't exist" i
            end
        in
        (* Check that [cap] points back at sender. *)
        match my_target_of t cap with
        | Some (`Import _ | `QuestionCap _ as target) -> reply_to_disembargo t target id
        | None -> Debug.failf "Protocol error: disembargo request for invalid target %t" cap#pp

    let disembargo_reply t target embargo_id =
      let embargo = snd (Embargoes.find_exn t.embargoes embargo_id) in
      Log.info (fun f -> f ~tags:t.tags "Received disembargo response %a -> %t"
                   EP.In.pp_desc target
                   embargo#pp);
      Embargoes.release t.embargoes embargo_id;
      embargo#disembargo;
      embargo#dec_ref

    let resolve t import_id new_target =
      Log.info (fun f -> f ~tags:t.tags "Received resolve of import %a to %a"
                   ImportId.pp import_id
                   (Fmt.result ~ok:In.pp_desc ~error:Exception.pp) new_target
               );
      let new_target ~embargo_path =
        match new_target with
        | Error e -> Core_types.broken_cap e
        | Ok desc -> import t desc ?embargo_path
      in
      match Imports.find t.imports import_id with
      | None ->
        let new_target = new_target ~embargo_path:None in
        Log.info (fun f -> f ~tags:t.tags "Import %a no longer used - releasing new resolve target %t"
                     ImportId.pp import_id new_target#pp);
        new_target#dec_ref
      | Some im ->
        let new_target =
          if im.import_used
          then new_target ~embargo_path:(Some (`ReceiverHosted import_id))
          else new_target ~embargo_path:None
        in
        match im.import_proxy with
        | `Uninitialised -> assert false
        | `Settled x -> Debug.failf "Got a Resolve (to %t) for settled import %t!" new_target#pp x#pp
        | `Unsettled x ->
          (* This will also dec_ref the old remote-promise, releasing the import. *)
          x#resolve new_target

(* TODO:
    let provide _t _question_id _message_target _recipient_id = ()
    let accept _t _question_id _provision_id ~embargo:_ = ()
    let join _t _question_id _message_target _join_key_part = ()
*)

  end

  let handle_msg t (msg : EP.In.t) =
    check_connected t;
    match msg with
    | `Call (aid, target, msg, descs) -> Input.call t aid target msg descs
    | `Bootstrap x                    -> Input.bootstrap t x
    | `Return (aid, ret)              -> Input.return t aid ret
    | `Finish (aid, release)          -> Input.finish t aid ~release_result_caps:release
    | `Release (id, count)            -> Input.release t id ~ref_count:count
    | `Disembargo_request req         -> Input.disembargo_request t req
    | `Disembargo_reply (target, id)  -> Input.disembargo_reply t target id
    | `Resolve (id, target)           -> Input.resolve t id target

  let dump_embargo f (id, proxy) =
    Fmt.pf f "%a: @[%t@]" EmbargoId.pp id proxy#pp

  let check_import   x = (get_import_proxy x)#check_invariants
  let check_export   x = x.export_service#check_invariants
  let check_question x = x.question_data#check_invariants
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
      (Questions.dump ~check:check_question dump_question) t.questions
      (Answers.dump   ~check:check_answer   dump_answer) t.answers
      (Exports.dump   ~check:check_export   dump_export) t.exports
      (Imports.dump   ~check:check_import   dump_import) t.imports
      (Embargoes.dump ~check:check_embargo  dump_embargo) t.embargoes
      (hashtbl_dump ~key:exported_sort_key (pp_exported_cap t)) t.exported_caps

  let check t =
    Questions.iter  (fun _ -> check_question) t.questions;
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
    Questions.drop_all t.questions (fun _ q ->
        if q.question_flags land flag_finished = 0 then q.question_data#resolve (Error (`Exception ex));
      );
    Answers.drop_all t.answers (fun _ a -> a.answer_promise#finish);
    Imports.drop_all t.imports (fun _ i -> i.import_proxy <- `Uninitialised);
    Embargoes.drop_all t.embargoes (fun _ (_, e) -> e#break ex; e#dec_ref);
    (* TODO: break existing caps *)
    ()
end
