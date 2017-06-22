open Asetmap

module Log = Debug.Log
module IntMap = Map.Make(struct type t = int let compare (a:int) b = compare a b end)

let rec filter_map f = function
  | [] -> []
  | x :: xs ->
    match f x with
    | None -> filter_map f xs
    | Some y -> y :: filter_map f xs

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
    question_data : Core_types.struct_resolver;
    mutable question_flags : int;
    mutable params_for_release : ExportId.t list;
    mutable question_pipelined_fields : PathSet.t; (* Fields used while unresolved; will need embargoes *)
  }

  type answer = {
    answer_id : AnswerId.t;
    mutable exports_for_release : ExportId.t list;
    answer_promise : Core_types.struct_resolver;
    mutable finished : bool;
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
    mutable import_proxy : Core_types.cap option;
  }

  type message_target_cap = [
    | `ReceiverHosted of import
    | `ReceiverAnswer of question * Wire.Path.t
  ]

  type descr = [
    message_target_cap
    | `None
    | `SenderHosted of export
    | `SenderPromise of export
    | `ThirdPartyHosted of Out.third_party_desc
    | `Local of Core_types.cap
  ]

  type recv_descr = [
    message_target_cap
    | `None
    | `ThirdPartyHosted of Out.third_party_desc
    | `Local of Core_types.cap
    | `LocalPromise of Core_types.struct_resolver * Wire.Path.t
  ]

  type recv_cap_with_embargo = [
    recv_descr
    | `LocalEmbargo of Core_types.cap * Out.disembargo_request   (* Send a Disembargo, and queue messages until it returns *)
  ]

  let pp_cap : [< descr | recv_cap_with_embargo] Fmt.t = fun f -> function
    | `None -> Fmt.pf f "null"
    | `ReceiverHosted import -> Fmt.pf f "ReceiverHosted:%a" ImportId.pp import.import_id
    | `ReceiverAnswer (question, p) -> Fmt.pf f "ReceiverAnswer:%a[%a]" QuestionId.pp question.question_id Wire.Path.pp p
    | `ThirdPartyHosted _third_party_desc -> Fmt.pf f "ThirdPartyHosted"
    | `SenderHosted export -> Fmt.pf f "SenderHosted:%a" ExportId.pp export.export_id
    | `SenderPromise export -> Fmt.pf f "SenderPromise:%a" ExportId.pp export.export_id
    | `Local local -> Fmt.pf f "Local:%t" local#pp
    | `LocalPromise (_promise, path) -> Fmt.pf f "LocalPromise:<p>%a" Wire.Path.pp path
    | `LocalEmbargo (local_service, req) -> Fmt.pf f "LocalEmbargo:%t:%a" local_service#pp Out.pp_disembargo_request req

  type t = {
    queue_send : (EP.Out.t -> unit);
    ours : (Core_types.cap, message_target_cap) Hashtbl.t;              (* TODO: use weak table *)
    tags : Logs.Tag.set;
    embargoes : (EmbargoId.t * Cap_proxy.embargo_cap) Embargoes.t;
    bootstrap : Core_types.cap option;

    questions : question Questions.t;
    answers : answer Answers.t;
    exports : export Exports.t;
    imports : import Imports.t;
    wrapper : (Core_types.cap, ExportId.t) Hashtbl.t;
  }

  let pp_question f q =
    Fmt.pf f "q%a" QuestionId.pp q.question_id

  let dump_question f q =
    Fmt.pf f "%t" q.question_data#pp

  let dump_answer f x =
    Fmt.pf f "%t" x.answer_promise#pp

  let dump_export f x =
    Fmt.pf f "%t" x.export_service#pp

  let dump_import f _x =
    Fmt.pf f "import"

  let stats t =
    { Stats.
      n_questions = Questions.active t.questions;
      n_answers = Answers.active t.answers;
      n_imports = Imports.active t.imports;
      n_exports = Exports.active t.exports;
    }

  let create ?bootstrap ~tags ~queue_send =
    {
      queue_send;
      ours = Hashtbl.create 10;
      tags;
      bootstrap;
      questions = Questions.make ();
      answers = Answers.make ();
      imports = Imports.make ();
      exports = Exports.make ();
      embargoes = Embargoes.make ();
      wrapper = Hashtbl.create 30;
    }

  let tags ?qid ?aid t =
    match qid, aid with
    | None, None -> t.tags
    | Some qid, None -> Logs.Tag.add Debug.qid_tag (QuestionId.uint32 qid) t.tags
    | None, Some aid -> Logs.Tag.add Debug.qid_tag (EP.Table.AnswerId.uint32 aid) t.tags
    | Some _, Some _ -> assert false

  let register t x y =
    match Hashtbl.find t.ours x with
    | exception Not_found -> Hashtbl.add t.ours x y
    | existing -> assert (y = existing)

  let unwrap t x =
    try Some (Hashtbl.find t.ours x)
    with Not_found -> None

  let to_cap_desc t (cap : Core_types.cap) =
    let cap = cap#shortest in
    match unwrap t cap with
    | None -> `Local cap
    | Some x -> (x :> [`Local of Core_types.cap | message_target_cap])

  (* We've just converted [caps] to [con_caps] and transmitted them.
     [dec_ref] each [cap], unless we need to keep it around so the peer can refer
     to it later. *)
  let release_remote_caps caps con_caps =
    con_caps |> RO_array.iteri (fun i -> function
        | `ReceiverHosted _
        | `ReceiverAnswer _ -> (RO_array.get caps i)#dec_ref
        | `Local _ -> ()
      )

  let pp_promise f = function
    | Some (q, _) -> pp_question f q
    | None -> Fmt.string f "(not initialised)"

  let maybe_release_question t question =
    if question.question_flags - flag_returned - flag_finished = 0 then (
      Questions.release t.questions question.question_id
    )

  module Send : sig
    (** Converts struct pointers into integer table indexes, ready for sending.
        The indexes are only valid until the next message is sent. *)

    open EP.Core_types

    val bootstrap : t -> struct_resolver -> question * Out.QuestionId.t
    val call : t -> struct_resolver -> message_target_cap -> [< descr] RO_array.t ->
      question * Out.QuestionId.t * Out.message_target * Out.desc RO_array.t

    val return_results : t -> answer -> Wire.Response.t -> [< descr] RO_array.t -> Out.AnswerId.t * Out.return
    val return_error : t -> answer -> string -> Out.AnswerId.t * Out.return
    val return_cancelled : t -> answer -> Out.AnswerId.t * Out.return

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
        the request. *)
    let export t : [< descr] -> Out.desc * ExportId.t list = function
      | `ReceiverHosted import ->
        (* Any ref-counting needed here? *)
        `ReceiverHosted import.import_id, []
      | `ReceiverAnswer (question, i) ->
        `ReceiverAnswer (question.question_id, i), []
      | `ThirdPartyHosted _ |`None |`SenderHosted _|`SenderPromise _ -> failwith "TODO: export"
      | `Local service ->
        match Hashtbl.find t.wrapper service with
        | id ->
          let export = Exports.find_exn t.exports id in
          export.export_count <- export.export_count + 1;
          `SenderHosted id, [id]
        | exception Not_found ->
          let export = Exports.alloc t.exports (fun export_id ->
              { export_count = 1; export_service = service; export_id }
            )
          in
          let id = export.export_id in
          Hashtbl.add t.wrapper service id;
          `SenderHosted id, [id]

    let bootstrap t question_data =
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
        | `ReceiverHosted import -> `ReceiverHosted import.import_id
        | `ReceiverAnswer (question, i) ->
          question.question_pipelined_fields <- PathSet.add i question.question_pipelined_fields;
          `ReceiverAnswer (question.question_id, i)
      in
      question, question.question_id, target, descrs

    let return_results t answer msg (caps : [< descr] RO_array.t) =
      let result =
        if answer.finished then `Cancelled
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

    let return_error _t answer msg =
      answer.answer_id, `Exception msg

    let return_cancelled _t answer =
      answer.answer_id, `Cancelled

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
      | `ReceiverHosted import -> `ReceiverHosted import.import_id
      | `ReceiverAnswer (question, path) -> `ReceiverAnswer (question.question_id, path)
  end

  type target = (question * unit Lazy.t) option  (* question, finish *)

  let rec send_call t target msg caps =
    let result = make_remote_promise t in
    let con_caps = RO_array.map (to_cap_desc t) caps in
    let question, qid, message_target, descs = Send.call t (result :> Core_types.struct_resolver) target con_caps in
    Log.info (fun f -> f ~tags:(tags ~qid t) "Sending: (%a).call %a"
                 pp_cap target
                 Core_types.Request_payload.pp (msg, caps));
    result#set_question question;
    t.queue_send (`Call (qid, message_target, msg, descs));
    release_remote_caps caps con_caps;
    (result :> Core_types.struct_ref)

  (* A cap that sends to a promised answer's cap at other *)
  and make_remote_promise t =
    object (self : #Core_types.struct_resolver)
      inherit [target] Struct_proxy.t None as super

      method do_pipeline question i msg caps =
        match question with
        | Some (target_q, _) ->
          let target = `ReceiverAnswer (target_q, i) in
          send_call t target msg caps
        | None -> failwith "Not initialised!"

      method on_resolve q _ =
        match q with
        | Some (_target_q, finish) -> Lazy.force finish
        | None -> failwith "Not initialised!"

      method! pp f =
        Fmt.pf f "remote-promise -> %a" (Struct_proxy.pp_state ~pp_promise) state

      method set_question q =
        let finish = lazy (
          let qid = Send.finish t q in
          Log.info (fun f -> f ~tags:(tags ~qid t) "Send finish %t" self#pp);
          t.queue_send (`Finish (qid, false));
        ) in
        self#update_target (Some (q, finish))

      method! cap path =
        let field = super#cap path in
        begin match state with
          | Unresolved u ->
            begin match u.target with
              | None -> failwith "Not intialised!"
              | Some (target_q, _) ->
                register t field (`ReceiverAnswer (target_q, path));        (* TODO: unregister *)
            end
          | _ -> ()
        end;
        field

      method do_finish = function
        | Some (_, finish) -> Lazy.force finish
        | None -> failwith "Not initialised!"
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
        release_remote_caps caps con_caps;
        aid, ret
      | Some (Error (`Exception msg)) ->
        let aid, ret = Send.return_error t answer msg in
        Log.info (fun f -> f ~tags:(tags ~aid t) "Returning error: %s" msg);
        aid, ret
      | Some (Error `Cancelled) ->
        let aid, ret = Send.return_cancelled t answer in
        Log.info (fun f -> f ~tags:(tags ~aid t) "Returning cancelled");
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
          promise#resolve (Error (`Exception "No bootstrap service available"));
      end;
      return_results t answer
    | `Call (answer, target, msg, caps) ->
      Log.info (fun f -> f ~tags:t.tags "Handling call: (%t).call %a" target#pp Core_types.Request_payload.pp (msg, caps));
      let resp = target#call msg caps in  (* Takes ownership of [caps]. *)
      target#dec_ref;
      (answer_promise answer)#connect resp;
      resp#when_resolved (fun _ -> return_results t answer)

  module Input : sig
    (* Converts received wire-messages containing integer table indexes that
       are only valid at the point when when message is received into messages
       containing pointers to structs. *)

    open EP.Core_types

    val call : t ->
      In.QuestionId.t -> In.message_target -> allowThirdPartyTailCall:bool -> In.desc RO_array.t ->
      In.sendResultsTo -> answer:struct_resolver ->
      answer * Core_types.cap * Core_types.cap RO_array.t
    val bootstrap : t -> In.QuestionId.t -> answer:struct_resolver -> answer
    val return_results : t -> In.AnswerId.t -> releaseParamCaps:bool -> Wire.Response.t -> In.desc RO_array.t -> struct_resolver * Core_types.cap RO_array.t
    val return_cancelled : t -> In.AnswerId.t -> releaseParamCaps:bool -> struct_resolver
    val return_exception : t -> In.AnswerId.t -> releaseParamCaps:bool -> struct_resolver

    val finish : t -> In.QuestionId.t -> releaseResultCaps:bool -> struct_resolver
    (* Returns the answer promise so that its caps can be released. *)

    val release : t -> In.ImportId.t -> referenceCount:int -> unit
    val disembargo_request : t -> In.disembargo_request ->
      [ `ReturnToSender of (struct_resolver * Wire.Path.t) * Message_types.EmbargoId.t]
    val disembargo_reply : t -> In.message_target -> Message_types.EmbargoId.t -> Cap_proxy.embargo_cap
(*
    val resolve : t -> In.ExportId.t -> [`Cap of In.desc | `Exception] -> unit
    val provide : t -> In.QuestionId.t -> In.message_target -> recipient_id -> unit
    val accept : t -> In.QuestionId.t -> provision_id -> embargo:bool -> unit
    val join : t -> In.QuestionId.t -> In.message_target -> join_key_part -> unit
*)
  end = struct
    let import t = function
      | `SenderHosted id ->
        (* Spec says this is "newly exported", so how does the remote indicate an existing, settled export? *)
        begin match Imports.find t.imports id with
          | Some import ->
            import.import_count <- import.import_count + 1;
            `ReceiverHosted import
          | None ->
            let import = { import_count = 1; import_id = id; import_proxy = None } in
            Imports.set t.imports id import;
            `ReceiverHosted import
        end
      | `ReceiverHosted id ->
        let export = Exports.find_exn t.exports id in
        `Export export
      | `ReceiverAnswer (id, path) ->
        let answer = Answers.find_exn t.answers id in
        `Answer (answer, path)
      | _ -> failwith "TODO: import"

    let import_simple t desc : [> recv_descr] =
      match import t desc with
      | `Export e -> `Local e.export_service
      | `Answer (answer, path) -> `Local (answer.answer_promise#cap path)
      | #recv_descr as x -> x

    let import_proxy import ~create ~inc_ref =
      match import.import_proxy with
      | Some p -> inc_ref p; p
      | None ->
        let p = create () in
        import.import_proxy <- Some p;
        p

    (* Turn a connection-scoped cap reference received from Other into a general-purpose
       cap for users. If the resulting cap is remote, our wrapper forwards it to Other.
       This will add a ref count to the cap if it already exists, or create a new
       one with [ref_count = 1]. *)
    let from_cap_desc t (desc:[< recv_cap_with_embargo]) : Core_types.cap =
      match desc with
      | `Local c -> c#inc_ref; c
      | `ReceiverHosted import as message_target ->
        import_proxy import
          ~inc_ref:(fun c -> c#inc_ref)
          ~create:(fun () ->
              let cap =
                object (self : #Core_types.cap)
                  inherit Core_types.ref_counted

                  method call msg caps = send_call t message_target msg caps
                  method pp f = Fmt.pf f "far-ref(rc=%d) -> %a" ref_count pp_cap message_target
                  method private release =
                    Log.info (fun f -> f ~tags:t.tags "Sending release %t" self#pp);
                    let id, count = Send.release t import in
                    t.queue_send (`Release (id, count))

                  method shortest = self
                  method blocker = None   (* Can't detect cycles over the network *)
                end
              in
              register t cap message_target;
              cap
            )
      | `None -> Core_types.null
      | `ReceiverAnswer _ -> failwith "TODO: from_cap_desc ReceiverAnswer"
      | `ThirdPartyHosted _ -> failwith "TODO: from_cap_desc ThirdPartyHosted"
      | `LocalPromise (p, i) -> p#cap i

    let call t id (message_target : In.message_target) ~allowThirdPartyTailCall descrs sendResultsTo ~answer =
      ignore allowThirdPartyTailCall; (* TODO *)
      ignore sendResultsTo; (* TODO *)
      let answer = {
        answer_id = id;
        exports_for_release = [];
        answer_promise = answer;
        finished = false;
      } in
      Answers.set t.answers id answer;
      let target : recv_descr =
        match message_target with
        | `ReceiverHosted id ->
          let export = Exports.find_exn t.exports id in
          `Local export.export_service
        | `ReceiverAnswer (id, path) ->
          let answer = Answers.find_exn t.answers id in
          `LocalPromise (answer.answer_promise, path)
      in
      let caps = RO_array.map (import_simple t) descrs in
      let target = from_cap_desc t target in
      let caps = RO_array.map (from_cap_desc t) caps in
      answer, target, caps

    let bootstrap t id ~answer =
      let answer = {
        answer_id = id;
        exports_for_release = [];
        answer_promise = answer;
        finished = false;
      } in
      Answers.set t.answers id answer;
      answer

    let return_generic t id ~releaseParamCaps =
      ignore releaseParamCaps; (* TODO *)
      let question = Questions.find_exn t.questions id in
      let flags = question.question_flags in
      assert (flags land flag_returned = 0);
      let flags = flags + flag_returned in
      question.question_flags <- flags;
      maybe_release_question t question;
      question

    let return_cancelled t id ~releaseParamCaps =
      let question = return_generic t id ~releaseParamCaps in
      question.question_data

    let return_exception t id ~releaseParamCaps =
      let question = return_generic t id ~releaseParamCaps in
      question.question_data

    let service = function
      | `Export e -> e.export_service
      | `Answer (answer, path) -> answer.answer_promise#cap path

    let return_results t id ~releaseParamCaps msg descrs =
      let question = return_generic t id ~releaseParamCaps in
      let caps_used = (* Maps used cap indexes to their paths *)
        PathSet.elements question.question_pipelined_fields
        |> filter_map (fun path ->
            match Wire.Response.cap_index msg path with
            | None -> None
            | Some i -> Some (i, path)
          )
        |> IntMap.of_list
      in
      let import_with_embargo i d =
        match import t d with
        | #recv_descr as x -> from_cap_desc t x
        | `Export _ | `Answer _ as x ->
          match IntMap.find i caps_used with
          | None -> from_cap_desc t (`Local (service x))        (* Not used, so no embargo needed *)
          | Some path ->
            let c = service x in
            let embargo = Cap_proxy.embargo c in
            let (embargo_id, _) = Embargoes.alloc t.embargoes (fun id -> (id, embargo)) in
            let disembargo_request = `Loopback ((id, path), embargo_id) in
            c#inc_ref;
            Log.info (fun f -> f ~tags:t.tags "Embargo %t until %a is delivered"
                         c#pp
                         EP.Out.pp_disembargo_request disembargo_request
                     );
            (* We previously pipelined messages to [qid, index], which now turns out to be
               local service [c]. We need to send a disembargo to clear the pipeline before
               using [c]. *)
            disembargo t disembargo_request;
            (embargo :> Core_types.cap)
      in
      let caps = RO_array.mapi import_with_embargo descrs in
      question.question_data, caps

    let release t export_id ~referenceCount =
      assert (referenceCount > 0);
      let export = Exports.find_exn t.exports export_id in
      assert (export.export_count >= referenceCount);
      let count = export.export_count - referenceCount in
      export.export_count <- count;
      if count = 0 then (
        Log.info (fun f -> f ~tags:t.tags "Releasing export %a" ExportId.pp export_id);
        Hashtbl.remove t.wrapper export.export_service;
        Exports.release t.exports export_id
      )

    let finish t answer_id ~releaseResultCaps =
      let answer = Answers.find_exn t.answers answer_id in
      answer.finished <- true;
      Answers.release t.answers answer_id;
      if releaseResultCaps then (
        (* This is very unclear. It says "all capabilities that were in the
           results should be considered released". However, only imports can
           be released, not capabilities in general.
           Also, assuming we decrement the ref count by one in this case. *)
        List.iter (release t ~referenceCount:1) answer.exports_for_release
      );
      answer.answer_promise

    let disembargo_request t : In.disembargo_request -> _ = function
      | `Loopback ((aid, i), id) ->
        let answer = Answers.find_exn t.answers aid in
        (* Check that [answer:i] points back at sender. *)
        (* TODO: move the struct_ref logic here so we can do the check here? *)
        `ReturnToSender ((answer.answer_promise, i), id)

    let disembargo_reply t _target embargo_id =
      let embargo = snd (Embargoes.find_exn t.embargoes embargo_id) in
      Embargoes.release t.embargoes embargo_id;
      embargo

(* TODO:
    let resolve _t _export_id = function
      | `Cap _desc -> ()
      | `Exception -> ()
    let provide _t _question_id _message_target _recipient_id = ()
    let accept _t _question_id _provision_id ~embargo:_ = ()
    let join _t _question_id _message_target _join_key_part = ()
*)

  end

  let handle_bootstrap t qid =
    let promise = Local_struct_promise.make () in
    let answer = Input.bootstrap t qid ~answer:promise in
    reply_to_call t (`Bootstrap answer)

  let handle_call t (aid, message_target, msg, descs) =
    Log.info (fun f -> f ~tags:(tags ~aid t) "Received call to %a" EP.In.pp_desc message_target);
    let promise = Local_struct_promise.make () in
    let answer, target, caps = Input.call t aid message_target descs ~allowThirdPartyTailCall:false `Caller ~answer:promise in
    reply_to_call t (`Call (answer, target, msg, caps))

  let handle_return t (qid, ret) =
    match ret with
    | `Results (msg, descs) ->
      let result, caps = Input.return_results t qid msg descs ~releaseParamCaps:false in
      Log.info (fun f -> f ~tags:(tags ~qid t) "Got results: %a"
                   Core_types.Response_payload.pp (msg, caps)
               );
      result#resolve (Ok (msg, caps))
    | `Exception msg ->
      let result = Input.return_exception t qid ~releaseParamCaps:false in
      Log.info (fun f -> f ~tags:(tags ~qid t) "Got exception: %s" msg);
      result#resolve (Error (`Exception msg))
    | `Cancelled ->
      let result = Input.return_cancelled t qid ~releaseParamCaps:false in
      Log.info (fun f -> f ~tags:(tags ~qid t) "Got cancelled");
      result#resolve (Error `Cancelled)
    | _ -> failwith "TODO: other return"

  let handle_finish t (aid, releaseResultCaps) =
    let answer = Input.finish t aid ~releaseResultCaps in
    Log.info (fun f -> f ~tags:(tags ~aid t) "Received finish for %t" answer#pp);
    answer#finish

  let handle_disembargo_request t request =
    Log.info (fun f -> f ~tags:t.tags "Received disembargo %a" EP.In.pp_disembargo_request request);
    match Input.disembargo_request t request with
    | `ReturnToSender ((answer_promise, path), id) ->
      match answer_promise#response with
      | None -> failwith "Got disembargo for unresolved promise!"
      | Some (Error _) -> failwith "Got disembargo for exception!"
      | Some (Ok payload) ->
        let cap = Core_types.Response_payload.field payload path in
        match unwrap t cap with
        | Some (`ReceiverHosted _ | `ReceiverAnswer _ as target) -> reply_to_disembargo t target id
        | None -> failwith "Protocol error: disembargo for invalid target"

  let handle_disembargo_reply t (target, embargo_id) =
    let embargo = Input.disembargo_reply t target embargo_id in
    Log.info (fun f -> f ~tags:t.tags "Received disembargo response %a -> %t"
                 EP.In.pp_desc target
                 embargo#pp);
    embargo#disembargo

  let handle_release t (id, referenceCount) =
    Input.release t id ~referenceCount

  let handle_msg t = function
    | `Bootstrap x          -> handle_bootstrap t x
    | `Call x               -> handle_call t x
    | `Return x             -> handle_return t x
    | `Finish x             -> handle_finish t x
    | `Release x            -> handle_release t x
    | `Disembargo_request x -> handle_disembargo_request t x
    | `Disembargo_reply x   -> handle_disembargo_reply t x

  let dump_embargo f (id, proxy) =
    Fmt.pf f "%a: @[%t@]" EmbargoId.pp id proxy#pp

  let dump f t =
    Fmt.pf f "@[<2>Questions:@,%a@]@,\
              @[<2>Answers:@,%a@]@,\
              @[<2>Exports:@,%a@]@,\
              @[<2>Imports:@,%a@]@,\
              @[<2>Embargoes:@,%a@]"
      (Questions.dump dump_question) t.questions
      (Answers.dump dump_answer) t.answers
      (Exports.dump dump_export) t.exports
      (Imports.dump dump_import) t.imports
      (Embargoes.dump dump_embargo) t.embargoes
end
