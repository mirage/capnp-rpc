module Log = Debug.Log

open Asetmap

module EmbargoId = Message_types.EmbargoId

module IntMap = Map.Make(struct type t = int let compare (a:int) b = compare a b end)

let rec filter_map f = function
  | [] -> []
  | x :: xs ->
    match f x with
    | None -> filter_map f xs
    | Some y -> y :: filter_map f xs

module Make (EP: Message_types.ENDPOINT) = struct
  module C = EP.Core_types
  module Wire = EP.Core_types.Wire
  module In = EP.In
  module Out = EP.Out

  open EP.Table

  module type S = sig
    type t

    type question
    type answer
    type export
    type import

    type message_target_cap = [
      | `ReceiverHosted of import
      | `ReceiverAnswer of question * Wire.Path.t
    ]

    type cap = [
      message_target_cap
      | `None
      | `SenderHosted of export
      | `SenderPromise of export
      | `ThirdPartyHosted of Out.third_party_desc (* TODO *)
      | `Local of C.cap
    ]

    type recv_cap = [
      message_target_cap
      | `None
      | `ThirdPartyHosted of Out.third_party_desc
      | `Local of C.cap
      | `LocalPromise of C.struct_resolver * Wire.Path.t
    ]

    type recv_cap_with_embargo = [
      recv_cap
      | `LocalEmbargo of C.cap * Out.disembargo_request   (* Send a Disembargo, and queue messages until it returns *)
    ]

    val pp_cap : [< cap | recv_cap_with_embargo] Fmt.t
        
    val create : tags:Logs.Tag.set -> unit -> t

    val answer_promise : answer -> C.struct_resolver

    val import_proxy : import ->
      create:(unit -> C.cap) ->
      inc_ref:(C.cap -> unit) ->
      C.cap
    (* Get (and inc_rec) the proxy associated with [import], or create (and store) a new one. *)

    module Input : sig
      val call : t ->
        In.QuestionId.t -> In.message_target -> allowThirdPartyTailCall:bool -> In.desc RO_array.t ->
        In.sendResultsTo -> answer:C.struct_resolver ->
        answer * recv_cap * recv_cap RO_array.t
      val bootstrap : t -> In.QuestionId.t -> answer:C.struct_resolver -> answer
      val return_results : t -> In.AnswerId.t -> releaseParamCaps:bool -> Wire.Response.t -> In.desc RO_array.t -> C.struct_resolver * recv_cap_with_embargo RO_array.t
      val return_cancelled : t -> In.AnswerId.t -> releaseParamCaps:bool -> C.struct_resolver
      val return_exception : t -> In.AnswerId.t -> releaseParamCaps:bool -> C.struct_resolver

      val finish : t -> In.QuestionId.t -> releaseResultCaps:bool -> C.struct_resolver
      (* Returns the answer promise so that its caps can be released. *)

      val resolve : t -> In.ExportId.t -> [`Cap of In.desc | `Exception] -> unit
      val release : t -> In.ImportId.t -> referenceCount:int -> unit
      val disembargo_request : t -> In.disembargo_request ->
        [ `ReturnToSender of (C.struct_resolver * Wire.Path.t) * EmbargoId.t]
      val disembargo_reply : t -> In.message_target -> EmbargoId.t -> unit
      val provide : t -> In.QuestionId.t -> In.message_target -> C.recipient_id -> unit
      val accept : t -> In.QuestionId.t -> C.provision_id -> embargo:bool -> unit
      val join : t -> In.QuestionId.t -> In.message_target -> C.join_key_part -> unit
    end

    module Send : sig
      val bootstrap : t -> C.struct_resolver -> question * Out.QuestionId.t
      val call : t -> C.struct_resolver -> message_target_cap -> [< cap] RO_array.t ->
        question * Out.QuestionId.t * Out.message_target * Out.desc RO_array.t

      val return_results : t -> answer -> Wire.Response.t -> [< cap] RO_array.t -> Out.AnswerId.t * Out.return
      val return_error : t -> answer -> string -> Out.AnswerId.t * Out.return
      val return_cancelled : t -> answer -> Out.AnswerId.t * Out.return

      val release : t -> import -> Out.ImportId.t * int
      (** [release t i] indicates that [i] is no longer used by the client.
          Returns the [referenceCount] for the Release message. *)

      val finish : t -> question -> Out.QuestionId.t
      (** [finish t qid] tells the system that we're about to send a Finish message
          (with [releaseResultCaps=false]). *)

      val disembargo_reply : t -> message_target_cap -> Out.message_target
    end

    val dump : t Fmt.t

    val stats : t -> Stats.t
    val pp_question : question Fmt.t
  end

  module PathSet = Set.Make(Wire.Path)
  module Embargoes = Table.Allocating(EmbargoId)

  module Questions = Table.Allocating(QuestionId)
  module Answers = Table.Tracking(AnswerId)
  module Exports = Table.Allocating(ExportId)
  module Imports = Table.Tracking(ImportId)

  type question = {
    question_id : QuestionId.t;
    question_data : C.struct_resolver;
    mutable question_flags : int;
    mutable params_for_release : ExportId.t list;
    mutable question_pipelined_fields : PathSet.t; (* Fields used while unresolved; will need embargoes *)
  }

  type answer = {
    answer_id : AnswerId.t;
    mutable exports_for_release : ExportId.t list;
    answer_promise : C.struct_resolver;
    mutable finished : bool;
  }

  let flag_returned = 1
  let flag_finished = 2

  type export = {
    export_id : ExportId.t;
    mutable export_count : int; (* Number of times sent to remote and not yet released *)
    export_service : C.cap;
  }

  type import = {
    import_id : ImportId.t;
    mutable import_count : int; (* Number of times remote sent us this *)
    mutable import_proxy : C.cap option;
  }

  type message_target_cap = [
    | `ReceiverHosted of import
    | `ReceiverAnswer of question * Wire.Path.t
  ]

  type cap = [
    message_target_cap
    | `None
    | `SenderHosted of export
    | `SenderPromise of export
    | `ThirdPartyHosted of Out.third_party_desc
    | `Local of C.cap
  ]

  type recv_cap = [
    message_target_cap
    | `None
    | `ThirdPartyHosted of Out.third_party_desc
    | `Local of C.cap
    | `LocalPromise of C.struct_resolver * Wire.Path.t
  ]

  type recv_cap_with_embargo = [
    recv_cap
    | `LocalEmbargo of C.cap * Out.disembargo_request   (* Send a Disembargo, and queue messages until it returns *)
  ]

  let pp_cap : [< cap | recv_cap_with_embargo] Fmt.t = fun f -> function
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
    tags : Logs.Tag.set;
    questions : question Questions.t;
    answers : answer Answers.t;
    exports : export Exports.t;
    imports : import Imports.t;
    embargoes : EmbargoId.t Embargoes.t;
    wrapper : (C.cap, ExportId.t) Hashtbl.t;
  }

  let create ~tags () =
    {
      tags;
      questions = Questions.make ();
      answers = Answers.make ();
      imports = Imports.make ();
      exports = Exports.make ();
      embargoes = Embargoes.make ();
      wrapper = Hashtbl.create 30;
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

  let dump f t =
    Fmt.pf f "@[<2>Questions:@,%a@]@,\
              @[<2>Answers:@,%a@]@,\
              @[<2>Exports:@,%a@]@,\
              @[<2>Imports:@,%a@]"
      (Questions.dump dump_question) t.questions
      (Answers.dump dump_answer) t.answers
      (Exports.dump dump_export) t.exports
      (Imports.dump dump_import) t.imports

  let maybe_release_question t question =
    if question.question_flags - flag_returned - flag_finished = 0 then (
      Questions.release t.questions question.question_id
    )

  (** [export t target] is a descriptor for [target], plus a list of exports that
      should be freed if we get a request to free all capabilities associated with
      the request. *)
  let export t : [< cap] -> Out.desc * ExportId.t list = function
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

  let import_simple t desc : [> recv_cap] =
    match import t desc with
    | `Export e -> `Local e.export_service
    | `Answer (answer, path) -> `Local (answer.answer_promise#cap path)
    | #recv_cap as x -> x

  let answer_promise answer = answer.answer_promise

  let import_proxy import ~create ~inc_ref =
    match import.import_proxy with
    | Some p -> inc_ref p; p
    | None ->
      let p = create () in
      import.import_proxy <- Some p;
      p

  module Input = struct
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
      let target : recv_cap =
        match message_target with
        | `ReceiverHosted id ->
          let export = Exports.find_exn t.exports id in
          `Local export.export_service
        | `ReceiverAnswer (id, path) ->
          let answer = Answers.find_exn t.answers id in
          `LocalPromise (answer.answer_promise, path)
      in
      let caps = RO_array.map (import_simple t) descrs in
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

    let return_results t id ~releaseParamCaps msg descrs : C.struct_resolver * recv_cap_with_embargo RO_array.t =
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
        | #recv_cap as x -> x
        | `Export _ | `Answer _ as x ->
          match IntMap.find i caps_used with
          | None -> `Local (service x)        (* Not used, so no embargo needed *)
          | Some path ->
            let cap = service x in
            let embargo_id = Embargoes.alloc t.embargoes (fun id -> id) in
            `LocalEmbargo (cap, `Loopback ((id, path), embargo_id))
      in
      let caps = RO_array.mapi import_with_embargo descrs in
      question.question_data, caps

    let resolve _t _export_id = function
      | `Cap _desc -> ()
      | `Exception -> ()

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
      Embargoes.release t.embargoes embargo_id

    let provide _t _question_id _message_target _recipient_id = ()
    let accept _t _question_id _provision_id ~embargo:_ = ()
    let join _t _question_id _message_target _join_key_part = ()
  end

  module Send = struct
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

    let return_results t answer msg (caps : [< cap] RO_array.t) =
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

  let stats t =
    { Stats.
      n_questions = Questions.active t.questions;
      n_answers = Answers.active t.answers;
      n_imports = Imports.active t.imports;
      n_exports = Exports.active t.exports;
    }
end
