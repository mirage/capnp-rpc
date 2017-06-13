[@@@ocaml.warning "-27"]        (* TODO *)

module Log = Debug.Log

open Asetmap

module IntMap = Map.Make(struct type t = int let compare (a:int) b = compare a b end)

module EmbargoId = Id.Make ( )

module Make (C : S.CORE_TYPES) (N : S.NETWORK_TYPES) = struct

  module PathSet = Set.Make(C.Path)
  module EmbargoId = EmbargoId

  type ('third, 'export) generic_third_party_desc = {
    id : 'third;
    vine_id : 'export;
  }

  module MessageTypes (T : S.TABLE_TYPES) = struct
    module  N = N
    include T

    type third_party_desc = (N.third_party_cap_id, T.ExportId.t) generic_third_party_desc

    type message_target = [
      | `ReceiverHosted of T.ImportId.t
      | `ReceiverAnswer of T.QuestionId.t * C.Path.t
    ]

    type desc = [
      message_target
      | `None
      | `SenderHosted of T.ExportId.t
      | `SenderPromise of T.ExportId.t
      | `ThirdPartyHosted of third_party_desc
    ]

    let pp_desc f = function
      | `None                  -> Fmt.pf f "None"
      | `Local local           -> Fmt.pf f "Local:%t" local#pp
      | `ReceiverAnswer (x, p) -> Fmt.pf f "ReceiverAnswer:%a:%a" T.QuestionId.pp x C.Path.pp p
      | `ReceiverHosted x      -> Fmt.pf f "ReceiverHosted:%a" T.ImportId.pp x
      | `ThirdPartyHosted _    -> Fmt.pf f "ThirdPartyHosted"
      | `SenderHosted x        -> Fmt.pf f "SenderHosted:%a" T.ExportId.pp x
      | `SenderPromise x       -> Fmt.pf f "SenderPromise:%a" T.ExportId.pp x

    type sendResultsTo = [
      | `Caller
      | `Yourself
      | `ThirdParty of N.recipient_id
    ]

    type return = [
      | `Results of C.Response.t * desc RO_array.t
      | `Exception of string
      | `Cancelled
      | `ResultsSentElsewhere
      | `TakeFromOtherQuestion
      | `AcceptFromThirdParty
    ]

    type disembargo_request = [
      | `Loopback of (QuestionId.t * C.Path.t) * EmbargoId.t
    ]

    let pp_return f = function
      | `Results descrs -> Fmt.pf f "Results:%a" (Fmt.Dump.list pp_desc) descrs
      | `Exception msg -> Fmt.pf f "Exception:%s" msg
      | `Cancelled -> Fmt.pf f "Cancelled"
      | `ResultsSentElsewhere -> Fmt.pf f "ResultsSentElsewhere"
      | `TakeFromOtherQuestion -> Fmt.pf f "TakeFromOtherQuestion"
      | `AcceptFromThirdParty -> Fmt.pf f "AcceptFromThirdParty"

    let pp_disembargo_request : disembargo_request Fmt.t = fun f -> function
      | `Loopback ((qid, p), id) -> Fmt.pf f "senderLoopback for question %a:%a (embargo_id = %a)"
                                    QuestionId.pp qid C.Path.pp p
                                    EmbargoId.pp id;

    type t = [
      | `Bootstrap of T.QuestionId.t
      | `Call of T.QuestionId.t * message_target * C.Request.t * desc RO_array.t
      | `Finish of (T.QuestionId.t * bool)      (* bool is release-caps *)
      | `Release of T.ImportId.t * int
      | `Disembargo_request of disembargo_request
      | `Disembargo_reply of [`ReceiverHosted of T.ImportId.t] * EmbargoId.t
      | `Return of (T.AnswerId.t * return)
    ]
  end

  module type S = sig
    type t

    module T : S.TABLE_TYPES

    module Out : (module type of MessageTypes(T))
    (** Types for messages we send. *)

    module In : module type of MessageTypes(struct
        module QuestionId = T.AnswerId
        module AnswerId = T.QuestionId
        module ImportId = T.ExportId
        module ExportId = T.ImportId
      end)
    (** Types for messages we receive. *)

    type question
    type answer
    type export
    type import

    type message_target_cap = [
      | `ReceiverHosted of import
      | `ReceiverAnswer of question * C.Path.t
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
      | `LocalPromise of C.struct_resolver * C.Path.t
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
      val return_results : t -> In.AnswerId.t -> releaseParamCaps:bool -> C.Response.t -> In.desc RO_array.t -> C.struct_resolver * recv_cap_with_embargo RO_array.t
      val return_cancelled : t -> In.AnswerId.t -> releaseParamCaps:bool -> C.struct_resolver
      val return_exception : t -> In.AnswerId.t -> releaseParamCaps:bool -> C.struct_resolver

      val finish : t -> In.QuestionId.t -> releaseResultCaps:bool -> C.struct_resolver
      (* Returns the answer promise so that its caps can be released. *)

      val resolve : t -> In.ExportId.t -> [`Cap of In.desc | `Exception] -> unit
      val release : t -> In.ImportId.t -> referenceCount:int -> unit
      val disembargo_request : t -> In.disembargo_request ->
        [ `ReturnToSender of (C.struct_resolver * C.Path.t) * EmbargoId.t]
      val disembargo_reply : t -> [`ReceiverHosted of In.ImportId.t] -> C.cap
      val provide : t -> In.QuestionId.t -> In.message_target -> N.recipient_id -> unit
      val accept : t -> In.QuestionId.t -> N.provision_id -> embargo:bool -> unit
      val join : t -> In.QuestionId.t -> In.message_target -> N.join_key_part -> unit
    end

    module Send : sig
      val bootstrap : t -> C.struct_resolver -> question * Out.QuestionId.t
      val call : t -> C.struct_resolver -> message_target_cap -> [< cap] RO_array.t ->
        question * Out.QuestionId.t * Out.message_target * Out.desc RO_array.t

      val return_results : t -> answer -> C.Response.t -> [< cap] RO_array.t -> Out.AnswerId.t * Out.return
      val return_error : t -> answer -> string -> Out.AnswerId.t * Out.return
      val return_cancelled : t -> answer -> Out.AnswerId.t * Out.return

      val release : t -> import -> Out.ImportId.t * int
      (** [release t i] indicates that [i] is no longer used by the client.
          Returns the [referenceCount] for the Release message. *)

      val finish : t -> question -> Out.QuestionId.t
      (** [finish t qid] tells the system that we're about to send a Finish message
          (with [releaseResultCaps=false]). *)

      val disembargo_reply : t -> [`ReceiverHosted of import] -> [`ReceiverHosted of Out.ImportId.t]
    end

    val stats : t -> Stats.t
    val pp_question : question Fmt.t
  end

  module Make (T : S.TABLE_TYPES) : S with module T = T = struct
    module T = T

    module Questions = Table.Allocating(T.QuestionId)
    module Answers = Table.Tracking(T.AnswerId)
    module Exports = Table.Allocating(T.ExportId)
    module Imports = Table.Tracking(T.ImportId)

    module Out = MessageTypes(T)

    module In = MessageTypes(struct
        module QuestionId = T.AnswerId
        module AnswerId = T.QuestionId
        module ImportId = T.ExportId
        module ExportId = T.ImportId
      end)

    type question = {
      question_id : T.QuestionId.t;
      question_data : C.struct_resolver;
      mutable question_flags : int;
      mutable params_for_release : T.ExportId.t list;
      mutable question_pipelined_fields : PathSet.t; (* Fields used while unresolved; will need embargoes *)
    }

    type answer = {
      answer_id : T.AnswerId.t;
      mutable exports_for_release : T.ExportId.t list;
      answer_promise : C.struct_resolver;
      mutable finished : bool;
    }

    let flag_returned = 1
    let flag_finished = 2

    type export = {
      export_id : T.ExportId.t;
      mutable export_count : int; (* Number of times sent to remote and not yet released *)
      export_service : C.cap;
      mutable export_next_embargo : EmbargoId.t; (* Next unused ID *)
    }

    type import = {
      import_id : T.ImportId.t;
      mutable import_count : int; (* Number of times remote sent us this *)
      mutable import_proxy : C.cap option;
    }

    type message_target_cap = [
      | `ReceiverHosted of import
      | `ReceiverAnswer of question * C.Path.t
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
      | `LocalPromise of C.struct_resolver * C.Path.t
    ]

    type recv_cap_with_embargo = [
      recv_cap
      | `LocalEmbargo of C.cap * Out.disembargo_request   (* Send a Disembargo, and queue messages until it returns *)
    ]

    let pp_cap : [< cap | recv_cap_with_embargo] Fmt.t = fun f -> function
      | `None -> Fmt.pf f "null"
      | `ReceiverHosted import -> Fmt.pf f "ReceiverHosted:%a" T.ImportId.pp import.import_id
      | `ReceiverAnswer (question, p) -> Fmt.pf f "ReceiverAnswer:%a[%a]" T.QuestionId.pp question.question_id C.Path.pp p
      | `ThirdPartyHosted _third_party_desc -> Fmt.pf f "ThirdPartyHosted"
      | `SenderHosted export -> Fmt.pf f "SenderHosted:%a" T.ExportId.pp export.export_id
      | `SenderPromise export -> Fmt.pf f "SenderPromise:%a" T.ExportId.pp export.export_id
      | `Local local -> Fmt.pf f "Local:%t" local#pp
      | `LocalPromise (_promise, path) -> Fmt.pf f "LocalPromise:<p>%a" C.Path.pp path
      | `LocalEmbargo (local_service, req) -> Fmt.pf f "LocalEmbargo:%t:%a" local_service#pp Out.pp_disembargo_request req

    type t = {
      tags : Logs.Tag.set;
      questions : question Questions.t;
      answers : answer Answers.t;
      exports : export Exports.t;
      imports : import Imports.t;
      wrapper : (C.cap, T.ExportId.t) Hashtbl.t;
    }

    let create ~tags () =
      {
        tags;
        questions = Questions.make ();
        answers = Answers.make ();
        imports = Imports.make ();
        exports = Exports.make ();
        wrapper = Hashtbl.create 30;
      }

    let pp_question f q =
      Fmt.pf f "q%a" T.QuestionId.pp q.question_id

    let maybe_release_question t question =
      if question.question_flags - flag_returned - flag_finished = 0 then (
        Questions.release t.questions question.question_id
      )

    (** [export t target] is a descriptor for [target], plus a list of exports that
        should be freed if we get a request to free all capabilities associated with
        the request. *)
    let export t : [< cap] -> Out.desc * T.ExportId.t list = function
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
              { export_count = 1; export_service = service; export_id; export_next_embargo = EmbargoId.zero }
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
        (`Local (answer.answer_promise#cap path))
      | _ -> failwith "TODO: import"

    let import_simple t desc : [> recv_cap] =
      match import t desc with
      | `Export e -> `Local e.export_service
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

      let return_results t id ~releaseParamCaps msg descrs : C.struct_resolver * recv_cap_with_embargo RO_array.t =
        let question = return_generic t id ~releaseParamCaps in
        let caps_used = (* Maps used cap indexes to their paths *)
          PathSet.elements question.question_pipelined_fields
          |> List.map (fun path -> C.Response.cap_index msg path, path)
          |> IntMap.of_list
        in
        let import_with_embargo i d =
          match import t d with
          | #recv_cap as x -> x
          | `Export e ->
            match IntMap.find i caps_used with
            | Some path ->
              let embargo_id = e.export_next_embargo in
              e.export_next_embargo <- EmbargoId.succ e.export_next_embargo;
              `LocalEmbargo (e.export_service, `Loopback ((id, path), embargo_id))
            | None -> `Local e.export_service
        in
        let caps = RO_array.mapi import_with_embargo descrs in
        question.question_data, caps

      let resolve t export_id = function
        | `Cap desc -> ()
        | `Exception -> ()

      let release t export_id ~referenceCount =
        assert (referenceCount > 0);
        let export = Exports.find_exn t.exports export_id in
        assert (export.export_count >= referenceCount);
        let count = export.export_count - referenceCount in
        export.export_count <- count;
        if count = 0 then (
          Log.info (fun f -> f ~tags:t.tags "Releasing export %a" T.ExportId.pp export_id);
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

      let disembargo_reply t (`ReceiverHosted export_id) =
        let export = Exports.find_exn t.exports export_id in
        export.export_service

      let provide t question_id message_target recipient_id = ()
      let accept t question_id provision_id ~embargo = ()
      let join t question_id message_target join_key_part = ()
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

      let return_error t answer msg =
        answer.answer_id, `Exception msg

      let return_cancelled t answer =
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

      let disembargo_reply t (`ReceiverHosted import) =
        `ReceiverHosted import.import_id
    end

    let stats t =
      { Stats.
        n_questions = Questions.active t.questions;
        n_answers = Answers.active t.answers;
        n_imports = Imports.active t.imports;
        n_exports = Exports.active t.exports;
      }
  end
end
