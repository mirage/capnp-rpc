module Make (EP: Message_types.ENDPOINT) : sig
  open EP
  open EP.Core_types

  type t

  type question
  type answer
  type export
  type import

  type message_target_cap = [
    | `ReceiverHosted of import
    | `ReceiverAnswer of question * Wire.Path.t
  ]

  type descr = [
    message_target_cap
    | `None
    | `SenderHosted of export
    | `SenderPromise of export
    | `ThirdPartyHosted of Out.third_party_desc (* TODO *)
    | `Local of cap
  ]

  type recv_descr = [
    message_target_cap
    | `None
    | `ThirdPartyHosted of Out.third_party_desc
    | `Local of cap
    | `LocalPromise of struct_resolver * Wire.Path.t
  ]

  type recv_cap_with_embargo = [
    recv_descr
    | `LocalEmbargo of cap * Out.disembargo_request   (* Send a Disembargo, and queue messages until it returns *)
  ]

  val pp_cap : [< descr | recv_cap_with_embargo] Fmt.t
      
  val create : tags:Logs.Tag.set -> unit -> t

  val answer_promise : answer -> struct_resolver

  val import_proxy : import ->
    create:(unit -> cap) ->
    inc_ref:(cap -> unit) ->
    cap
  (* Get (and inc_rec) the proxy associated with [import], or create (and store) a new one. *)

  module Input : sig
    val call : t ->
      In.QuestionId.t -> In.message_target -> allowThirdPartyTailCall:bool -> In.desc RO_array.t ->
      In.sendResultsTo -> answer:struct_resolver ->
      answer * recv_descr * recv_descr RO_array.t
    val bootstrap : t -> In.QuestionId.t -> answer:struct_resolver -> answer
    val return_results : t -> In.AnswerId.t -> releaseParamCaps:bool -> Wire.Response.t -> In.desc RO_array.t -> struct_resolver * recv_cap_with_embargo RO_array.t
    val return_cancelled : t -> In.AnswerId.t -> releaseParamCaps:bool -> struct_resolver
    val return_exception : t -> In.AnswerId.t -> releaseParamCaps:bool -> struct_resolver

    val finish : t -> In.QuestionId.t -> releaseResultCaps:bool -> struct_resolver
    (* Returns the answer promise so that its caps can be released. *)

    val resolve : t -> In.ExportId.t -> [`Cap of In.desc | `Exception] -> unit
    val release : t -> In.ImportId.t -> referenceCount:int -> unit
    val disembargo_request : t -> In.disembargo_request ->
      [ `ReturnToSender of (struct_resolver * Wire.Path.t) * Message_types.EmbargoId.t]
    val disembargo_reply : t -> In.message_target -> Message_types.EmbargoId.t -> unit
    val provide : t -> In.QuestionId.t -> In.message_target -> recipient_id -> unit
    val accept : t -> In.QuestionId.t -> provision_id -> embargo:bool -> unit
    val join : t -> In.QuestionId.t -> In.message_target -> join_key_part -> unit
  end

  module Send : sig
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
  end

  val dump : t Fmt.t

  val stats : t -> Stats.t
  val pp_question : question Fmt.t
end
