module Log : Logs.LOG

val qid_tag : Uint32.t Logs.Tag.def
(** [qid_tag] is used in log reports to tag the question (or answer) ID in the call. *)
