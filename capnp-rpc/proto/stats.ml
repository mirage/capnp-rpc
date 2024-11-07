(** Statistics for CapTP connections. *)

type t = {
  n_questions : int;
  n_answers : int;
  n_imports : int;
  n_exports : int;
}

let zero = {
  n_questions = 0;
  n_answers = 0;
  n_imports = 0;
  n_exports = 0;
}

let pp f t =
  Fmt.pf f "{%dq,%da,%di,%de}"
    t.n_questions
    t.n_answers
    t.n_imports
    t.n_exports
