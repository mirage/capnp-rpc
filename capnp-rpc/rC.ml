type t = int

let leaked  = -1
let zero    =  0
let one     =  1

let pp f = function
  | -1 -> Fmt.pf f "rc=%a" Fmt.(styled `Red string) "LEAKED"
  | 0  -> Fmt.pf f "rc=%a" Fmt.(styled `Red int) 0
  | t  -> Fmt.pf f "rc=%d" t

let sum ~pp:pp_obj t d =
  if t > 0 then (
    let t' = t + d in
    if t' < 0 then (
      if d > 0 then Debug.failf "Ref-count %a + %d would wrap!" pp t d pp_obj
      else Debug.failf "Ref-count %a - %d would go negative!" pp t (-d) pp_obj
    );
    t'
  ) else if d >= 0 then (
    Debug.failf "Attempt to change ref-count (to %a+%d) on freed resource %t" pp t d pp_obj
  ) else (
    Debug.failf "Attempt to change ref-count (to %a%d) on freed resource %t" pp t d pp_obj
  )

let succ ~pp t = sum ~pp t 1
let pred ~pp t = sum ~pp t (-1)

let is_zero = function
  | 0 -> true
  | _ -> false

let check ~pp t =
  if t < 1 then
    Debug.invariant_broken (fun f -> Fmt.pf f "Already unref'd! %t" pp)

let to_int t =
  if t >= 0 then Some t
  else None
