type t = int

let leaked  = -1
let zero    =  0
let one     =  1

let pp f = function
  | -1 -> Fmt.pf f "rc=%a" Fmt.(styled `Red string) "LEAKED"
  | 0  -> Fmt.pf f "rc=%a" Fmt.(styled `Red int) 0
  | t  -> Fmt.pf f "rc=%d" t

let succ ~pp:pp_obj t =
  if t > 0 then (
    let t' = t + 1 in
    if t' < 0 then Debug.failf "Ref-count %a on %t would wrap if incremented further!" pp t pp_obj;
    t'
  ) else (
    Debug.failf "Attempt to inc-ref on freed resource %t" pp_obj
  )

let pred ~pp t =
  if t > 0 then t - 1
  else if t = 0 then Debug.failf "Attempt to free already-freed resource %t" pp
  else leaked

let is_zero = function
  | 0 -> true
  | _ -> false

let check ~pp t =
  if t < 1 then
    Debug.invariant_broken (fun f -> Fmt.pf f "Already unref'd! %t" pp)

let to_int t = t
