open Syntax
open Support.Error

(* ------------------------   SMALL STEPS EVALUATION  ------------------------ *)

exception NoRuleApplies

let rec isNumericVal t = 
  match t with
    TermZero(_) -> true
  | TermSucc(_, t') -> isNumericVal t'
  | _ -> false

let rec smallStep term = 
  match term with
    TermIf(_, TermTrue(_), t2, _) -> t2
  | TermIf(_, TermFalse(_), _, t3) -> t3
  | TermIf(fileInfo, t1, t2, t3) -> 
      let t1' = smallStep t1 in
      TermIf(fileInfo, t1', t2, t3)
  | TermSucc(fileInfo, t1) ->
      let t1' = smallStep t1 in
      TermSucc(fileInfo, t1')
  | TermPred(_, TermZero(_)) -> TermZero(dummyinfo)
  | TermPred(_, TermSucc(_, nv)) when isNumericVal nv -> nv
  | TermPred(fileInfo, t1) -> 
      let t1' = smallStep t1 in
      TermPred(fileInfo, t1')
  | TermIsZero(_, TermZero(_)) -> TermTrue(dummyinfo)
  | TermIsZero(_, TermSucc(_, nv)) when isNumericVal nv -> TermFalse(dummyinfo)
  | TermIsZero(fileInfo, t1) -> 
      let t1' = smallStep t1 in
      TermIsZero(fileInfo, t1')
  | _ -> raise NoRuleApplies

let rec smallStepEval term = 
  try 
    let term' = smallStep term in
    smallStepEval term'
  with 
    NoRuleApplies -> term