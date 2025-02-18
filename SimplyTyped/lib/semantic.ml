open Syntax
(* open Support.Error *)

(* ------------------------   SMALL STEPS EVALUATION  ------------------------ *)

exception NoRuleApplies

let isVal _ term = 
  match term with
    TermAbs(_, _, _) -> true
  | _ -> false


let rec smallStep ctx term = 
  match term with
    TermApp(_, TermAbs(_, _, t12), v2) when isVal ctx v2 -> 
      termSubstTop v2 t12
  | TermApp(fileInfo, v1, t2) when isVal ctx v1 -> 
      let t2' = smallStep ctx t2 in
      TermApp(fileInfo, v1, t2')
  | TermApp(fileInfo, t1, t2) -> 
      let t1' = smallStep ctx t1 in
      TermApp(fileInfo, t1', t2)
  | _ -> raise NoRuleApplies


let rec eval ctx t = 
  try 
    let t' = smallStep ctx t in 
    eval ctx t'
  with NoRuleApplies -> t