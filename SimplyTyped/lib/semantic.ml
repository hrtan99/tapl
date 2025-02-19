open Syntax
open Support.Error

(* ------------------------   SMALL STEPS EVALUATION  ------------------------ *)

exception NoRuleApplies

let isVal _ term = 
  match term with
  | TermTrue(_) -> true
  | TermFalse(_) -> true
  |  TermAbs(_, _, _, _) -> true
  | _ -> false


let rec smallStep ctx term = 
  match term with
    TermApp(_, TermAbs(_, _, _, t12), v2) when isVal ctx v2 -> 
      termSubstTop v2 t12
  | TermApp(fileInfo, v1, t2) when isVal ctx v1 -> 
      let t2' = smallStep ctx t2 in
      TermApp(fileInfo, v1, t2')
  | TermApp(fileInfo, t1, t2) -> 
      let t1' = smallStep ctx t1 in
      TermApp(fileInfo, t1', t2)
  | TermIf(_, TermTrue(_), t2, _) -> t2
  | TermIf(_, TermFalse(_), _, t3) -> t3
  | TermIf(fileInfo, t1, t2, t3) -> 
      let t1' = smallStep ctx t1 in
      TermIf(fileInfo, t1', t2, t3)
  | _ -> raise NoRuleApplies


let rec eval ctx t = 
  try 
    let t' = smallStep ctx t in 
    eval ctx t'
  with NoRuleApplies -> t


 (* ------------------------   TYPING RULES  ------------------------ *)
 
let rec typeOf ctx term = 
  match term with
    TermVar(fileInfo, i, _) -> getTypeFromContext fileInfo ctx i
  | TermAbs(_, x, tyT1, t2) -> 
      let ctx' = addBinding ctx x (VarBind(tyT1)) in
      let tyT2 = typeOf ctx' t2 in
      TypeArr(tyT1, tyT2)
  | TermApp(fileInfo, t1, t2) -> 
      let tyT1 = typeOf ctx t1 in
      let tyT2 = typeOf ctx t2 in
      (match tyT1 with
        TypeArr(tyT11, tyT12) -> 
          if (=) tyT2 tyT11 then tyT12
          else errAt fileInfo "parameter type mismatch"
      | _ -> errAt fileInfo "arrow type expected")
  | TermTrue(_) -> TypeBool
  | TermFalse(_) -> TypeBool
  | TermIf(fileInfo, t1, t2, t3) -> 
      if (=) (typeOf ctx t1) TypeBool then
        let tyT2 = typeOf ctx t2 in
        if (=) tyT2 (typeOf ctx t3) then tyT2
        else errAt fileInfo "arms of conditional have different types"
      else errAt fileInfo "guard of conditional not a boolean"