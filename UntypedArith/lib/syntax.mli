(* module Syntax: syntax trees and associated support functions *)

open Support.Error

(* Data type definitions *)
type term = 
    TermTrue of info
  | TermFalse of info
  | TermIf of info * term * term * term
  | TermZero of info
  | TermSucc of info * term
  | TermPred of info * term
  | TermIsZero of info * term

type command = 
    Import of string
  | Eval of info * term

(* Printing *)
val printTerm : term -> unit
val printATerm : bool -> term -> unit
val termInfo : term -> info