(* module Syntax: syntax trees and associated support functions *)

open Support.Error

(* Data type definitions *)

type ty = 
  | TypeBool
  | TypeArr of ty * ty

type term = 
  | TermVar of info * int * int
  | TermAbs of info * string * ty * term
  | TermApp of info * term * term
  | TermTrue of info
  | TermFalse of info
  | TermIf of info * term * term * term

type binding = NameBind | VarBind of ty

type command = 
    Import of string
  | Eval of info * term
  | Bind of info * string * binding

type context = (string * binding) list

(* Context management *)
val emptyContext : context
val ctxLength : context -> int
val addBinding : context -> string -> binding -> context
val addName : context -> string -> context
val isNameBound : context -> string -> bool
val getBinding : info -> context -> int -> binding
val index2name : info -> context -> int -> string
val name2index : info -> context -> string -> int
val getTypeFromContext : info -> context -> int -> ty

(* Shifting *)
val termShift : int -> term -> term
val termSubstTop : term -> term -> term

(* Printing *)
val printTerm : context -> term -> unit
val printATerm : bool -> context -> term -> unit
val printBinding : context -> binding -> unit

(* MISC *)
val termInfo : term -> info