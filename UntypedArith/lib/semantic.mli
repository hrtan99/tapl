(* module Semantic

   typechecking and evaluation semantic functions
*)


open Syntax


val smallStepEval : term -> term
val bigStepEval : term -> term