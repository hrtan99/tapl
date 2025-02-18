(* module Semantic

   typechecking and evaluation semantic functions
*)


open Syntax


(* ------------------------   SMALL STEPS EVALUATION  ------------------------ *)

val isVal : context -> term -> bool
val eval : context -> term -> term