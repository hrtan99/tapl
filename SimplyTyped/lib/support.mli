(* module Support

   Collects a number of low-level facilities used by the other modules
   in the typechecker/evaluator. 

*)



(* Error printing utilities -- opened everywhere by convention *)

module Error : sig

  (* 
    An exception raised by the low-level error printer; exported
    here so that it can be caught in module Main and converted into
    an exit status for the whole program. 
  *)
  exception Exit of int

  (* 
    An element of the type info represents a "file position": a 
    file name, line number, and character position within the line.  
    Used for printing error messages.
  *)
  type info
  val dummyinfo : info

  (* Create file position info: filename, lineno, column *)
  val createInfo : string -> int -> int -> info
  val printInfo : info -> unit

  (* 
    A convenient datatype for a "value with file info."  Used in
    the lexer and parser.
  *)
  type 'a withInfo = {i: info; v: 'a}

  (* 
    Print an error message and fail. The printing function is called
    in a context where the formatter is processing an hvbox. Insert
    calls to Format.print_space to print a space or, if necessary,
    break the line at that point. 
  *)
  val errf : (unit -> unit) -> 'a
  val errfAt : info -> (unit -> unit) -> 'a

  (* 
    Convenient wrappers for the above, for the common case where the
    action to be performed is just to print a given string.
  *)
  val err : string -> 'a
  val errAt : info -> string -> 'a


  (* Variants that print a message but do not fail afterwards *)
  val warn : string -> unit
  val warnAt : info -> string -> unit

end


(* Some pervasive abbreviations -- opened everywhere by convention *)
module Pervasive : sig
  val ps : string -> unit
  val obox : unit -> unit
  val obox0 : unit -> unit
  val cbox : unit -> unit
  val break : unit -> unit
end








