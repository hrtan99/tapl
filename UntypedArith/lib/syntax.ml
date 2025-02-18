(* open Format *)
open Support.Error
open Support.Pervasive

(* Datatypes *)
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

(* Extracting file info *)
let termInfo term = 
  match term with
    TermTrue fileInfo -> fileInfo
  | TermFalse fileInfo -> fileInfo
  | TermIf (fileInfo, _, _, _) -> fileInfo
  | TermZero fileInfo -> fileInfo
  | TermSucc (fileInfo, _) -> fileInfo
  | TermPred (fileInfo, _) -> fileInfo
  | TermIsZero (fileInfo, _) -> fileInfo


(* Printing *)
let rec printTermStr outer term = 
  match term with
    TermIf(_, t1, t2, t3) -> 
      obox0 ();
      ps "if ";
      printTermStr false t1;
      ps " then ";
      printTermStr false t2;
      ps " else ";
      printTermStr false t3;
      cbox ()
  | t -> printAppTerm outer t

and printAppTerm outer term =
  match term with
    TermPred(_, t) -> ps "psed "; printATerm false t
  | TermIsZero(_, t) -> ps "iszero "; printATerm false t
  | t -> printATerm outer t

and printATerm outer term =
  match term with
    TermTrue _ -> ps "true"
  | TermFalse _ -> ps "false"
  | TermZero _ -> ps "0"
  | TermSucc(_, t) -> 
      let rec printSucc t = 
        match t with
          TermZero _ -> ps "0"
        | TermSucc(_, s) -> printSucc s
        | _ -> ps "succ "; printATerm false t
      in printSucc t
  | t -> ps "("; printTermStr outer t; ps ")"

let printTerm term = printTermStr true term

