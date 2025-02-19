(* open Format *)
open Format
open Support.Error
open Support.Pervasive

(* Datatypes *)
type ty = 
  | TypeBool
  | TypeArr of ty * ty


type term =
    TermVar of info * int * int
  | TermAbs of info * string * ty * term
  | TermApp of info * term * term
  | TermTrue of info
  | TermFalse of info
  | TermIf of info * term * term * term

type binding =
    NameBind 
  | VarBind of ty

type context = (string * binding) list

type command = 
    Import of string
  | Eval of info * term
  | Bind of info * string * binding


(* Context management *)
let emptyContext = []

let ctxLength ctx = List.length ctx

let addBinding ctx x bind = (x, bind) :: ctx

let addName ctx x = addBinding ctx x NameBind

let rec isNameBound ctx x = 
  match ctx with
    [] -> false
  | (y, _) :: rest -> 
    if y = x then 
      true
    else isNameBound rest x


let rec pickFreshName ctx x = 
  if isNameBound ctx x then 
    pickFreshName ctx (x ^ "'")
  else 
    (addName ctx x, x)


let index2name fileInfo ctx x = 
  try 
    let (xn, _) = List.nth ctx x in xn
  with 
    Failure _ -> 
      let msg = Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
      errAt fileInfo (msg x (List.length ctx))

let rec name2index fileInfo ctx x = 
  match ctx with
    [] -> 
      let msg = Printf.sprintf "Identifier %s is unbound" in
      errAt fileInfo (msg x)
  | (y, _) :: rest -> 
      if y = x then 0
      else 1 + name2index fileInfo rest x


(* Shifting *)
let termMap onvar c t = 
  let rec walk c t = 
    match t with
    | TermVar(fileInfo, x, n) -> onvar fileInfo c x n
    | TermAbs(fileInfo, x, ty, t1) -> TermAbs(fileInfo, x, ty, walk (c + 1) t1)
    | TermApp(fileInfo, t1, t2) -> TermApp(fileInfo, walk c t1, walk c t2)
    | TermTrue(fileInfo) -> TermTrue(fileInfo)
    | TermFalse(fileInfo) -> TermFalse(fileInfo)
    | TermIf(fileInfo, t1, t2, t3) -> TermIf(fileInfo, walk c t1, walk c t2, walk c t3)
  in walk c t

let termShiftAbove d c t = 
  termMap 
    (
      fun fileInfo c x n -> 
        if x >= c then 
          TermVar(fileInfo, x + d, n + d)
        else 
          TermVar(fileInfo, x, n + d)
    )
    c t

let termShift d t = termShiftAbove d 0 t

(* Substitution *)
let termSubst j s t = 
  termMap 
    (
      fun fileInfo c x n -> 
        if x = j + c then termShift c s
        else TermVar(fileInfo, x, n)
    )
    0 t

let termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)


(* Context management (continued) *)
let getBinding fileInfo ctx i = 
  try 
    let (_, bind) = List.nth ctx i in bind
  with 
    Failure _ -> 
      let msg = Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
      errAt fileInfo (msg i (List.length ctx))

let getTypeFromContext fileInfo ctx i =
  match getBinding fileInfo ctx i with
  | VarBind(ty) -> ty
  | _ -> errAt fileInfo ("getTypeFromContext: Wrong kind of binding for variable" ^ (index2name fileInfo ctx i))


(* Extracting file info *)
let termInfo term = 
  match term with
  | TermVar(fileInfo, _, _) -> fileInfo
  | TermAbs(fileInfo, _, _, _) -> fileInfo
  | TermApp(fileInfo, _, _) -> fileInfo
  | TermTrue(fileInfo) -> fileInfo
  | TermFalse(fileInfo) -> fileInfo
  | TermIf(fileInfo, _, _, _) -> fileInfo

let small term = 
  match term with
  | TermVar(_, _, _) -> true
  | _ -> false

let rec printTypeStr outer ty = match ty with
  | ty -> printArrowType outer ty

and printArrowType outer ty = match ty with 
  | TypeArr(ty1, ty2) -> 
      obox ();
      printArrowType false ty1;
      if outer then ps " ";
      ps "->";
      if outer then print_space () else break ();
      printArrowType outer ty2;
      cbox ()
  | ty -> printAType outer ty

and printAType outer ty = match ty with
  | TypeBool -> ps "Bool"
  | ty -> 
      ps "("; printTypeStr outer ty; ps ")"

let printType ty = printTypeStr true ty

(* Printing *)
let rec printTermStr outer ctx term = match term with
  | TermAbs(_, x, termType, t) -> 
      let (ctx', x') = pickFreshName ctx x in
      obox ();
      ps "lambda "; ps x'; ps ":";
      printTypeStr false termType;
      ps "."; 
      if (small t) && not outer then
        break ()
      else
        print_space ();
      printTermStr outer ctx' t;
      cbox ();
  | TermIf(_, t1, t2, t3) ->
      obox ();
      ps "if "; printTermStr false ctx t1; ps " then ";
      printTermStr false ctx t2; ps " else ";
      printTermStr false ctx t3;
      cbox ()
  | term -> printAppTerm outer ctx term

and printAppTerm outer ctx term =
  match term with
  | TermApp(_, t1, t2) -> 
      obox ();
      printAppTerm false ctx t1;
      print_space ();
      printATerm false ctx t2;
      cbox ();
  | t -> printATerm outer ctx t

and printATerm outer ctx term =
  match term with
  | TermVar(fileInfo, x, n) -> 
      if ctxLength ctx = n then 
        ps (index2name fileInfo ctx x)
      else 
        ps ("[bad index]" ^ (string_of_int x) ^ "/" ^ (string_of_int n)
          ^ " in {"
          ^ (List.fold_left (fun str (x, _) -> str ^ " " ^x) "" ctx)
          ^ "}"
          )
  | TermTrue(_) -> ps "true"
  | TermFalse(_) -> ps "false"
  | term -> 
      ps "("; printTermStr outer ctx term; ps ")"

let printTerm ctx term = printTermStr true ctx term

let printBinding _ bind = match bind with
    NameBind -> ()
  | VarBind(ty) -> ps ": "; printType ty
