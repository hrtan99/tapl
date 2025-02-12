open Format
open String
open Arith.Support.Error
open Arith.Syntax
open Arith.Semantic

module Lexer = Arith.Lexer
module Parser = Arith.Parser

let searchPath = ref [""]
let argDefs = [
  "-I",
  Arg.String (fun f -> searchPath := f::!searchPath),
  "Append a directory to the search path"
]

let parseArgs () = 
  let inFile = ref (None : string option) in
  Arg.parse argDefs
    (fun s-> 
      match !inFile with
          Some(_) -> err "You must specify exactly one input file"
        | None -> inFile := Some s
    )
    "";
  match !inFile with
      None -> err "You must specify an input file"
    | Some(f) -> f


let openFile inFile = 
  List.iter (fun dir -> print_endline ("search path: " ^ dir)) !searchPath;
  let rec tryNext list = match list with
      [] -> err ("Can't find file " ^ inFile)
    | (dir::rest) -> 
        let name = if dir = "" then inFile else dir ^ "/" ^ inFile in
        try 
          open_in name 
        with 
          Sys_error _ -> tryNext rest
  in tryNext !searchPath


let parseFile inFile = 
  let pi = openFile inFile in
  let lexBuf = Lexer.create inFile pi in
  let result = 
    try
      Parser.toplevel Lexer.main lexBuf
    with 
      Parsing.Parse_error ->
        errAt (Lexer.info lexBuf) "Parse error"
  in
  Parsing.clear_parser ();
  close_in pi;
  result

let parseString str =
  let lexBuf = Lexer.createFromStr str in
  let result = 
    try
      Parser.toplevel Lexer.main lexBuf
    with 
      Parsing.Parse_error ->
        print_endline "Parse error";
        print_flush ();
        []
  in
  Parsing.clear_parser ();
  result

let rec readTilSemi ?(prompt = " > ") () =
  print_string prompt;
  print_flush ();
  let line = read_line () in
  if ends_with ~suffix:";" line then
    line
  else
    line ^ (readTilSemi ~prompt: "+> " ())

let alreadyImported = ref ([] : string list)

let rec processFile file =
  if file = "repl" then
    try
      let text = readTilSemi () in
      let cmds = parseString text in
      List.iter processCmds cmds;
      processFile "repl";
      ()
    with
      End_of_file -> print_endline "";
      ()
  else if List.mem file (!alreadyImported) then
    ()
  else
    alreadyImported := file :: !alreadyImported;
    let cmds = parseFile file in
    List.iter processCmds cmds
and processCmds cmd = 
  open_hvbox 0;
  processCmd cmd;
  print_flush ();
and processCmd cmd = match cmd with
    Import file -> processFile file
  | Eval(_, t) -> 
      (* let t' = smallStepEval t in  *)
      let t' = bigStepEval t in
      printATerm true t';
      force_newline ();
      ()

let main () = 
  let inFile = parseArgs () in 
  (* print_string "File: "; print_string inFile; print_newline (); *)
  let _ = processFile inFile in
  ()


let () = set_max_boxes 1000
let () = set_margin 67

let res = 
  try
    main ();
    0
  with
    Exit x -> x

let () = print_flush ()
let () = exit res