open Format
open String
open SimplyTyped.Support.Error
open SimplyTyped.Support.Pervasive
open SimplyTyped.Syntax
open SimplyTyped.Semantic

module Lexer = SimplyTyped.Lexer
module Parser = SimplyTyped.Parser

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
        fun ctx -> ([], ctx)
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

let rec processFile file ctx =
  if file = "repl" then
    try 
      let text = readTilSemi () in
      let cmds, _ = (parseString text ctx) in
      processFile "repl" (List.fold_left processCmds ctx cmds);
    with
      End_of_file -> print_endline "";
      ctx 
  else if List.mem file (!alreadyImported) then
    ctx
  else 
    (
      alreadyImported := file :: !alreadyImported;
      let cmds, _ = parseFile file ctx in
      List.fold_left processCmds ctx cmds
    )
and processCmds ctx cmd = 
  open_hvbox 0;
  let ctx' = processCmd ctx cmd in
  print_flush ();
  ctx'

and processCmd ctx cmd = match cmd with
  | Import file -> 
      processFile file ctx
  | Eval(_, term) -> 
      let term' = eval ctx term in
      printATerm true ctx term';
      force_newline ();
      ctx
  | Bind(_, x, bind) -> 
      ps x;
      ps " ";
      printBinding ctx bind;
      force_newline ();
      addBinding ctx x bind


let main () = 
  let inFile = parseArgs () in 
  (* print_string "File: "; print_string inFile; print_newline (); *)
  let _ = processFile inFile emptyContext in
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