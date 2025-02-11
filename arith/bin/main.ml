open Format
(* open String *)
open Arith.Support.Error


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

let main () = 
  let inFile = parseArgs () in 
  print_string "File: "; print_string inFile; print_newline ();
  ()

let res = 
  try
    main ();
    0
  with
    Exit x -> x

let () = print_flush ()
let () = exit res