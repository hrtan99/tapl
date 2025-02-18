open Format


module Error = struct
  exception Exit of int

  type info = FILEINFO of string * int * int | UNKNOWN
  let dummyinfo = UNKNOWN

  let createInfo file line cloumn = FILEINFO(file, line, cloumn)
  let printInfo = 
  (* 
    In the text of the book, file positions in error messages are replaced
    with the string "Error:" 
  *)
  function (* function 主要用于模式匹配, 适用于只依赖单个参数进行分支的情况. 可以省略 match 关键字, 使代码更简洁. *)
    | FILEINFO (file, line, column) -> 
      fprintf std_formatter "%s:%d.%d: " file line column
    | UNKNOWN -> 
      fprintf std_formatter "<Unknown file and line>: " 

  type 'a withInfo = {i: info; v: 'a}
  
  let errf printMsg = 
    print_flush ();
    open_vbox 0;
    open_hvbox 0;
    printMsg ();
    print_cut ();
    close_box ();
    print_newline ();
    raise (Exit 1)
  
  let errfAt fileInfo printMsg = errf (fun () -> printInfo fileInfo; print_space (); printMsg ())

  let err msg = errf (fun () -> print_string "Error: "; print_string msg; print_newline ())

  let errAt fileInfo msg = errfAt fileInfo (fun () -> print_string msg; print_newline ())
  
  let warn msg = 
    print_string "Warning: ";
    print_string msg;
    print_newline ()

  let warnAt fileInfo msg =
    printInfo fileInfo;
    print_string "Warning: ";
    print_string msg;
    print_newline ()
end


module Pervasive = struct
  (* 
    The printing functions call these utility functions to insert grouping
    information and line-breaking hints for the pretty-printing library:
      obox  Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
      obox0 Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
      cbox  Close the current box
      break Insert a breakpoint indicating where the line maybe broken if
            necessary.
    See the documentation for the Format module in the OCaml library for
    more details. 
  *)
  let obox () = open_hvbox 2
  let obox0 () = open_hvbox 0
  let cbox () = close_box ()
  let break () = print_break 0 0
  let ps = Format.print_string

end




