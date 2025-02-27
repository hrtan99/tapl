/*  
 *  Yacc grammar for the parser. The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
open Support.Error
open Syntax
%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* We first list all the tokens mentioned in the parsing rules
   below.  The names of the tokens are common to the parser and the
   generated lexical analyzer.  Each token is annotated with the type
   of data that it carries; normally, this is just file information
   (which is used by the parser to annotate the abstract syntax trees
   that it constructs), but sometimes -- in the case of identifiers and
   constant values -- more information is provided.
 */

/* Keyword tokens */
%token <Support.Error.info> IMPORT
%token <Support.Error.info> LAMBDA
%token <Support.Error.info> IF
%token <Support.Error.info> THEN
%token <Support.Error.info> ELSE
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE
%token <Support.Error.info> BOOL

/* Identifier and constant value tokens */
%token <string Support.Error.withInfo> UCID  /* uppercase-initial */
%token <string Support.Error.withInfo> LCID  /* lowercase/symbolic-initial */
%token <int Support.Error.withInfo> INTV
%token <float Support.Error.withInfo> FLOATV
%token <string Support.Error.withInfo> STRINGV

/* Symbolic tokens */
%token <Support.Error.info> APOSTROPHE
%token <Support.Error.info> DQUOTE
%token <Support.Error.info> ARROW
%token <Support.Error.info> BANG
%token <Support.Error.info> BARGT
%token <Support.Error.info> BARRCURLY
%token <Support.Error.info> BARRSQUARE
%token <Support.Error.info> COLON
%token <Support.Error.info> COLONCOLON
%token <Support.Error.info> COLONEQ
%token <Support.Error.info> COLONHASH
%token <Support.Error.info> COMMA
%token <Support.Error.info> DARROW
%token <Support.Error.info> DDARROW
%token <Support.Error.info> DOT
%token <Support.Error.info> EOF
%token <Support.Error.info> EQ
%token <Support.Error.info> EQEQ
%token <Support.Error.info> EXISTS
%token <Support.Error.info> GT
%token <Support.Error.info> HASH
%token <Support.Error.info> LCURLY
%token <Support.Error.info> LCURLYBAR
%token <Support.Error.info> LEFTARROW
%token <Support.Error.info> LPAREN
%token <Support.Error.info> LSQUARE
%token <Support.Error.info> LSQUAREBAR
%token <Support.Error.info> LT
%token <Support.Error.info> RCURLY
%token <Support.Error.info> RPAREN
%token <Support.Error.info> RSQUARE
%token <Support.Error.info> SEMI
%token <Support.Error.info> SLASH
%token <Support.Error.info> STAR
%token <Support.Error.info> TRIANGLE
%token <Support.Error.info> USCORE
%token <Support.Error.info> VBAR

/* ---------------------------------------------------------------------- */
/* The starting production of the generated parser is the syntactic class
   toplevel.  The type that is returned when a toplevel is recognized is
     Syntax.context -> (Syntax.command list * Syntax.context) 
   that is, the parser returns to the user program a function that,
   when given a naming context, returns a fully parsed list of
   Syntax.commands and the new naming context that results when
   all the names bound in these commands are defined.

   All of the syntactic productions in the parser follow the same pattern:
   they take a context as argument and return a fully parsed abstract
   syntax tree (and, if they involve any constructs that bind variables
   in some following phrase, a new context).
   
*/

%start toplevel
%type < Syntax.context -> (Syntax.command list * Syntax.context) > toplevel
%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */
toplevel :
    EOF
      { fun ctx -> [], ctx }
  | Command SEMI toplevel
      { fun ctx ->
          let cmd, ctx = $1 ctx in
          let cmds, ctx = $3 ctx in
          cmd::cmds, ctx }

/* A top-level command */
Command :
    IMPORT STRINGV { fun ctx -> (Import($2.v)),ctx }
  | Term 
      { fun ctx -> (let t = $1 ctx in Eval(termInfo t,t)),ctx }
  | LCID Binder
      { fun ctx -> ((Bind($1.i,$1.v,$2 ctx)), addName ctx $1.v) }

/* Right-hand sides of top-level bindings */
Binder :
    COLON Type 
      { fun ctx -> VarBind($2 ctx) }

/* All type expressions */
Type :
    ArrowType
                { $1 }

/* Atomic types are those that never need extra parentheses */
AType :
    LPAREN Type RPAREN  
           { $2 } 
  | BOOL
      { fun _ -> TypeBool }

/* An "arrow type" is a sequence of atomic types separated by
   arrows. */
ArrowType :
    AType ARROW ArrowType
     { fun ctx -> TypeArr($1 ctx, $3 ctx) }
  | AType
            { $1 }

Term :
    AppTerm
      { $1 }
  | LAMBDA LCID COLON Type DOT Term 
      { fun ctx ->
          let ctx1 = addName ctx $2.v in
          TermAbs($1, $2.v, $4 ctx, $6 ctx1) }
  | LAMBDA USCORE COLON Type DOT Term 
      { fun ctx ->
          let ctx1 = addName ctx "_" in
          TermAbs($1, "_", $4 ctx, $6 ctx1) }
  | IF Term THEN Term ELSE Term
      { fun ctx -> TermIf($1, $2 ctx, $4 ctx, $6 ctx) }

AppTerm :
    ATerm
      { $1 }
  | AppTerm ATerm
      { fun ctx ->
          let e1 = $1 ctx in
          let e2 = $2 ctx in
          TermApp(termInfo e1, e1, e2) }

/* Atomic terms are ones that never require extra parentheses */
ATerm :
    LPAREN Term RPAREN  
      { $2 } 
  | LCID 
      { fun ctx ->
          TermVar($1.i, name2index $1.i ctx $1.v, ctxLength ctx) }
  | TRUE
      { fun _ -> TermTrue($1) }
  | FALSE
      { fun _ -> TermFalse($1) }



/*   */
