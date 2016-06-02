/*  
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
open Support.Error
open Support.Pervasive
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
%token <Support.Error.info> IF
%token <Support.Error.info> THEN
%token <Support.Error.info> ELSE
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE
%token <Support.Error.info> SUCC
%token <Support.Error.info> PRED
%token <Support.Error.info> ISZERO

%token <Support.Error.info> LAMBDA
%token <Support.Error.info> TYPEBOOL
%token <Support.Error.info> TYPENAT

%token <Support.Error.info> TYPETOP

/* Identifier and constant value tokens */
%token <string Support.Error.withinfo> UCID  /* uppercase-initial */
%token <string Support.Error.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int Support.Error.withinfo> INTV
%token <float Support.Error.withinfo> FLOATV
%token <string Support.Error.withinfo> STRINGV

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
   Syntax.command list.
*/

/*
%start toplevel
%type < Syntax.command list > toplevel
%%
*/

%start toplevel
%type < Syntax.context ->  (Syntax.command list * Syntax.context) > toplevel
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
          let cmd,ctx = $1 ctx in
          let cmds,ctx = $3 ctx in
          cmd::cmds, ctx }


/* Type Meta Variables */
AType :
    LPAREN Type RPAREN
      { $2 }
  | TYPEBOOL
      { fun ctx -> TyBool }
  | TYPENAT
      { fun ctx -> TyNat }
  | TYPETOP
      { fun ctx -> TyTop }
  | LCURLY FieldTypes RCURLY
      { fun ctx -> TyRecord($2 ctx 1) }

FieldTypes :
  /* empty */
    { fun ctx i -> [] }
  | NEFieldTypes
    { $1 }

NEFieldTypes :
    FieldType
      { fun ctx i -> [$1 ctx i] }
  | FieldType COMMA NEFieldTypes
      { fun ctx i -> ($1 ctx i) :: ($3 ctx (i + 1)) }

FieldType :
    LCID COLON Type
      { fun ctx i -> ($1.v, $3 ctx) }
  | Type
      { fun ctx i -> (string_of_int i, $1 ctx) }

ArrowType :
  AType ARROW AType
    { fun ctx -> TyArrow($1 ctx, $3 ctx) }

Type :
    ArrowType
      { $1 }
  | AType
      { $1 }

/* A top-level command */
Command :
  | Term 
      { fun ctx -> (let t = $1 ctx in Eval(tmInfo t,t)), ctx }

Term :
    AppTerm
      { $1 }
  | IF Term THEN Term ELSE Term
      { fun ctx -> TmIf($1, $2 ctx, $4 ctx, $6 ctx) }
  | LAMBDA LCID COLON Type DOT Term
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TmAbs($1, $2.v, $4 ctx1, $6 ctx1) }

AppTerm :
    ATerm
      { $1 }
  | AppTerm ATerm
      { fun ctx ->
          let e1 = $1 ctx in
          let e2 = $2 ctx in
          TmApp(tmInfo e1, e1, e2) }
  | SUCC ATerm
      { fun ctx -> TmSucc($1, $2 ctx) }
  | PRED ATerm
      { fun ctx -> TmPred($1, $2 ctx) }
  | ISZERO ATerm
      { fun ctx -> TmIsZero($1, $2 ctx) }

/* Atomic terms are ones that never require extra parentheses */
ATerm :
    LPAREN Term RPAREN  
      { $2 } 
  | LCID
      { fun ctx -> 
          TmVar($1.i, name2index $1.i ctx $1.v, ctxlength ctx) }
  | TRUE
      { fun ctx -> TmTrue($1) }
  | FALSE
      { fun ctx -> TmFalse($1) }
  | INTV
      { fun ctx ->
          let rec f n = match n with
              0 -> TmZero($1.i)
            | n -> TmSucc($1.i, f (n-1))
          in f $1.v }

  /* Added for subtyping */
  | ATerm DOT LCID
      { fun ctx -> TmProj($2, $1 ctx, $3.v) }
  | ATerm DOT INTV
      { fun ctx -> TmProj($2, $1 ctx, string_of_int $3.v) }

  | LCURLY Fields RCURLY
      { fun ctx -> TmRecord($1, $2 ctx 1) }

Fields :
    /* empty */
      { fun ctx i -> [] }
  | NEFields
      { $1 }

NEFields :
    Field
      { fun ctx i -> [$1 ctx i] }
  | Field COMMA NEFields
      { fun ctx i -> ($1 ctx i) :: ($3 ctx (i+1)) }

Field :
    LCID EQ Term
      { fun ctx i -> ($1.v, $3 ctx) }
  | Term
      { fun ctx i -> (string_of_int i, $1 ctx) }


/*   */
