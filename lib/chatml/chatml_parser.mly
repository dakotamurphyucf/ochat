(***************************************************************************)
(*
    chatml_parser.mly

    A minimal parser for our DSL.
    Build with: menhir chatml_parser.mly
    Produces: chatml_parser.ml, chatml_parser.mli
*)
(***************************************************************************)

(** ChatML syntax parser.

    This Menhir grammar turns the token stream produced by
    {!module:Chatml_lexer} into the *abstract syntax tree* (AST)
    defined in {!module:Chatml_lang}.  The syntax is deliberately
    ML-flavoured so that anyone familiar with OCaml can skim through
    ChatML without feeling lost.

    {1 Entry points}

    Unless you pass [`--infer`] to Menhir, the generator exposes two
    families of entry points:

    • **Monolithic** API – [program]

      {[ val program
           : (Lexing.lexbuf -> token)  (** lexer function               *)
          -> Lexing.lexbuf              (** mutable input buffer         *)
          -> Chatml_lang.stmt_node list (** resulting top-level AST      *) ]}

      This is the traditional, non-incremental interface: it lexes and
      parses the whole file in one go, raising {!MenhirLib.Error} when
      it encounters a syntax error.

    • **Incremental** API – the sub-module {!module:Incremental}
      generated thanks to the [`--table`] flag (see the [dune]
      stanza).  It exposes persistent LR automaton checkpoints that
      can be fed tokens one by one, which is invaluable for editors
      and REPLs needing fine-grained error recovery.  Example:

      {[
        let supplier = MenhirLib.LexerUtil.make_supplier Chatml_lexer.token lexbuf in
        let checkpoint = Chatml_parser.Incremental.program lexbuf.lex_curr_p in
        MenhirLib.Engine.stream_checkpoint checkpoint supplier
      ]}

    {1 Grammar highlights}

    * Arithmetic and comparison operators are desugared into
      applications of the corresponding *variable* (e.g. [a + b] ↦
      `EApp (EVar "+", [a; b])`).  This facilitates overloading at
      the *resolver* phase.

    * Record and array operations (`{ a = 1 }`, `arr[i] <- x`, …) are
      encoded explicitly into the AST instead of via sugar so that the
      evaluator can pattern-match directly on dedicated constructors.

    {1 Error handling}

    The grammar relies on Menhir's default error strategy which raises
    [Failure _] (wrapping [MenhirLib.Error]) as soon as it cannot shift
    or reduce.  Downstream code should either:

    1. Catch the exception when using the monolithic API; or
    2. Use the incremental API that returns checkpoints you can inspect
       and resume after inserting *synthetic* tokens.

    {1 Limitations}

    • Unicode identifiers are not supported yet – this is inherited
      from the lexer.
    • No layout-sensitive syntax: everything has to be delimited by
      keywords or punctuation.
*)
%{

open Chatml_lang
open Source

(* ------------------------------------------------------------------- *)
(* Helper functions for location tracking                               *)
(* ------------------------------------------------------------------- *)

let position_of_lex (pos : Lexing.position) : Source.position =
  { line = pos.pos_lnum
  ; column = pos.pos_cnum - pos.pos_bol
  ; offset = pos.pos_cnum
  }

let span_of_lex (startp : Lexing.position) (endp : Lexing.position) : Source.span =
  { left = position_of_lex startp; right = position_of_lex endp }

let mk_node startp endp value =
  { value; span = span_of_lex startp endp }

  let mk_exprnode startp endp value =
  { value; span = span_of_lex startp endp }
%}

(***************************************************************************)
(* 1) Declare tokens & semantic types                                      *)
(***************************************************************************)

%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> STRING
%token <string> LIDENT
%token <string> UIDENT
%token <string> TICKIDENT
%token FUN IF THEN ELSE WHILE DO DONE LET IN MATCH WITH MODULE STRUCT END OPEN REF REC AND
%token ARROW LEFTARROW COLONEQ BANGEQ EQ EQEQ LTEQ GTEQ LT GT PLUS MINUS STAR SLASH LPAREN RPAREN UNDERSCORE
%token LBRACE RBRACE LBRACKET RBRACKET SEMI COMMA DOT BAR BANG
%token EOF

%start program
%type <Chatml_lang.stmt_node list> program

%left ELSE
%left EQ EQEQ BANGEQ LT GT
%left PLUS MINUS
%left STAR SLASH
%left BANG

%%

(***************************************************************************)
(* 2) Grammar rules                                                        *)
(***************************************************************************)

program:
    stmts EOF  { $1 }

stmts:
    stmts stmt { $1 @ [$2] }
| /* empty */ { [] }

stmt:
  LET REC rec_bindings          { mk_node $startpos $endpos (SLetRec( $3)) }
| LET REC rec_bindings IN expr_sequence  { mk_node $startpos $endpos (SExpr( mk_exprnode $startpos $endpos (ELetRec($3, $5)) )) }
| LET LIDENT params EQ expr_sequence { mk_node $startpos $endpos (SLet($2, mk_exprnode $startpos $endpos (ELambda($3, ($5))))) }
| LET LIDENT EQ expr_sequence        { mk_node $startpos $endpos (SLet($2, $4)) }
| MODULE UIDENT EQ STRUCT stmts END { mk_node $startpos $endpos (SModule($2, $5)) }
| OPEN UIDENT               { mk_node $startpos $endpos (SOpen($2)) }
| expr                      { mk_node $startpos $endpos (SExpr($1)) }

expr:
    (* basic literals *)
    | INT                     { mk_exprnode $startpos $endpos (EInt $1) }
    | FLOAT                   { mk_exprnode $startpos $endpos (EFloat $1) }
    | BOOL                    { mk_exprnode $startpos $endpos (EBool $1) }
    | STRING                  { mk_exprnode $startpos $endpos (EString $1) }

    | expr PLUS expr       { mk_exprnode $startpos $endpos (EApp(mk_exprnode $startpos $endpos (EVar "+"), [$1; $3])) }
    | expr MINUS expr      { mk_exprnode $startpos $endpos (EApp(mk_exprnode $startpos $endpos (EVar "-"), [$1; $3])) }
    | expr STAR expr       { mk_exprnode $startpos $endpos (EApp(mk_exprnode $startpos $endpos (EVar "*"), [$1; $3])) }
    | expr SLASH expr      { mk_exprnode $startpos $endpos (EApp(mk_exprnode $startpos $endpos (EVar "/"), [$1; $3])) }

    | expr LT expr { mk_exprnode $startpos $endpos (EApp(mk_exprnode $startpos $endpos (EVar "<"), [$1; $3])) }
    | expr GT expr { mk_exprnode $startpos $endpos (EApp(mk_exprnode $startpos $endpos (EVar ">"), [$1; $3])) }
    | expr LTEQ expr { mk_exprnode $startpos $endpos (EApp(mk_exprnode $startpos $endpos (EVar "<="), [$1; $3])) }
    | expr GTEQ expr { mk_exprnode $startpos $endpos (EApp(mk_exprnode $startpos $endpos (EVar ">="), [$1; $3])) }
    | expr EQEQ expr { mk_exprnode $startpos $endpos (EApp(mk_exprnode $startpos $endpos (EVar "=="), [$1; $3])) }
    | expr BANGEQ expr { mk_exprnode $startpos $endpos (EApp(mk_exprnode $startpos $endpos (EVar "!="), [$1; $3])) }

    (* variable / identifier *)
    | ident { mk_exprnode $startpos $endpos ($1) }

    (* function definition *)
    | FUN params ARROW expr_sequence   { mk_exprnode $startpos $endpos (ELambda($2, $4)) }

    (* function application: expr(expr, ...) – now allows any *expression* in
       function position, not just an identifier.  This generalisation
       enables higher-order patterns such as (f x) y and (fun z -> z) 42. *)
    | expr LPAREN expr_list RPAREN { mk_exprnode $startpos $endpos (EApp($1, List.map (fun sn -> sn) $3)) }

    (* Polymorphic variant: `Variant or `Variant(expr1, expr2) *)
    | TICKIDENT               { mk_exprnode $startpos $endpos (EVariant($1, [])) }
    | TICKIDENT LPAREN expr_list RPAREN { mk_exprnode $startpos $endpos (EVariant($1, List.map (fun sn -> sn) $3)) }

    (* If expression *)
    | IF expr_sequence THEN expr_sequence ELSE expr_sequence  { mk_exprnode $startpos $endpos (EIf($2, $4, $6)) }

    (* While expression *)
    | WHILE expr_sequence DO expr_sequence DONE      { mk_exprnode $startpos $endpos (EWhile($2, $4)) }

    | LET REC rec_bindings IN expr_sequence  { mk_exprnode $startpos $endpos (ELetRec($3, $5)) }

    (* Let-in expression *)
    | LET LIDENT EQ expr_sequence IN expr_sequence { mk_exprnode $startpos $endpos (ELetIn($2, $4, $6)) }
	| LET LIDENT params EQ expr_sequence IN expr_sequence { mk_exprnode $startpos $endpos (ELetIn($2, mk_exprnode $startpos $endpos (ELambda($3, $5)), $7)) }

    (* match with *)
    | MATCH expr_sequence WITH pattern_cases { mk_exprnode $startpos $endpos (EMatch($2, List.map (fun (s,sn) -> s,sn) $4)) }

    (* record extension { base with a = expr; ... } *)
    | LBRACE expr_sequence WITH field_decls RBRACE { mk_exprnode $startpos $endpos (ERecordExtend($2, List.map (fun (s,sn) -> s,sn) $4)) }
    (* record literal *)
    | LBRACE field_list RBRACE      { mk_exprnode $startpos $endpos (ERecord( List.map (fun (s,sn) -> s,sn) $2)) }

    (* record field access, e.g. expr.field *)
    | expr DOT LIDENT              { mk_exprnode $startpos $endpos (EFieldGet($1, $3)) }

    | expr DOT LIDENT LPAREN expr_list RPAREN
		    { mk_exprnode $startpos $endpos (EApp( mk_exprnode $startpos $endpos (EFieldGet($1, $3)),  $5)) }

    (* record field set, e.g. expr.field <- expr *)
    | expr DOT LIDENT LEFTARROW expr { mk_exprnode $startpos $endpos (EFieldSet($1, $3, $5)) }

    (* array literal *)
    | LBRACKET expr_list RBRACKET  { mk_exprnode $startpos $endpos (EArray(List.map (fun sn -> sn) $2)) }

    (* array get, e.g. arr[expr] *)
    | expr LBRACKET expr RBRACKET  { mk_exprnode $startpos $endpos (EArrayGet($1, $3)) }

    (* array set, e.g. arr[expr] <- expr *)
    | expr LBRACKET expr RBRACKET LEFTARROW expr { mk_exprnode $startpos $endpos (EArraySet($1, $3, $6)) }

    (* parentheses *)
    | LPAREN expr_sequence RPAREN           { $2 }

    (* references *)
    | REF expr                     { mk_exprnode $startpos $endpos (ERef($2)) }
    | expr COLONEQ expr            { mk_exprnode $startpos $endpos (ESetRef($1, $3)) }
    | BANG expr                    { mk_exprnode $startpos $endpos (EDeref($2)) }

    (* sequence of expressions *)
    | BANGEQ expr                  { failwith "Unsupported: != as a pattern" }
    | MINUS expr                   %prec MINUS { mk_exprnode $startpos $endpos (EApp(mk_exprnode $startpos $endpos (EVar "-"), [$2]) ) }
    | DOT expr                     { failwith "Unexpected '.' expression" }
    /* fallback to handle conflicts if any. */

pattern_cases:
    pattern_case { [$1] }
| pattern_case pattern_cases { $1 :: $2 }

pattern_case:
   BAR pattern ARROW expr_sequence { ($2, $4) }

pattern:
    | UNDERSCORE      { PWildcard }
    | INT             { PInt $1 }
    | FLOAT           { PFloat $1 }
    | BOOL            { PBool $1 }
    | STRING          { PString $1 }
    | TICKIDENT       { PVariant($1, []) }
    | TICKIDENT LPAREN pattern_list RPAREN { PVariant($1, $3) }
    | LBRACE pattern_field_list RBRACE { PRecord(fst $2, snd $2) }
    | LIDENT          { PVar $1 }

pattern_list:
    pattern { [$1] }
| pattern COMMA pattern_list { $1 :: $3 }

pattern_field_list:
    /* empty */ { [], false }
  | pattern_field_decls { $1 }

opt_row_tail:
    /* nothing */ { false }
  | SEMI UNDERSCORE { true }

pattern_field_decls:
    pattern_field_decl opt_row_tail { [$1], $2 }
  | pattern_field_decl SEMI pattern_field_decls { $1 :: (fst $3), (snd $3) }

pattern_field_decl:
    LIDENT EQ pattern { ($1, $3) }

expr_list:
    /* empty */ { [] }
| expr_list_nonempty { $1 }

expr_list_nonempty:
    expr_sequence { [$1] }
| expr_sequence COMMA expr_list_nonempty { $1 :: $3 }

params:
    /* possibly multiple variable names for e.g. fun x y ->  ... */
    LIDENT { [$1] }
| LIDENT params { $1 :: $2 }

field_list:
    /* empty */ { [] }
| field_decls { $1 }

field_decls:
    field_decl { [$1] }
| field_decl SEMI field_decls { $1 :: $3 }

field_decl:
    LIDENT EQ expr { ($1, $3) }

ident:
| LIDENT   { EVar $1 }
| UIDENT   { EVar $1 }

rec_bindings:
| rec_binding                  { [$1] }
| rec_binding AND rec_bindings { $1 :: $3 }

rec_binding:
| LIDENT params EQ expr_sequence { ($1, mk_exprnode $startpos $endpos (ELambda($2, $4)) ) }

expr_sequence:
| expr  { $1 }                        
| expr SEMI expr_sequence { mk_exprnode $startpos $endpos (ESequence($1, $3)) }
