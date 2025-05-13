(***************************************************************************)
(*
    chatml_parser.mly

    A minimal parser for our DSL.
    Build with: menhir chatml_parser.mly
    Produces: chatml_parser.ml, chatml_parser.mli
*)
(***************************************************************************)

%{
open Chatml_lang
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
%type <Chatml_lang.program> program

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
| LET REC rec_bindings          { SLetRec($3) }
| LET REC rec_bindings IN expr_sequence  { SExpr( ELetRec($3, $5) ) }  
| LET LIDENT params EQ expr_sequence { SLet($2, ELambda($3, $5)) }
| LET LIDENT EQ expr_sequence        { SLet($2, $4) }
| MODULE UIDENT EQ STRUCT stmts END { SModule($2, $5) }
| OPEN UIDENT               { SOpen($2) }
| expr                      { SExpr($1) }

expr:
    (* basic literals *)
    | INT                     { EInt $1 }
    | FLOAT                   { EFloat $1 }
    | BOOL                    { EBool $1 }
    | STRING                  { EString $1 }

    | expr PLUS expr       { EApp(EVar "+", [$1; $3]) }
    | expr MINUS expr      { EApp(EVar "-", [$1; $3]) }
    | expr STAR expr       { EApp(EVar "*", [$1; $3]) }
    | expr SLASH expr      { EApp(EVar "/", [$1; $3]) }

    | expr LT expr { EApp(EVar "<", [$1; $3]) }
    | expr GT expr { EApp(EVar ">", [$1; $3]) }
    | expr LTEQ expr { EApp(EVar "<=", [$1; $3]) }
    | expr GTEQ expr { EApp(EVar ">=", [$1; $3]) }
    | expr EQEQ expr { EApp(EVar "==", [$1; $3]) }
    | expr BANGEQ expr { EApp(EVar "!=", [$1; $3]) }

    (* variable / identifier *)
    | ident { $1 }

    (* function definition *)
    | FUN params ARROW expr_sequence   { ELambda($2, $4) }

    (* function application: e.g. id(expr, expr) *)
    | LIDENT LPAREN expr_list RPAREN { EApp(EVar $1, $3) }

    (* Polymorphic variant: `Variant or `Variant(expr1, expr2) *)
    | TICKIDENT               { EVariant($1, []) }
    | TICKIDENT LPAREN expr_list RPAREN { EVariant($1, $3) }

    (* If expression *)
    | IF expr_sequence THEN expr_sequence ELSE expr_sequence  { EIf($2, $4, $6) }

    (* While expression *)
    | WHILE expr_sequence DO expr_sequence DONE      { EWhile($2, $4) }

    | LET REC rec_bindings IN expr_sequence  { ELetRec($3, $5) }

    (* Let-in expression *)
    | LET LIDENT EQ expr_sequence IN expr_sequence { ELetIn($2, $4, $6) }
	| LET LIDENT params EQ expr_sequence IN expr_sequence { ELetIn($2, ELambda($3, $5), $7) }

    (* match with *)
    | MATCH expr_sequence WITH pattern_cases { EMatch($2, $4) }

    (* record extension { base with a = expr; ... } *)
    | LBRACE expr_sequence WITH field_decls RBRACE { ERecordExtend($2, $4) }
    (* record literal *)
    | LBRACE field_list RBRACE      { ERecord($2) }

    (* record field access, e.g. expr.field *)
    | expr DOT LIDENT              { EFieldGet($1, $3) }

    | expr DOT LIDENT LPAREN expr_list RPAREN
		    { EApp(EFieldGet($1, $3), $5) }

    (* record field set, e.g. expr.field <- expr *)
    | expr DOT LIDENT LEFTARROW expr { EFieldSet($1, $3, $5) }

    (* array literal *)
    | LBRACKET expr_list RBRACKET  { EArray($2) }

    (* array get, e.g. arr[expr] *)
    | expr LBRACKET expr RBRACKET  { EArrayGet($1, $3) }

    (* array set, e.g. arr[expr] <- expr *)
    | expr LBRACKET expr RBRACKET LEFTARROW expr { EArraySet($1, $3, $6) }

    (* parentheses *)
    | LPAREN expr_sequence RPAREN           { $2 }

    (* references *)
    | REF expr                     { ERef($2) }
    | expr COLONEQ expr            { ESetRef($1, $3) }
    | BANG expr                    { EDeref($2) }

    (* sequence of expressions *)
    | BANGEQ expr                  { failwith "Unsupported: != as a pattern" }
    | MINUS expr                   %prec MINUS { (* unary minus if needed *) EApp(EVar "-", [$2]) }
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
| LIDENT params EQ expr_sequence { ($1, ELambda($2, $4)) }

expr_sequence:
| expr  { $1 }                        
| expr SEMI expr_sequence { ESequence($1, $3) }
