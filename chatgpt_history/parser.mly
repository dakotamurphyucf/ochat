%token <(Lexing.position * Lexing.position * string)> DOC_STRING
%token EOF
%start <(Lexing.position * Lexing.position * string) option> doc
%%


doc:
  | s = DOC_STRING { Some s }
  | EOF            { None   }