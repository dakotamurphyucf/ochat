(** The single ChatMD grammar, parameterized only by node construction.
    The ordinary builder preserves the semantic parser API; the located builder
    retains exact element endpoints in its owned effective input. Both use the
    same lexer/token representation and accepted syntax. Import expansion and
    runtime declaration validation belong to later stages. *)

%{
open Core
open Chatmd_ast

let string_of_tag : tag -> string = function
  | Msg -> "msg"
  | User -> "user"
  | Assistant -> "assistant"
  | Agent -> "agent"
  | System -> "system"
  | Developer -> "developer"
  | Script -> "script"
  | Doc -> "doc"
  | Import -> "import"
  | Img -> "img"
  | Config -> "config"
  | Reasoning -> "reasoning"
  | Summary -> "summary"
  | Tool -> "tool"
  | Tool_call -> "tool_call"
  | Tool_response -> "tool_response"
  | Shell_access -> "shell_access"
  | Moderator_runtime -> "moderator_runtime"
  | Uses -> "uses"
  | Authoring_context -> "authoring_context"
  | Authoring_help -> "authoring_help"
  | Shell_element element -> Chatmd_shell_spec.Shell_element.to_string element

let tag_mismatch ~(open_tag : tag) ~(close_tag : tag) =
  failwithf "Mismatching tags: <%s> … </%s>" (string_of_tag open_tag) (string_of_tag close_tag) ()
%}

%token <(Chatmd_ast.tag * Chatmd_ast.attribute list)> START
%token <(Chatmd_ast.tag * Chatmd_ast.attribute list)> SELF
%token <Chatmd_ast.tag> END
%token <Chatmd_parser_builder.Text_fragment.t> TEXT
%token EOF

%parameter <Builder : Chatmd_parser_builder.S>

%start <Builder.node list> document

%%

(* Accumulate lists and text fragments in reverse to avoid quadratic copies. *)
document:
  | rec_elems EOF { List.rev $1 }

rec_elems:
    /* empty */                 { [] }
  | rec_elems whitespace        { $1 }
  | rec_elems rec_elem          { $2 :: $1 }

(*--------------------------------------------------------------------*)
(*  Whitespace helper – TEXT tokens that contain *only* whitespace     *)
(*--------------------------------------------------------------------*)

whitespace:
  | TEXT {
      let text = Chatmd_parser_builder.Text_fragment.text $1 in
      if String.for_all ~f:Char.is_whitespace text then ()
      else failwithf "Unexpected text at top level: %S" text ()
    }

rec_elem:
  | SELF                  { let (t,attrs) = $1 in Builder.element t attrs ~children:[] ~start_byte:($startpos.pos_cnum) ~end_byte:($endpos.pos_cnum) }
  | START children END    {
        let (t_open, attrs) = $1 in
        let t_close = $3 in
        if tag_equal t_open t_close then Builder.element t_open attrs ~children:(List.rev $2) ~start_byte:($startpos($1).pos_cnum) ~end_byte:($endpos($3).pos_cnum)
        else tag_mismatch ~open_tag:t_open ~close_tag:t_close }

children:
    /* empty */           { [] }
  | children child        { $2 :: $1 }

(*--------------------------------------------------------------------*)
(* Collapse consecutive TEXT tokens into one node so that             *)
(*    "a < b"     → Text "a < b"                                    *)
(* instead of                                                         *)
(*    Text "a " ; Text "<" ; Text " b"                              *)
(*--------------------------------------------------------------------*)

child:
    text_block            { Builder.text (List.rev $1) }
  | SELF                  { let (t,attrs) = $1 in Builder.element t attrs ~children:[] ~start_byte:($startpos.pos_cnum) ~end_byte:($endpos.pos_cnum) }
  | START children END    {
        let (t_open, attrs) = $1 in
        let t_close = $3 in
        if tag_equal t_open t_close then Builder.element t_open attrs ~children:(List.rev $2) ~start_byte:($startpos($1).pos_cnum) ~end_byte:($endpos($3).pos_cnum)
        else tag_mismatch ~open_tag:t_open ~close_tag:t_close }

(* A [text_block] is one or more consecutive TEXT tokens, concatenated. *)

text_block:
    TEXT                        { [$1] }
  | text_block TEXT             { $2 :: $1 }
