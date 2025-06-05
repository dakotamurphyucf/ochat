(*------------------------------------------------------------------------
  chatmd_parser.mly

  Menhir grammar for ChatMarkdown.  Recognises only the official set of
  chatmd tags.  Any unknown XML that appears inside one of those tags has
  already been turned into a TEXT token by the lexer and is accepted by
  the [children] rule below.
------------------------------------------------------------------------*)

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
  | Doc -> "doc"
  | Import -> "import"
  | Img -> "img"
  | Config -> "config"
  | Reasoning -> "reasoning"
  | Summary -> "summary"
  | Tool -> "tool"
  | Tool_call -> "tool_call"
  | Tool_response -> "tool_response"

let tag_mismatch ~(open_tag : tag) ~(close_tag : tag) =
  failwithf "Mismatching tags: <%s> … </%s>" (string_of_tag open_tag) (string_of_tag close_tag) ()
%}

%token <(Chatmd_ast.tag * Chatmd_ast.attribute list)> START
%token <(Chatmd_ast.tag * Chatmd_ast.attribute list)> SELF
%token <Chatmd_ast.tag> END
%token <string> TEXT
%token EOF

%start <Chatmd_ast.document> document

%%

document:
  | rec_elems EOF { $1 }

rec_elems:
    /* empty */                 { [] }
  | rec_elems whitespace        { $1 }
  | rec_elems rec_elem          { $1 @ [$2] }

(*--------------------------------------------------------------------*)
(*  Whitespace helper – TEXT tokens that contain *only* whitespace     *)
(*--------------------------------------------------------------------*)

whitespace:
  | TEXT {
      if String.for_all ~f:Char.is_whitespace $1 then ()
      else failwithf "Unexpected text at top level: %S" $1 ()
    }

rec_elem:
  | SELF                  { let (t,attrs) = $1 in Element (t, attrs, []) }
  | START children END    {
        let (t_open, attrs) = $1 in
        let t_close = $3 in
        if tag_equal t_open t_close then Element (t_open, attrs, $2)
        else tag_mismatch ~open_tag:t_open ~close_tag:t_close }

children:
    /* empty */           { [] }
  | children child        { $1 @ [$2] }

(*--------------------------------------------------------------------*)
(* Collapse consecutive TEXT tokens into one node so that             *)
(*    "a < b"     → Text "a < b"                                    *)
(* instead of                                                         *)
(*    Text "a " ; Text "<" ; Text " b"                              *)
(*--------------------------------------------------------------------*)

child:
    text_block            { Text $1 }
  | SELF                  { let (t,attrs) = $1 in Element (t, attrs, []) }
  | START children END    {
        let (t_open, attrs) = $1 in
        let t_close = $3 in
        if tag_equal t_open t_close then Element (t_open, attrs, $2)
        else tag_mismatch ~open_tag:t_open ~close_tag:t_close }

(* A [text_block] is one or more consecutive TEXT tokens, concatenated. *)

text_block:
    TEXT                        { $1 }
  | text_block TEXT             { $1 ^ $2 }

