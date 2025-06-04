open Core

(** Tiny DOM / AST limited to the chatmd language.  Only the tags that the
    application supports are represented explicitly – everything else is
    treated as raw text later in the lexer. *)

(*--------------------------------------------------------------------------*)
(* Tag enumeration                                                          *)
(*--------------------------------------------------------------------------*)

type tag =
  | Msg
  | User
  | Agent
  | Assistant
  | System
  | Developer
  | Doc
  | Img
  | Config
  | Reasoning
  | Summary
  | Tool_call
  | Tool_response
  | Tool
[@@deriving sexp]

let tag_equal (a : tag) (b : tag) : bool =
  match a, b with
  | Msg, Msg
  | User, User
  | Assistant, Assistant
  | Agent, Agent
  | System, System
  | Developer, Developer
  | Doc, Doc
  | Img, Img
  | Reasoning, Reasoning
  | Summary, Summary
  | Tool_call, Tool_call
  | Tool_response, Tool_response
  | Tool, Tool
  | Config, Config -> true
  | _ -> false
;;

let tag_of_string_opt : string -> tag option = function
  | "msg" -> Some Msg
  | "user" -> Some User
  | "agent" -> Some Agent
  | "assistant" -> Some Assistant
  | "system" -> Some System
  | "developer" -> Some Developer
  | "doc" -> Some Doc
  | "img" -> Some Img
  | "config" -> Some Config
  | "reasoning" -> Some Reasoning
  | "summary" -> Some Summary
  | "tool_call" -> Some Tool_call
  | "tool_response" -> Some Tool_response
  | "tool" -> Some Tool
  | _ -> None
;;

let tag_of_string s =
  match tag_of_string_opt s with
  | Some t -> t
  | None -> invalid_arg (Printf.sprintf "Unknown chatmd tag <%s>" s)
;;

(*--------------------------------------------------------------------------*)
(* DOM                                                                     *)
(*--------------------------------------------------------------------------*)

type attribute = string * string option [@@deriving sexp]

(* A very small DOM – either an element with attributes & children, or raw
   text.  Unknown XML fragments that appear inside a recognised element are
   turned into a single [Text] node by the lexer. *)

type node =
  | Element of tag * attribute list * node list
  | Text of string
[@@deriving sexp]

type document = node list
