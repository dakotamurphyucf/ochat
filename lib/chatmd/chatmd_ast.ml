open Core

(** Chatmd_ast — typed representation of *ChatMarkdown* documents.

    The module defines a **minimal DOM**: only the *official* ChatMarkdown
    elements are represented as distinct constructors; *all other* markup is
    folded into plain [`Text`] nodes by {!Chatmd_lexer}.  The goal is to keep
    the structure lightweight while still allowing programmatic inspection of
    the parts that matter to the application (e.g. differentiating a user
    message from a tool call).

    {1 Design choices}

    • **No full XML fidelity** – we purposefully ignore namespaces, comments
      and processing instructions.  If such constructs appear inside a known
      element they will be preserved verbatim in a [`Text`] node so that no
      information is lost.

    • **No position tracking** – the AST stores only semantic information.
      When error reporting is required the lexer and parser embed the location
      in their error messages instead.

    • **S-expression converters** – all public types derive [`sexp_of`] and
      [`t_of_sexp`] using `[@@deriving sexp]`, simplifying debugging and unit
      testing.

    {1 Quick example}

    {[
      open Chatmd_ast

      let doc : document =
        [ Element
            ( Msg, [ "role", Some "user" ],
              [ Text "Hello" ] ) ]

      let () =
        Sexp.pp_hum Format.std_formatter (sexp_of_document doc)
    ]}
 *)

(** Enumeration of ChatMarkdown element names.

    The set is **closed** – any unknown tag encountered by the lexer will be
    treated as raw text.  Values follow the exact lower-case strings produced
    by the language (e.g. [`Msg`] corresponds to `<msg>`). *)
type tag =
  | Msg
  | User
  | Agent
  | Assistant
  | System
  | Developer
  | Doc
  | Img
  | Import
  | Config
  | Reasoning
  | Summary
  | Tool_call
  | Tool_response
  | Tool
[@@deriving sexp]

(** [tag_equal a b] returns [true] iff tags [a] and [b] are identical.

    This is a specialised equality that avoids relying on structural
    comparison so that the implementation can remain stable should we ever
    attach payloads to the variant constructors. *)
let tag_equal (a : tag) (b : tag) : bool =
  match a, b with
  | Msg, Msg
  | User, User
  | Assistant, Assistant
  | Agent, Agent
  | System, System
  | Developer, Developer
  | Doc, Doc
  | Import, Import
  | Img, Img
  | Reasoning, Reasoning
  | Summary, Summary
  | Tool_call, Tool_call
  | Tool_response, Tool_response
  | Tool, Tool
  | Config, Config -> true
  | _ -> false
;;

(** [tag_of_string_opt s] converts the lower-case tag name [s] to the
    corresponding {!type:tag}.  Returns [Some _] when [s] is recognised, or
    [None] otherwise.  The function expects the exact canonical spelling – no
    normalisation other than case‐sensitivity is performed. *)
let tag_of_string_opt : string -> tag option = function
  | "msg" -> Some Msg
  | "user" -> Some User
  | "agent" -> Some Agent
  | "assistant" -> Some Assistant
  | "system" -> Some System
  | "developer" -> Some Developer
  | "doc" -> Some Doc
  | "import" -> Some Import
  | "img" -> Some Img
  | "config" -> Some Config
  | "reasoning" -> Some Reasoning
  | "summary" -> Some Summary
  | "tool_call" -> Some Tool_call
  | "tool_response" -> Some Tool_response
  | "tool" -> Some Tool
  | _ -> None
;;

(** [tag_of_string s] behaves like {!tag_of_string_opt} but raises
    [Invalid_argument] instead of returning [None] when the input is
    unrecognised.

    @raise Invalid_argument if [s] is not a valid ChatMarkdown tag name. *)
let tag_of_string s =
  match tag_of_string_opt s with
  | Some t -> t
  | None -> invalid_arg (Printf.sprintf "Unknown chatmd tag <%s>" s)
;;

(** XML‐style attribute represented as [(name, value)].

    • [name] – raw attribute key, *case‐sensitive*.
    • [value] – [Some v] when a value is supplied (quotes are stripped and
      entities are decoded by the lexer) or [None] for bare attributes such
      as [disabled]. *)
type attribute = string * string option [@@deriving sexp]

(** Polymorphic tree representation.

    • [Element (tag, attrs, children)] – a recognised ChatMarkdown element.
      Unknown nested markup is already collapsed into a single [`Text`]
      inside [children].

    • [Text s] – raw character data as it appeared in the source, with HTML
      entities *decoded* by the lexer.  New-lines and insignificant spaces
      are preserved because layout can carry semantic meaning in LLM prompts.
*)
type node =
  | Element of tag * attribute list * node list
  | Text of string
[@@deriving sexp]

(** A complete ChatMarkdown document – it is simply a list of top‐level
    elements.  Pure whitespace between elements is discarded by the parser
    and therefore never appears in the AST. *)
type document = node list
