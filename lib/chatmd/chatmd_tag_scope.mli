open! Core

(** Pure module in [chatmd_source_input], shared by the strict ChatMD
    lexer and the tolerant highlighter. It replaces the lexer's current private
    depth/structured/extension scope bookkeeping without changing which complete
    inputs the language accepts. No parser result, path or authority is stored. *)

module Error : sig
  type t = Depth_limit [@@deriving equal, sexp_of]
end

type t

val root : t
val depth : t -> int
val top : t -> Chatmd_ast.tag option

module Root_hint : sig
  type t =
    | Shell_access
    | Tool of
        { shell : bool
        ; read_file : bool
        ; extension : bool
        }
  [@@deriving sexp_of]
end

(** The nearest completed lexical structured root and the entered tags since
    that root, outermost first. This is placement metadata, not proof that the
    declaration parser accepts that nesting. The caller polls during the
    bounded ancestor traversal. *)
val nearest_structured_root
  :  t
  -> checkpoint:(unit -> unit)
  -> (Root_hint.t * Chatmd_ast.tag list) option

(** Exactly the strict lexer's current official-tag placement rule. Unknown
    spellings and shell/extension tags outside their existing lexical scopes
    remain text. This is lexical recognition, not declaration validation. *)
val recognize : t -> name:string -> Chatmd_ast.tag option

(** A completed opening tag changes scope; self-closing tags do not. [attributes]
    are the existing lexer's decoded complete attributes, used only for its
    current structured/extension-root classification. The caller admits source
    bytes and charges token work; this operation polls a potentially long
    attribute list. A depth failure publishes no new scope. *)
val enter
  :  t
  -> tag:Chatmd_ast.tag
  -> attributes:Chatmd_ast.attribute list
  -> max_depth:int
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) result

(** A completed END token leaves one lexical level, matching the current lexer.
    The Menhir grammar, not this scope record, checks open/close tag equality.
    At root, leaving keeps root so invalid syntax remains the parser's error. *)
val leave : t -> t
