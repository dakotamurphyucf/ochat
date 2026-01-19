(** Typed representation and parser for {e ChatMarkdown} prompts.

    {b ChatMarkdown} is a lightweight XML dialect used in this code-base to
    describe conversations for Large Language Models (LLMs).  The purpose of
    {!module:Chat_markdown} is twofold:

    • expose a zero-cost {e strongly-typed} view of the language so that
      downstream modules can pattern-match on variants instead of inspecting
      raw strings;
    • offer a single helper – {!val:parse_chat_inputs} – to parse, resolve
      imports, and convert a complete document into a list of OCaml records
      ready to be serialised to the OpenAI chat API.

    All public types derive [@@deriving jsonaf] as well as [sexp], [compare],
    [hash] and [bin_io] for seamless debugging and persistence.  Unknown or
    future ChatMarkdown tags are preserved verbatim inside [`Text`]
    placeholders, guaranteeing forward-compatibility. *)

module Chat_markdown : sig
  type function_call =
    { name : string
    ; arguments : string
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type tool_call =
    { id : string
    ; function_ : function_call
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type image_url = { url : string } [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* A single item of content, which can be text or an image. *)
  type basic_content_item =
    { type_ : string [@key "type"]
    ; text : string option [@jsonaf.option]
    ; image_url : image_url option [@jsonaf.option]
    ; document_url : string option [@jsonaf.option]
    ; is_local : bool [@default false]
    ; cleanup_html : bool [@default false]
    ; markdown : bool [@default false] (* whether to convert HTML to Markdown *)
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* Agent content: has a url, is_local, and sub-items. *)
  type agent_content =
    { url : string
    ; is_local : bool
    ; items : content_item list [@default []]
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* content_item can be either a Basic variant or an Agent variant. *)
  and content_item =
    | Basic of basic_content_item
    | Agent of agent_content
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* The overall content can be either a single string or a list of items. *)
  type chat_message_content =
    | Text of string
    | Items of content_item list
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type reasoning_summary =
    { text : string
    ; _type : string (* usually "summary" *)
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type reasoning =
    { summary : reasoning_summary list
    ; id : string
    ; status : string option
    ; _type : string (* always "reasoning" *)
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* A generic <msg role="…"> element.  Still used for legacy messages
     (e.g. roles other than the four specialised shorthands).
     NOTE:  The newer shorthand tags (<user/>, <assistant/>, <tool_call/>,
     <tool_response/>) are mapped to dedicated OCaml record types that are
     aliases of [msg].  This removes the need to inspect the [role] string
     when traversing the parse-tree, while keeping the underlying shape
     identical so existing logic can be reused. *)

  type msg =
    { role : string
    ; type_ : string option [@key "type"] [@jsonaf.option]
    ; content : chat_message_content option [@jsonaf.option]
    ; name : string option [@jsonaf.option]
    ; id : string option [@jsonaf.option]
    ; status : string option [@jsonaf.option]
    ; function_call : function_call option [@jsonaf.option]
    ; tool_call : tool_call option [@jsonaf.option]
    ; tool_call_id : string option [@jsonaf.option]
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* Dedicated message records for the new shorthand tags.  They are simple
     aliases so that the JSON / serialisation helpers generated for [msg]
     can be re-used without code duplication. *)

  type user_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type assistant_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type tool_call_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type tool_response_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type developer_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type system_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type custom_tool =
    { name : string
    ; description : string option
    ; command : string
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type agent_tool =
    { name : string
    ; description : string option
    ; agent : string
    ; is_local : bool
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type mcp_tool =
    { names : string list option
    ; description : string option
    ; mcp_server : string
    ; strict : bool
    ; client_id_env : string option
    ; client_secret_env : string option
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type tool =
    | Builtin of string
    | Custom of custom_tool
    | Agent of agent_tool
    | Mcp of mcp_tool
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type config =
    { max_tokens : int option [@jsonaf.option]
    ; model : string option [@jsonaf.option]
    ; reasoning_effort : string option [@jsonaf.option]
    ; temperature : float option [@jsonaf.option]
    ; show_tool_call : bool
    ; id : string option [@jsonaf.option]
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type top_level_elements =
    | Msg of msg (** Legacy <msg/> element (system, developer…) *)
    | Developer of developer_msg
    | System of system_msg
    | User of user_msg (** <user/> *)
    | Assistant of assistant_msg (** <assistant/> *)
    | Tool_call of tool_call_msg (** <tool_call/> *)
    | Tool_response of tool_response_msg (** <tool_response/> *)
    | Config of config
    | Reasoning of reasoning
    | Tool of tool
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (** [parse_chat_inputs ~dir raw] tokenises, parses and normalises the
      ChatMarkdown snippet contained in [raw].

      The result preserves the original order of logical messages and is
      ready to feed into the OpenAI chat API or any compatible backend.

      Parameters:
      • [dir] – base directory against which relative paths are resolved
        (imports, local images, nested agent prompts).  The directory is a
        standard Eio capability, making the function safe with respect to
        the ambient file-system.
      • [raw] – the UTF-8 text to parse.  It can be a whole document or a
        fragment; leading BOM and surrounding whitespace are ignored.

      Behaviour:
      1. Preprocesses the input via {!Preprocessor.preprocess} to strip
         comments and handle conditional compilation markers.
      2. Parses the cleaned source with the Menhir grammar from
         {!module:Chatmd_parser}.
      3. Expands [`<import>`] directives recursively.
      4. Converts AST nodes into strongly-typed
         {!type:Chat_markdown.top_level_elements} values, mapping shorthand
         tags (`<user>`, `<assistant>` & co.) to dedicated aliases.
      5. Filters out nodes that are irrelevant to the conversation
         (e.g. stray whitespace between top-level blocks).

      @raise Failure  If the source is not valid ChatMarkdown or if an
                      imported resource cannot be read. *)
  val parse_chat_inputs
    :  dir:Eio.Fs.dir_ty Eio.Path.t
    -> string
    -> top_level_elements list
end

(** {1 Metadata helpers}
    Attach key/value metadata to any top-level element without changing
    existing record definitions.  The data lives in an external
    registry so serialisation of prompts is unaffected. *)

module Metadata : sig
  (** [add elt ~key ~value] attaches the metadata pair [(key, value)] to
      [elt].  If the same [key] already exists it is appended 
      (i.e. multiple values per key are allowed).  The call mutates a
      global in-memory table; it has no effect on serialisation. *)
  val add : Chat_markdown.top_level_elements -> key:string -> value:string -> unit

  (** [get elt] returns all key/value metadata associated with [elt] or
      [None] if no entry is present.  The list preserves the insertion
      order, most-recent first.  Mutating the returned list does **not**
      update the registry. *)
  val get : Chat_markdown.top_level_elements -> (string * string) list option

  (** [set elt kvs] replaces the whole metadata list of [elt] with [kvs].
      Use {!val:add} if you only need to add a single pair. *)
  val set : Chat_markdown.top_level_elements -> (string * string) list -> unit

  (** [clear ()] removes **all** stored metadata for **every** element.
      Call it at the end of a request to avoid memory leaks in long-running
      processes. *)
  val clear : unit -> unit
end
