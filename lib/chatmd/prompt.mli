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
    ; ochat_history_id : History_entry.Id.t option
    ; source_context : string option
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
    ; phase : string option [@jsonaf.option]
    ; ochat_history_id : History_entry.Id.t option [@jsonaf.option]
    ; source_context : string option [@jsonaf.option]
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
    ; source : Chatmd_shell_spec.Source_ref.t
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

  (** Persistence-enabled agent declarations keep the legacy [Agent] representation
      for the default/explicit [one_off] case, preserving its serialized identity. *)
  type agent_persistence =
    | Persistent
    | Optional
  [@@deriving jsonaf, sexp, hash, bin_io, compare, equal]

  type tool =
    | Builtin of string
    | Read_file of Chatmd_read_file_spec.t
    | Custom of custom_tool
    | Shell of Chatmd_shell_spec.Shell_tool_spec.t
    | Agent of agent_tool
    | Mcp of mcp_tool
    | Extension of Chatmd_shell_spec.Extension_spec.tool
    | Inherited of string
    | Persistent_agent of agent_tool * agent_persistence
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

  type script_reference =
    { path : string
    ; source_text : string
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type script_source =
    | Inline of string
    | Src of script_reference
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type script =
    { id : string
    ; language : string
    ; kind : string
    ; source : script_source
    ; project_selection : Chatmd_shell_spec.Script_project.t option
          [@jsonaf.option] [@sexp.option]
    ; source_ref : Chatmd_shell_spec.Source_ref.t option [@jsonaf.option]
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
    | Shell_runtime of Chatmd_shell_spec.Shell_spec.t
    | Moderator_runtime of Chatmd_shell_spec.Manifest_compiler.moderator_runtime
    | Script of script
    | Shell_script of Chatmd_shell_spec.Chatmd_script_spec.t
    | Extension_script of Chatmd_shell_spec.Extension_spec.script
    | Authoring_context of Chatmd_shell_spec.Extension_spec.authoring_context
    | Authoring_help of Chatmd_shell_spec.Extension_spec.authoring_help
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  module Document_occurrence : sig
    (** One actual included document content slot, owned by this Parsed result.
        A repeated import creates a distinct slot even for an equal input/range.
        No native directory or authored display source_context is authority. *)
    type t

    module Selection : sig
      type t =
        | Dynamic
        | Local_capture
        | Builtin of string
      [@@deriving equal, sexp_of]
    end

    val range : t -> Chatmd_parser_input.Range.t
    val ordinal : t -> int
    val src : t -> string
    val selection : t -> Selection.t
    val local : t -> bool
    val cleanup_html : t -> bool
    val markdown : t -> bool
  end

  module Declaration : sig
    type t

    val range : t -> Chatmd_parser_input.Range.t
    val source_ref : t -> Chatmd_shell_spec.Source_ref.t
    val script_id : t -> string

    (** Authored selection and external src spelling from this actual converted
        declaration. Neither is a native grant or resolved filesystem path.
        Relative paths use the owning parser input, never display provenance. *)
    val project_selection : t -> Chatmd_shell_spec.Script_project.t option

    val script_path : t -> string option

    (** Body is the actual inline extraction / loaded external body already used
        by the script parser; artifact custody computes/reconciles its digest. *)
    val script_body : t -> string
  end

  module Agent_reference : sig
    (** Actual emitted invocation descriptor. The parser records the distinction;
        host role/source-mode policy remains with the caller. *)
    type invocation =
      | Tool of agent_tool
      | Persistent_tool of agent_tool * agent_persistence
      | Content of agent_content

    type t

    val invocation : t -> invocation

    (** Genuine effective Root/Import tag range. Display source context and
        materialized paths remain separate from this declaration owner. *)
    val range : t -> Chatmd_parser_input.Range.t

    (** Exact key from the same successful relative-local Source_loader
        resolution that notifies the capture queue. Nonlocal and absolute local
        external references have no queued key. This is immutable provenance,
        not native authority; never infer a key from the runtime path string. *)
    val queued_key : t -> string option
  end

  module Parsed : sig
    type t

    val elements : t -> top_level_elements list
    val root_input : t -> Chatmd_parser_input.t
    val inputs : t -> Chatmd_parser_input.t list
    val declarations : t -> Declaration.t list

    (** Only actual emitted top-level agent tools and message content-agent
        invocations, including nested items. Owned conversion resolves against
        each actual declaration source node, preserving existing display context.
        Ignored markup is not an invocation. Legacy AST/wire representations are
        unchanged; ordinary legacy wrappers allocate no reference inventory. *)
    val agent_references : t -> Agent_reference.t list

    (** Only documents actually included in message/Agent content, in emitted
        order; ignored markup does not become an acquisition request. *)
    val document_occurrences : t -> Document_occurrence.t list
  end

  module Document_projection : sig
    (** Literal content projection authenticated against its original Parsed.
        Declaration/input provenance stays in that unchanged parser result. *)
    type t

    val parsed : t -> Parsed.t
    val elements : t -> top_level_elements list

    (** Logical traversal/comparison/list-copy units consumed by materialization. *)
    val work_steps : t -> int
  end

  (** Resolve one actual emitted content-agent descriptor to its original parser
      reference. With [projection], require SAME Parsed and exact descriptor
      association retained by that projection's bounded copy traversal. Without
      projection, require original descriptor identity. No structural/path/body
      matching or native source lookup; foreign/missing descriptors reject.
      [max_work_steps] is a positive bound for this complete lookup, counting each
      association/reference scan and checkpoints before work. Borrowed checkpoint
      exceptions propagate unchanged. *)
  val content_agent_reference
    :  Parsed.t
    -> ?projection:Document_projection.t
    -> max_work_steps:int
    -> checkpoint:(unit -> unit)
    -> agent_content
    -> Agent_reference.t Core.Or_error.t

  (** Replace exact actual Local_capture/Builtin slots with literal Basic text.
      Reject foreign, duplicate, surplus or missing occurrences before projection
      allocation. Dynamic slots remain unchanged. Preserve roles, ordering, IDs,
      display source_context and all other message fields. Nonnegative explicit
      occurrence/output-byte/work ceilings apply before list/string retention.
      Work includes validation, slot comparisons and copying every plain item,
      including items with no document. It is a logical admission model, not a
      CPU or wall-clock bound. No I/O
      or content lookup; borrowed checkpoint exceptions propagate unchanged. *)
  val materialize_documents
    :  Parsed.t
    -> rendered:(Document_occurrence.t * string) list
    -> max_occurrences:int
    -> max_output_bytes:int
    -> max_work_steps:int
    -> checkpoint:(unit -> unit)
    -> Document_projection.t Core.Or_error.t

  module Limits : sig
    type t

    (** Positive explicit bounds for ONE Root preparation/replay INCLUDING its
        imported parsing. Byte cap applies before hashing/index/lexing each input;
        parse-call/token counters do not reset for imports. Depth bounds both
        per-file markup and the expanded output. Capture combines at most 256
        Root calls under the artifact's
        separately enforced joint authored/private256-file/8MiB allowance.
        Suggested capture values reuse established generated-admission ceilings:
        8MiB input,1024 parse calls,100000 tokens,depth128. No default is implicit.
        Ordinary legacy authored wrapper retains its current acceptance policy. *)
    val create
      :  max_input_bytes:int
      -> max_parse_calls:int
      -> max_tokens:int
      -> max_depth:int
      -> t Core.Or_error.t

    val input_bytes : t -> int
  end

  (** Explicit parser-owned preparation callback. Ordinary production wrapper
      supplies Meta_prompting.Preprocessor.preprocess. Capture tests supply a
      deterministic transformation and count invocations. Called ONCE for this
      Root, never for raw imports. Cancellation/callback exceptions propagate
      unchanged. Root + each queued local-agent Root have separate calls. *)
  val prepare_chat_inputs
    :  source:string
    -> source_loader:Source_loader.t
    -> dir:Eio.Fs.dir_ty Eio.Path.t
    -> preprocess:(string -> string)
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> string
    -> Parsed.t

  (** Parse an explicitly imported transcript with the same owned grammar and
      limits. No preprocessing, imports, scripts, tools, configuration, documents
      or embedded agents are admitted. Only literal message/call/result/reasoning
      fragments and nonlocal image references are accepted. Validation of actual
      syntax precedes expansion or declaration/resource loading. Nonwhitespace
      top-level text (including unsupported opaque export markup) rejects rather
      than disappearing. This is authored interpretation, not provider replay.
      Original checkpoint/cancellation exceptions propagate; syntax or admission
      errors raise [Failure]. No source reads or runtime initialization. *)
  val parse_transcript_inputs
    :  dir:Eio.Fs.dir_ty Eio.Path.t
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> string
    -> top_level_elements list

  (** Replay an actual retained Root input through ordinary authored declaration
      semantics with preprocessing Fn.id. This DOES NOT use generated-bundle
      validate_inert: effective input may legitimately retain a META_REFINE marker.
      Imports resolve/read solely through the supplied captured loader and become
      Import inputs with exact raw=effective equality. Never consult a preprocessor,
      provider, native source fallback or an ambient enablement flag. Missing Root
      association is rejected by the artifact owner before this call. *)
  val replay_chat_inputs
    :  Chatmd_parser_input.t
    -> source_loader:Source_loader.t
    -> dir:Eio.Fs.dir_ty Eio.Path.t
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> Parsed.t

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
      1. Applies optional meta-refinement via {!Preprocessor.preprocess}.
         Use [parse_source_bundle] for generated source that must not execute
         preprocessing.
      2. Parses the cleaned source with the Menhir grammar from
         {!module:Chatmd_parser}.
      3. Expands [`<import>`] directives recursively.
      4. Converts AST nodes into strongly-typed
         {!type:Chat_markdown.top_level_elements} values, mapping shorthand
         tags (`<user>`, `<assistant>` & co.) to dedicated aliases.
      5. Resolves and validates top-level [`<script>`] declarations.
      6. Filters out nodes that are irrelevant to the conversation
         (e.g. stray whitespace between top-level blocks).

      @raise Failure  If the source is not valid ChatMarkdown or if an
                      imported resource cannot be read. *)
  val parse_chat_inputs
    :  ?source:string
    -> ?source_loader:Source_loader.t
    -> dir:Eio.Fs.dir_ty Eio.Path.t
    -> string
    -> top_level_elements list

  (** Authored-parser semantics with executable preprocessing disabled. The
      required loader controls dependency reads; this is not the bounded,
      canonical generated-bundle admission path. Used to check captured legacy
      artifact closures without starting refinement. *)
  val parse_chat_inputs_without_preprocessing
    :  ?source:string
    -> source_loader:Source_loader.t
    -> dir:Eio.Fs.dir_ty Eio.Path.t
    -> string
    -> top_level_elements list

  type parsed_bundle =
    { root : top_level_elements list
    ; agents : (string * top_level_elements list) list
    ; parsed_root : Parsed.t
    ; parsed_agents : (string * Parsed.t) list
    }

  (** Recheck the parsed declaration registry, including IDs, handler kinds,
      dependency cycles and authoring declaration uniqueness. No source reads,
      preprocessing, compilation or evaluation. Returns the same elements;
      raises [Failure] on invalid declarations. Source/hash/schema and runtime
      capability checks remain the admission service's responsibility. *)
  val validate_declarations : top_level_elements list -> top_level_elements list

  (** Parse the root and the complete reachable local-agent source closure from
      supplied bytes only. Imports and script/schema sources cannot fall back to
      disk; external/missing agent definitions reject. No preprocessing runs,
      including when ambient meta-refinement is enabled; explicit markers fail.
      Cumulative reads, bytes, tokens and markup depth are bounded. Generated
      provenance uses canonical root-relative names. [dir] anchors provenance;
      no files are read from it or materialized into it by this function.
      This is parsing, not execution/authority admission. Native tool/root/MCP
      configuration, message resource access and handler compilation still need
      validation against inherited authority before any runtime consumes results. *)
  val parse_source_bundle
    :  dir:Eio.Fs.dir_ty Eio.Path.t
    -> Chatmd_source_bundle.t
    -> parsed_bundle
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
