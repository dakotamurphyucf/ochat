open Core

(** Trusted source manifest for topic assembly. The host must use a reviewed
    installed manifest; caller-supplied source names or review labels do not
    establish official semantics or authority. *)
type excerpt =
  { path : string
  ; heading : string
  ; include_children : bool
  }
[@@deriving sexp]

type review =
  | Pending
  | Audited of
      { excerpt_sha256 : string list
      ; evidence : string list
      }
[@@deriving sexp]

type specification =
  { id : string
  ; title : string
  ; prerequisites : string list
  ; surfaces : string list
  ; excerpts : excerpt list
  ; review : review
  }
[@@deriving sexp]

type fragment = private
  { source : excerpt
  ; document_sha256 : string
  ; sha256 : string
  ; text : string
  }

type origin =
  | Installed
  | Authored of
      { package : string
      ; package_sha256 : string
      }
[@@deriving sexp, equal]

type topic = private
  { specification : specification
  ; fragments : fragment list
  ; sha256 : string
  ; origin : origin
  }

type t

(** Captured author conventions, distinct from official runtime semantics.
    [source_name] labels provenance only; construction never opens it. *)
type authored_topic =
  { id : string
  ; title : string
  ; prerequisites : string list
  ; surfaces : string list
  ; source_name : string
  ; text : string
  }
[@@deriving sexp]

type authored_package =
  { help : Chatmd_shell_spec.Authoring_metadata.help
  ; topics : authored_topic list
  }
[@@deriving sexp]

(** Add captured packages without replacing installed topics or existing packages.
    Every authored ID uses [custom.<package>.]; package roots belong to that
    package. Existing dependency/cycle/surface checks also apply to authored
    topics. Cross-package dependencies are allowed but must remain available
    when scoping. The default aggregate authored text budget is 4 MB; [max_bytes]
    can override it. At most 128 packages/512 total topics fit catalog admission.

    Authored origins and complete package digests are assigned by construction;
    authored input cannot claim an audited compiler contract. Official topic
    hashes remain unchanged. Identity is independent of package insertion order.
    Neither prose nor embedded snippets are executed or implicitly audited. *)
val extend_authored : ?max_bytes:int -> t -> authored_package list -> (t, string) result

val authored_packages : t -> Chatmd_shell_spec.Authoring_metadata.help list

(** Keep installed references and exactly the named authored packages. The host
    must derive this list from actual selected tool metadata. Missing/duplicate
    names and unavailable cross-package dependencies reject, never silently
    restore an omitted package. Selecting none recovers the installed corpus
    identity and cannot retain private authored prose. *)
val scope_authored : t -> packages:string list -> (t, string) result

(** Extract a unique exact ATX heading and its body from maintained Markdown.
    Ignore headings inside backtick/tilde fences; preserve original source bytes.
    With [include_children], stop at the next heading of equal or lesser depth;
    otherwise stop at the next heading. Missing/duplicate headings and unclosed
    fences reject. This is not a general Markdown renderer or HTML parser. *)
val section
  :  text:string
  -> heading:string
  -> include_children:bool
  -> (string, string) result

(** Resolve every excerpt against installed source bytes and validate unique IDs,
    compiler surfaces, dependencies, dependency-surface compatibility and cycles.
    Audited entries pin the ordered excerpt hashes and name review/test evidence;
    changed excerpts reject until reviewed again. Hashes bind specification/review
    metadata, exact fragments and source revision.
    Pending topics remain inspectable; their presence cannot imply audit completion.
    No source or compiler implementation is executed. *)
val create : sources:Authoring_sources.t -> specification list -> (t, string) result

val identity : t -> string
val topics : t -> topic list

(** Unfiltered manifest inspection for trusted tooling. Model-facing lookup must
    also check the actual surface, capabilities and policy before returning text. *)
val topic : t -> id:string -> (topic, string) result

(** Stable prerequisite-first closure, preserving declared dependency/root order
    and deduplicating shared dependencies. The host supplies the actual compiler
    surface. Missing/duplicate roots or unavailable topics fail explicitly; no
    unrelated surface or truncated subset is substituted. This does not yet apply
    capability filtering or token budgets. *)
val assemble : t -> surface_id:string -> roots:string list -> (topic list, string) result

(** All topics still requiring semantic/example review. Empty means only that
    this manifest's entries are audited; it does not prove full feature coverage. *)
val pending : t -> string list

(** Documentation coverage is separate from topic review. The compiler inventory
    detects added/removed bindings; explicit mappings bind a reviewed contract and
    topic version to named behavioral evidence. Evidence references are metadata,
    not a claim that a test ran. Semantic/ChatMD/native-tool feature inventories
    must also be supplied before claiming complete public-feature coverage. *)
module Coverage : sig
  type target =
    { id : string
    ; surface_id : string
    ; contract_sha256 : string
    }
  [@@deriving sexp]

  type mapping =
    { target_id : string
    ; contract_sha256 : string
    ; topic_id : string
    ; topic_closure_sha256 : string
    ; evidence : string list
    }
  [@@deriving sexp]

  type report =
    { mapped : string list
    ; missing : target list
    }
  [@@deriving sexp]

  (** Every module, global, export, type alias and entrypoint on the exact selected
      compiler surfaces. Stable IDs distinguish identical names across surfaces;
      hashes preserve the structural type scheme. Empty/duplicate/unknown surface
      selections fail. No builtin or candidate program is executed. *)
  val compiler_targets
    :  sources:Authoring_sources.t
    -> surface_ids:string list
    -> (target list, string) result

  (** Complete regular-production inventory from the installed compiled parser,
      separately scoped to each requested surface. No documentation coverage is
      inferred from compiler-binding mappings. Lexer, precedence, type inference
      and runtime behavior retain their own semantic review obligations. *)
  val grammar_targets
    :  sources:Authoring_sources.t
    -> surface_ids:string list
    -> (target list, string) result

  (** Literal reviewed production/action contracts and topic-closure pins for
      the four extension surfaces. Completeness means every grammar production
      is accounted for, including structure and rejection branches. It is not
      proof of lexical, precedence, inference or runtime semantic coverage. *)
  val grammar_mappings : mapping list

  type semantic_feature = private
    { id : string
    ; description : string
    ; implementation_paths : string list
    ; topic_id : string
    ; evidence : string list
    }
  [@@deriving sexp]

  (** Reviewed lexical, inference, evaluation and codec taxonomy. Source hashes
      detect implementation drift but do not automatically discover new behavior.
      This inventory is separate from grammar, bindings and host/native features. *)
  val semantic_features : semantic_feature list

  val semantic_targets
    :  sources:Authoring_sources.t
    -> surface_ids:string list
    -> (target list, string) result

  val semantic_mappings : mapping list

  (** Reviewed ChatMD extension, generated-definition and authoring declarations.
      Surface IDs select documentation compatibility, not permission to execute
      every authored declaration in a generated child. The referenced guides
      distinguish those contexts. Legacy shell/MCP details and native operation
      semantics still require their separate feature accounting. *)
  val declaration_features : semantic_feature list

  (** Requires nonempty unique selections from tool_v1, moderator_v1 and
      delegated_moderator_v1. One-off scripts do not declare ChatMD tools. *)
  val declaration_targets
    :  sources:Authoring_sources.t
    -> surface_ids:string list
    -> (target list, string) result

  val declaration_mappings : mapping list

  (** Digest the complete prerequisite-first topic closure for this surface.
      Every topic must be audited; a prerequisite change invalidates the pin
      even if the root topic's excerpts remain unchanged. *)
  val topic_contract
    :  t
    -> surface_id:string
    -> topic_id:string
    -> (string, string) result

  (** Reject duplicate/obsolete mappings, changed contracts/topics, missing topics,
      unaudited or incompatible topics and absent evidence. Unmapped targets are
      reported explicitly, never mapped by a wildcard/module-prefix fallback.
      Callers must supply a trusted exhaustive inventory and reviewed literal
      pins; regenerating mapping pins on every run defeats drift detection. *)
  val audit : t -> targets:target list -> mappings:mapping list -> (report, string) result

  (** Fail unless every supplied target has a validated mapping. This proves only
      coverage of that inventory, not completeness of a caller-selected subset. *)
  val require_complete : report -> (unit, string) result

  (** Initial maintained coverage for the six entrypoint contracts on the four
      extensibility surfaces. Literal pins require review when compiler schemes,
      topic prose or prerequisite topics change. Other APIs remain unmapped;
      this list alone is not the full public-feature coverage manifest. *)
  val entrypoint_mappings : mapping list

  (** Exact reviewed Task module and pure/bind/map/fail/catch contracts on all
      four extensibility surfaces, including checked recovery/error boundaries. *)
  val task_mappings : mapping list

  (** Reviewed String module and all fourteen exports on each extensibility
      surface, with byte offsets, literal matching and immediate error examples. *)
  val string_mappings : mapping list

  (** Reviewed Array module and all twenty-two exports on each extensibility
      surface, including shallow aliases and explicit task sequencing. *)
  val array_mappings : mapping list

  (** Reviewed Option module and its five exports on each extensibility surface,
      including eager defaults and structural variants. *)
  val option_mappings : mapping list

  (** Reviewed Json module and all sixteen exports on each extensibility surface,
      including duplicates, payload aliasing and numeric export boundaries. *)
  val json_mappings : mapping list

  (** Reviewed string-keyed Hashtbl module and its five exports on each surface. *)
  val hashtbl_mappings : mapping list

  (** All ten shared globals, plus print only on the ordinary moderator surface. *)
  val global_mappings : mapping list

  (** Shared recursive json alias, with checked constructor/access/codec examples. *)
  val json_alias_mappings : mapping list

  (** All Item, Context and Tool_call exports plus item/tool_desc/tool_call/
      tool_result/context aliases on ordinary and delegated moderator surfaces.
      Checked examples execute actual moderator entrypoints on both targets. *)
  val moderator_data_mappings : mapping list

  (** Log on all surfaces, Turn on moderators, and each surface's exact Tool
      exports. Evidence covers diagnostic/external versus local rollback and
      the versioned owned-job spawn alias through an actual daemon. *)
  val host_effect_mappings : mapping list

  (** Runtime on both moderators; Model and Process on the ordinary moderator.
      Checked request rollback/phase/event semantics and fake model/process
      callbacks, with audited real recipe and shell adapter contracts. *)
  val runtime_control_mappings : mapping list

  (** Job on all surfaces; Subscription, Schedule, Notification and Ingress on
      moderators; six work/completion/policy aliases on their exact targets.
      Literal pins link complete operation semantics, checked value examples
      and existing daemon/transaction integration tests. *)
  val background_mappings : mapping list

  (** Invocation module and seven context/outcome/event aliases on their exact
      managed surfaces. Evidence includes actual direct/nested daemon admission
      and moderator resolution/rollback tests. *)
  val invocation_context_mappings : mapping list

  (** Reviewed mappings for all compiler bindings on the four extensibility
      surfaces. The normal test gate requires that complete compiler inventory;
      language semantics, ChatMD and native-contract inventories are separate.
      This alone is not a complete public-feature manifest. *)
  val reviewed_mappings : mapping list
end

(** Seven source-pinned topics from the checked OCaml-differences guide,
    including the existing chatml.syntax.calls/chatml.types/chatml.tasks IDs.
    The broad [chatml.programs] topic adds the checked program-writing guide:
    source/operators, control flow, matching/types, data utilities and effects.
    Shared prerequisite context identifies the examples as one-off candidates.
    The flat [chatml.inference] topic adds checked annotation, arity,
    generalization, recursive-value and structural-contract examples.
    This is the compact language foundation, not full language/runtime coverage
    or any of the five complete task packages. *)
val language_foundation : sources:Authoring_sources.t -> (t, string) result

(** Language topics plus seven reviewed invocation topics: execution contracts,
    extension declarations/schemas, one-off/standalone/moderator invocations,
    authority and non-executing validation. Target-specific entrypoint topics
    cannot be assembled for other targets. Five additional child-session topics
    cover captured definitions, creation/authority, receipts, output/cursor recovery
    and stop/helper semantics. Child guidance is reference context on all four
    surfaces, not a grant of the described tools. Eight background topics add
    jobs/recovery on all four surfaces, with moderator-only acknowledgements,
    subscriptions, timers, notifications, ingress and a checked shell coordinator.
    Notification/example prerequisites include acknowledgement and recovery
    boundaries. Flat String, Array/Option, Json, Hashtbl and global-helper topics
    cover the entire shared core API with executable semantic examples and exact
    surface availability. Moderator data inspection adds Item/Context/Tool_call
    and five aliases only on the two moderator targets. The host-effect guide
    covers Log/Turn/Tool with explicit target and host-service distinctions.
    This remains a foundation,
    not complete feature/native-schema
    coverage or complete task packages. The separate Authoring_context service
    adds retrieval and selected compiler/tool inventories. *)
val runtime_foundation : sources:Authoring_sources.t -> (t, string) result
