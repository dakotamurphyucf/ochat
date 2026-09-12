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

type topic = private
  { specification : specification
  ; fragments : fragment list
  ; sha256 : string
  }

type t

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

  (** Maintained reviewed subset. Other compiler APIs and semantic inventories
      still require coverage; this is not a complete public-feature manifest. *)
  val reviewed_mappings : mapping list
end

(** Seven source-pinned topics from the checked OCaml-differences guide,
    including the existing chatml.syntax.calls/chatml.types/chatml.tasks IDs.
    The broad [chatml.programs] topic adds the checked program-writing guide:
    source/operators, control flow, matching/types, data utilities and effects.
    Shared prerequisite context identifies the examples as one-off candidates.
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
    boundaries. This remains a foundation, not complete feature/native-schema
    coverage or complete task packages. The separate Authoring_context service
    adds retrieval and selected compiler/tool inventories. *)
val runtime_foundation : sources:Authoring_sources.t -> (t, string) result
