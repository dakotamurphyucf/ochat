open Core

(** Readonly validation of inline script candidates. This service only selects
    existing capability bindings and invokes the bounded static compiler. It does
    not construct/evaluate a runtime, read source files, invoke tools, or create
    sessions. Generated ChatMD bundles use the same captured-source admission as
    creation, without materializing an artifact or instantiating a runtime. *)
type target =
  | One_off_script
  | Standalone_tool
  | Moderator
  | Generated_chatmd
[@@deriving sexp, equal]

type moderator_surface =
  | Ordinary
  | Delegated
[@@deriving sexp, equal]

type host

type context_budget = private
  { default_tokens : int
  ; max_tokens : int
  ; preload_tokens : int
  }
[@@deriving compare, equal, sexp]

(** Host-selected estimates for reference responses and automatic context.
    Values must be positive, at most one million, with default <= maximum.
    These do not change execution limits or grant any capability. *)
val context_budget
  :  default_tokens:int
  -> max_tokens:int
  -> preload_tokens:int
  -> (context_budget, string) result

val configure_context_budget : host -> context_budget -> (host, string) result
val configured_context_budget : host -> context_budget option
val default_context_budget : context_budget

(** The owning host supplies the installed runtime identity, available targets,
    actual moderator surface and compiler ceilings. None of these are accepted
    from submitted JSON. Identity must change with the target runtime build or
    execution contract. Guidance retrieval never changes this host or its tools. *)
val create_host
  :  runtime_identity:string
  -> targets:target list
  -> moderator_surface:moderator_surface
  -> compilation:Chatml_compilation.limits
  -> (host, string) result

(** Binds helper registration/cache identity to every supported target contract,
    the installed runtime identity and compiler policy. Changes require a fresh
    native capability binding as well as fresh validation. *)
val host_fingerprint : host -> string

(** Narrow moderator validation to the generated execution surface, preserving
    targets/runtime/limits and selecting the captured delegated catalog when
    present. Uncaptured, explicitly supplied catalogs remain host-owned metadata. *)
val for_delegated : host -> host

(** Capture custom conventions in an immutable host snapshot, with separate
    ordinary/delegated catalogs derived from actual supported surfaces. This
    replaces previous captured packages/catalog metadata; source names are labels,
    never paths to open. Source/package revisions participate in host identity. *)
val configure_authored
  :  ?max_bytes:int
  -> host
  -> packages:Authoring_corpus.authored_package list
  -> (host, string) result

val corpus : host -> Authoring_corpus.t option
val delegated_catalog : host -> Authoring_policy.catalog option

(** Derive package/topic compatibility and complete authored dependency ownership
    for this host. Unavailable packages are excluded rather than relabelled as
    compatible; policy admission then rejects a requested unavailable dependency. *)
val catalog_of_corpus
  :  host
  -> Authoring_corpus.t
  -> (Authoring_policy.catalog, string) result

(** Configure host-owned source limits and compatible catalog metadata. These are
    never read from candidate JSON. Omitted configuration uses default bundle
    limits and no catalog; authoring auto/preload still requires authentic helpers.
    A captured corpus's catalogs cannot be replaced independently of its source
    packages; use [configure_authored] to replace that snapshot. *)
val configure_generated
  :  host
  -> limits:Chatmd_source_bundle.limits
  -> catalog:Authoring_policy.catalog option
  -> (host, string) result

(** Shared admission policy for execution hosts. Creation/restoration still run
    their own validators and authority checks; a report does not grant admission. *)
val compilation_limits : host -> Chatml_compilation.limits

val bundle_limits : host -> Chatmd_source_bundle.limits
val catalog : host -> Authoring_policy.catalog option

(** Trusted target metadata shared with readonly documentation retrieval. Model
    requests choose an authoring task, never these host identities or surfaces. *)
val runtime_identity : host -> string

val targets : host -> target list
val moderator_surface : host -> moderator_surface

(** Record a known execution limitation without removing readonly compiler
    validation targets. This affects host identity and authoring preparation;
    it never changes the execution service's own admission checks. Hosts without
    this restriction still require actual service/capability checks at execution. *)
val without_persisted_children : host -> host

val execution_unavailable_reason
  :  host
  -> Chatmd_shell_spec.Authoring_metadata.task
  -> string option

(** Shared task-to-compiler-surface resolution for retrieval, materialization and
    tool discovery. Unavailable host targets fail; a task request cannot enable
    another compiler surface. *)
val task_surface
  :  host
  -> Chatmd_shell_spec.Authoring_metadata.task
  -> (string, string) result

type diagnostic =
  { diagnostic : Chatmd_shell_spec.Diagnostic.t
  ; topic_ids : string list
  }
[@@deriving sexp]

type report = private
  { target : target option
  ; source : Chatmd_shell_spec.Source_ref.t option
  ; validation_id : string option
  ; compiler_contract : string option
  ; capability_fingerprint : string option
  ; runtime_identity : string
  ; diagnostics : diagnostic list
  ; checked : string list
  ; deferred : string list
  }
[@@deriving sexp]

val parameters : Jsonaf.t

(** Request version 1 accepts target, source and exact selected tool names.
    Standalone candidates additionally require input_schema/output_schema.
    Unknown/duplicate fields, unavailable targets, oversized sources and invalid
    schemas fail before compilation. Every compile includes static entrypoint
    checks. Successful validation explicitly defers initializer, dynamic tool,
    state and runtime schema/permission checks.

    Generated_chatmd instead accepts root_file and sources (path/text objects).
    It checks bounded captured imports, inherited declarations, authoring policy
    and delegated moderator compilation. It defers session creation, live parent
    moderation and lifecycle/revocation. Its identity covers all captured sources,
    bundle limits and the installed catalog. The source reference describes the
    root file. No artifact is materialized; all source loading is bundle-only.

    [capabilities] must be the caller/target's actual effective ceiling. A report
    and its identity grant no authority, even after successful validation. Run
    tools must independently re-admit current source and capabilities. Identities
    bind source, target, schemas, selected bindings, host surface, runtime build
    and compiler policy. Caller cancellation propagates through joined compiler
    cleanup. Responses include hashes/spans, never the full source or registry. *)
val validate
  :  env:Eio_unix.Stdenv.base
  -> host:host
  -> capabilities:Tool_capability.t
  -> Jsonaf.t
  -> report

val valid : report -> bool
val to_json : report -> Jsonaf.t

(** Stable topic dependencies for A01's shared corpus/coverage manifest. These
    are metadata, not a substitute for reference content or context insertion. *)
val topics : (string * string) list

val help : target -> Chatmd_shell_spec.Authoring_metadata.help
val helper_metadata : Chatmd_shell_spec.Authoring_metadata.t
