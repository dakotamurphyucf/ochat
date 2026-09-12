(** Installed, readonly documentation queries. Construction loads the checked
    in-memory corpus; queries never invoke tools, providers or filesystem reads. *)
type t

(** Host-produced coverage of one exact response page. These records have no
    model-input decoder or public constructor. Item hashes cover complete emitted
    items, including their text/schema, and indexes refer to the full ordered
    topic across all pages. *)
type reference_part = private
  { index : int
  ; item_sha256 : string
  }

type reference_topic = private
  { topic : Agent_protocol.Authoring_guidance.topic
  ; total_parts : int
  ; parts : reference_part list
  }

type reference_receipt = private
  { query_identity : string
  ; host_identity : string
  ; capability_fingerprint : string
  ; scope : string
  ; surface_id : string
  ; corpus_identity : string
  ; response_sha256 : string
  ; topics : reference_topic list
  }

type response = private
  { json : Jsonaf.t
  ; receipt : reference_receipt option
  }

(** Same strict JSON request/response contract as [query], with separate trusted
    metadata for actual emitted topic items. Search excerpts, rejected queries,
    orientation-only and empty pages produce no read receipt. A topic is complete
    in this receipt only if every part is present on this one page; the final page
    flag alone never implies that earlier pages remain in model context.

    Native tool/signature references get hashes of their complete selected item
    sequences, bound to actual host/capability/surface identities. Corpus topics
    preserve installed/authored source identities. Nothing is persisted or marked
    as delivered to a model by calling this function. The host must bind the
    receipt to the final disclosed output and its real history occurrence. *)
val query_with_receipt
  :  t
  -> host:Authoring_validation.host
  -> capabilities:Tool_capability.t
  -> scope:string
  -> Jsonaf.t
  -> response

(** Exact emitted-response digest check. This neither authorizes an invocation
    nor proves that the response was delivered, persisted or retained in context. *)
val matches_response : reference_receipt -> Jsonaf.t -> bool

(** Export an actual host-produced receipt for trusted persistence, applying the
    protocol metadata bounds. There is deliberately no inverse that turns decoded
    JSON into a newly produced query receipt. *)
val reference_to_protocol
  :  reference_receipt
  -> (Agent_protocol.Authoring_reference.t, Agent_protocol.Error.t) result

(** [secret] is a host-generated unpredictable cursor signing key, never model
    input. The immutable service can be shared across callers. Cursors also bind
    each caller's scope, target host, capability selection and corpus revision.
    Default responses target 12,000 estimated tokens with a configurable 32,000
    ceiling. Counts use ceil(UTF-8 response bytes / 3), explicitly labelled as an
    estimate, not an exact tokenizer, upper bound or provider billing count. *)
val create
  :  ?default_tokens:int
  -> ?max_tokens:int
  -> ?authored_packages:Authoring_corpus.authored_package list
  -> ?authored_max_bytes:int
  -> secret:string
  -> unit
  -> (t, string) result

val fingerprint : t -> string

(** Trusted host access to the same immutable, checked corpus used by queries.
    This does not grant model-facing access without target/policy checks. *)
val installed_corpus : t -> Authoring_corpus.t

(** Use a host's immutable captured corpus when configured; otherwise use the
    service's installed/captured source snapshot. Actual caller hosts can differ
    from the host that originally registered an inherited helper. *)
val corpus_for_host : t -> host:Authoring_validation.host -> Authoring_corpus.t

(** Captured custom packages are host input to [create], never query parameters.
    Select their visibility from actual tool help metadata. Missing dependencies
    fail rather than restoring an omitted package or exposing its private name.
    Custom sources retain authored provenance and cannot redefine installed topics.
    Query prepare adds selected custom roots and their prerequisites; topic/search/
    continuation apply the same scope and configured response budgets. *)
val scoped_corpus
  :  ?host:Authoring_validation.host
  -> t
  -> capabilities:Tool_capability.t
  -> (Authoring_corpus.t, string) result

(** Resolve a task against the host's actual enabled compiler targets. Shared by
    retrieval and automatic guidance; failure cannot enable another surface. *)
val task_surface
  :  Authoring_validation.host
  -> Chatmd_shell_spec.Authoring_metadata.task
  -> (string, string) result

val parameters : Jsonaf.t

(** Current metadata for [reference.signatures] and [reference.tools], derived
    from the exact query item builders and complete ordered topic hashes. The
    actual host must enable [task]; tool schemas use only [capabilities]. This
    produces no query receipt and does not imply that any content was read. *)
val virtual_topics
  :  t
  -> host:Authoring_validation.host
  -> capabilities:Tool_capability.t
  -> task:Chatmd_shell_spec.Authoring_metadata.task
  -> (Agent_protocol.Authoring_guidance.topic list, string) result

(** The host supplies the actual calling session/generation scope and narrowed
    capabilities. Task selection cannot enable an unavailable host target.
    Search returns ranked topic metadata/excerpts. Topic/prepare assemble stable
    prerequisite closures; continue resumes signed query positions. Whole reference
    sections, including code fences, remain atomic; insufficient budgets explicitly
    report the next minimum. The topic structure remains flat.

    Prepare starts with a flat orientation describing each feature's purpose,
    useful scenarios and direct reference roots. It distinguishes selected tools
    and enabled authoring targets from reference compatibility and runtime grants.

    [complete] concerns pagination of this query, not full feature coverage.
    Prepared packages remain labelled foundation-only until A01's full coverage
    audit is completed. Prepare includes exact selected tool schemas and the
    actual target compiler's readable signatures. Direct topic IDs
    [reference.tools] and [reference.signatures] retrieve these inventories with
    the same scope/budget/continuation checks. Read all pages for alias definitions;
    the signature legend distinguishes reference notation from ChatML source.
    Authoring tool descriptions use the same metadata-derived presentation as
    model requests: callable helper pointers reflect this query's selected
    capabilities, without changing binding identities or schemas. *)
val query
  :  t
  -> host:Authoring_validation.host
  -> capabilities:Tool_capability.t
  -> scope:string
  -> Jsonaf.t
  -> Jsonaf.t
