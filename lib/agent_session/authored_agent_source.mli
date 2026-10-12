open Core

(** Identity of an authored specialist inside an already captured parent revision.
    Source identity is not execution authority: private resource binding, current
    parent policy, instance ownership and revocation must be admitted separately. *)
module Identity : sig
  type t = private
    { parent_revision_id : Agent_protocol.Id.Prompt_revision.t
    ; parent_manifest_sha256 : string
    ; tool_name : string
    ; root_relative_path : string
    ; policy : Prompt.Chat_markdown.agent_persistence
    }
  [@@deriving equal, sexp_of]
end

type t

(** Select a persistence-enabled tool from the host's immutable parsed revision.
    Resolves only exact local agent paths already present in that revision's
    captured source map. Never fetches a URL, reads the live source directory, or
    accepts a caller-provided replacement definition. Missing, ambiguous, ordinary
    one-off and uncaptured external declarations reject before artifact creation.
    The supplied revision must come from the host's verified artifact admission.

    Owned parser revisions require the exact emitted persistence-enabled tool's
    parser-owned queued key and an existing captured member. The association is
    to that actual declaration and persistence policy, independently of its
    materialized runtime path. Missing, external or divergent owned metadata
    rejects without native path guessing. Revisions without parser custody keep
    the legacy confined materialized-path compatibility behavior.

    Retains the entire captured parent source closure, preserving relative import,
    script/schema and nested-agent edges without rewriting authored declarations.
    This conservative closure also pins unused captured sources; a new parent
    revision requires new admission even if this specialist's bytes are unchanged.
    Content-bearing parents require explicit content limits. The complete parent
    content inventory/unknowns is retained, while the actual child Root is replayed
    and materialized without original document/catalog reads. The resulting actual
    artifact-directory parser/projection is retained for [resource_revision]; that
    transfer verifies the child's complete authored tree and does not restart the
    content work allowance or re-materialize. Independently restored child artifacts
    still replay against their own installed directory under explicit host policy. *)
val capture
  :  ?checkpoint:(unit -> unit)
  -> ?content_limits:Chatmd_prompt_content.Limits.t
  -> parent:Prompt_revision.t
  -> tool_name:string
  -> unit
  -> (t, Agent_protocol.Error.t) result

val identity : t -> Identity.t
val fingerprint : t -> string

(** The exact immutable parent supplied to [capture], retained without source
    copying. Its captured parser/artifact data is not live source authority:
    fresh acquisition still requires the host's admitted original-source loan.
    This constant-time accessor performs no reads, callbacks or admission. *)
val defining_revision : t -> Prompt_revision.t

(** The exact parser-emitted persistence-enabled reference already admitted by
    [capture], physically associated with [defining_revision]'s Parsed and actual
    declaration/policy. [None] means the defining revision genuinely has no owned
    parser custody. Owned missing/foreign references reject during capture; they
    never become [None] or a path-based fallback. Retaining this immutable object
    does not grant filesystem, native execution or delegation authority. *)
val defining_reference : t -> Prompt.Chat_markdown.Agent_reference.t option

(** Descriptor with a portable root-relative agent path. Runtime resource
    construction must use the child's materialized revision, not this path as a
    process-relative file lookup. *)
val declaration : t -> Prompt.Chat_markdown.agent_tool

(** Pure artifact construction using only retained bytes. The specialist becomes
    the root; its original source directory and full closure remain intact. Uses
    ordinary authored parser/runtime versions, not generated-only inherited-tool
    syntax. Does not install, parse/evaluate, prepare tools, start a session or
    grant authority. Installation must be coupled to a durable authored reservation.
    Replacing the instance IDs/time preserves source identity. *)
val artifact
  :  t
  -> revision_id:Agent_protocol.Id.Prompt_revision.t
  -> created_at:Agent_protocol.Timestamp.t
  -> (Agent_store.Prompt_artifact_store.Artifact.t, Agent_store.Store_error.t) result

(** Re-root and reparse this captured specialist in the defining parent's verified
    tree, using the shared authored parser and retained source bytes. The returned
    revision has a deterministic source-derived preparation identity; it is not
    installed and must not be published as a persisted child revision. No script
    initializer or tool runs. [parent] must be the original source owner, even if
    another caller later inherits the wrapper. Retains the original catalog policy
    and directory relationships for host-authorized private resource preparation. *)
val resource_revision
  :  ?checkpoint:(unit -> unit)
  -> parent:Prompt_revision.t
  -> t
  -> (Prompt_revision.t, Agent_protocol.Error.t) result

(** Install only the captured source bound to a live durable authored reservation.
    Rebuilds artifact identity from the original reservation for uncertain retries;
    checks the source/declaration, manifest and exact private pins
    before writing. Concurrent replay accepts only the same verified tree. Advances
    [Artifact_installed] only after verification and a fresh revocation check.

    The host must admit the supplied private resource pins and reserve before
    calling. Source identity retains the original defining revision, even when
    another session inherits the wrapper; admission of that caller's own revision
    belongs to the common delegation authority service. This function neither
    constructs resources nor authorizes execution,
    initializes scripts, creates a session or publishes a child. *)
val install_reserved
  :  delegations:Agent_store.Delegation_store.t
  -> reservation:Agent_store.Delegation_store.record
  -> artifact_store:Agent_store.Prompt_artifact_store.t
  -> capability_pins:(string * string) list
  -> t
  -> (Agent_store.Delegation_store.record, Agent_protocol.Error.t) result

(** Verify the admitted manifest, complete materialized tree and ordinary authored
    parser/runtime contract. [reservation] must come from the host-owned ledger.
    Generated-origin records reject. Allows retained inspection after revocation;
    does not re-admit execution or require the parent to be available. *)
val load_artifact
  :  artifact_store:Agent_store.Prompt_artifact_store.t
  -> reservation:Agent_store.Delegation_store.record
  -> (Agent_store.Prompt_artifact_store.Artifact.t, Agent_protocol.Error.t) result

(** Inspect the exact artifact recorded by a host-owned authored admission.
    Store.load admits its complete manifest and verifies its materialized tree.
    Require the recorded revision/manifest, authored origin, supported ordinary
    parser/runtime versions, and either complete legacy captured metadata or
    complete current publisher metadata. Mixed metadata rejects.

    This does not authenticate original wrapper/private resources, admit execution,
    construct resources, run hooks or acquire source authority. The runtime must
    subsequently authorize its genuine original resources and selected code.
    Revoked records remain inspectable. The caller validates the session and prompt
    revision correspondence before using the artifact as a state source. The
    legacy [load_artifact] contract remains unchanged. *)
val load_recorded_artifact
  :  artifact_store:Agent_store.Prompt_artifact_store.t
  -> reservation:Agent_store.Delegation_store.record
  -> (Agent_store.Prompt_artifact_store.Artifact.t, Agent_protocol.Error.t) Result.t

(** Exact retained authored closure with this child's selected root. *)
val authored_sources : t -> Chatmd_source_bundle.t

(** Adopt a complete child projection before fingerprint/reservation/publication.
    Preserve previous program/input/source inventory and unknowns, require the
    actual retained queued Root, and validate active bindings through replay. *)
val with_programs
  :  ?checkpoint:(unit -> unit)
  -> t
  -> Chatml_prompt_programs.t
  -> (t, Agent_protocol.Error.t) Result.t
