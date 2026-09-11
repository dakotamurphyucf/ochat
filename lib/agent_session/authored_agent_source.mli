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

    Retains the entire captured parent source closure, preserving relative import,
    script/schema and nested-agent edges without rewriting authored declarations.
    This conservative closure also pins unused captured sources; a new parent
    revision requires new admission even if this specialist's bytes are unchanged. *)
val capture
  :  parent:Prompt_revision.t
  -> tool_name:string
  -> (t, Agent_protocol.Error.t) result

val identity : t -> Identity.t
val fingerprint : t -> string

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
