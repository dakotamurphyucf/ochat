open Core
module CM = Prompt.Chat_markdown
module P = Agent_protocol
module Store = Agent_store.Prompt_artifact_store

(** Captured child source custody, without persistence identity or source authority.
    Retains the complete defining parent's authored/program/content inventory and
    unknowns, selecting only the exact already-captured child Root. No original
    source acquisition, Fetch, compiler, initializer or reservation occurs. *)
type t

(** Require the original [reference] to occur physically in the SAME admitted
    parent Parsed. Validate that Parsed's Root against the parent artifact, then
    select only its genuine queued key from the captured source closure. Tool,
    Persistent_tool and Content share this owner. Nonlocal/external/missing keys,
    foreign references and inconsistent custody reject; no path/name/byte matching
    can manufacture membership. A projected content descriptor must first pass
    CM.content_agent_reference with the parent's exact document projection.

    Explicit content limits are required for present content custody; all parent
    records/unknowns are preserved while the actual selected Root is materialized.
    Structural artifact ceilings are existing Prompt_program_custody policy;
    borrowed checkpoint polls all owned walks and replay. No callback retained.
    Original callback exceptions/cancellation/backtraces propagate unchanged. *)
val capture
  :  parent:Prompt_revision.t
  -> reference:CM.Agent_reference.t
  -> content_limits:Chatmd_prompt_content.Limits.t option
  -> checkpoint:(unit -> unit)
  -> (t, P.Error.t) Result.t

(** Compatibility path used by Authored_agent_source only for a parent with NO
    parser custody. Require the actual emitted local tool descriptor from that
    parent's elements; keep existing confined artifact-path normalization and
    captured-byte membership. Never permit this adapter for an owned parent.
    This does not grant persistence or accept an arbitrary replacement descriptor. *)
val capture_legacy_tool
  :  parent:Prompt_revision.t
  -> declaration:CM.agent_tool
  -> content_limits:Chatmd_prompt_content.Limits.t option
  -> checkpoint:(unit -> unit)
  -> (t, P.Error.t) Result.t

val root_relative_path : t -> string
val authored_sources : t -> Chatmd_source_bundle.t
val programs_sha256 : t -> string option
val content_sha256 : t -> string option

(** Adopt exact same child Root and complete prior inventory/unknown ownership.
    Compute the existing full program fingerprint before returning. Does not
    select a host role by itself. Earlier immutable value remains unchanged. *)
val with_programs
  :  t
  -> Chatml_prompt_programs.t
  -> checkpoint:(unit -> unit)
  -> (t, P.Error.t) Result.t

(** Pure existing artifact construction; source_receipt/parser/runtime versions
    inherited exactly from defining parent, retained bundles/bytes unchanged.
    Caller-supplied IDs/time do not create source or execution authority. *)
val artifact
  :  t
  -> revision_id:P.Id.Prompt_revision.t
  -> created_at:P.Timestamp.t
  -> (Store.Artifact.t, Agent_store.Store_error.t) Result.t

(** Original parent ID AND manifest must match. Verify child's complete captured
    tree and attach its exact original child Parsed/materialized content. Legacy
    adapter keeps existing captured-tree reparse behavior. [revision_id] is an
    ephemeral preparation identity chosen by the actual caller, never installed
    here: persistent wrapper supplies its UNCHANGED prv_authored_<fingerprint>.
    No source directory reads or content re-materialization on the owned path. *)
val resource_revision
  :  t
  -> parent:Prompt_revision.t
  -> revision_id:P.Id.Prompt_revision.t
  -> checkpoint:(unit -> unit)
  -> (Prompt_revision.t, P.Error.t) Result.t
