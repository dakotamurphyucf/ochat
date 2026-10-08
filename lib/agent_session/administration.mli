open! Core

(** Pure administration does not mutate actors, reserve identifiers, write
    archives, or interrupt inference. Admitted replacements and planning-only
    candidates have distinct contracts below. *)

type reset_options =
  { keep_history : bool
  ; keep_tasks : bool
  ; keep_grants : bool
  ; keep_labels : bool
  ; workspace_instance : Workspace_instance.t option
  }

(** Returns a validated replacement with its retained inference ledger advanced
    to the new generation. Active inference attempts or turns reject the reset;
    no retained evidence is cleared or fabricated. *)
val reset
  :  Session_state.t
  -> reset_options
  -> (Session_state.t, Agent_protocol.Error.t) result

(** [rebuild state revision] returns a validated fresh stopped generation, retaining tasks,
    key-value data, labels, workspace and allocator monotonicity. The runtime
    preparer supplies fresh initial history and initialized moderator state.
    Active inference attempts or turns reject the rebuild. *)
val rebuild
  :  Session_state.t
  -> Agent_protocol.Id.Prompt_revision.t
  -> (Session_state.t, Agent_protocol.Error.t) result

(** Planning-only counterparts of [reset] and [rebuild]. They retain the exact
    original inference ledger, including active rows and its original generation.
    Validate with [Session_state.validate_administration_candidate]; they are not
    admitted state documents and must not be persisted directly. Commit only
    through [Session_actor.commit_reconciled_administration] after the actual
    runtime owner has retired and durably reconciled inference. *)
val plan_reset
  :  Session_state.t
  -> reset_options
  -> (Session_state.t, Agent_protocol.Error.t) result

val plan_rebuild
  :  Session_state.t
  -> Agent_protocol.Id.Prompt_revision.t
  -> (Session_state.t, Agent_protocol.Error.t) result

(** [upgrade state revision] retains history and generation while replacing the
    pinned revision and clearing moderator/shell state for detached preparation. *)
val upgrade : Session_state.t -> Agent_protocol.Id.Prompt_revision.t -> Session_state.t

(** [archive previous candidate kind] retains the previous revision independently
    of journal/snapshot pruning. Reset/rebuild reconcile old invocations against
    the candidate history and index their dispositions on the archive reference.
    Candidate history repairs and the index commit atomically with administration.
    Persistence writes the archive before commit. No handlers are run. *)
val archive
  :  archive_reference:
       (previous:Session_state.t
        -> kind:Session_state.Compaction_archive.kind
        -> Agent_protocol.Id.Operation.t
        -> (Session_state.Compaction_archive.t, Agent_protocol.Error.t) result)
  -> previous:Session_state.t
  -> Session_state.t
  -> Session_state.Compaction_archive.kind
  -> (Session_state.t, Agent_protocol.Error.t) result

val payloads
  :  previous:Session_state.t
  -> Session_state.t
  -> Agent_protocol.Event.Durable.Payload.t list
