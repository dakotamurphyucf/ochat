open! Core

(** Pure administrative candidate construction. Candidates do not mutate actors,
    reserve identifiers, or write archives. Commit through [Session_actor]. *)

type reset_options =
  { keep_history : bool
  ; keep_tasks : bool
  ; keep_grants : bool
  ; keep_labels : bool
  ; workspace_instance : Workspace_instance.t option
  }

val reset
  :  Session_state.t
  -> reset_options
  -> (Session_state.t, Agent_protocol.Error.t) result

(** [rebuild state revision] starts a fresh stopped generation, retaining tasks,
    key-value data, labels, workspace and allocator monotonicity. The runtime
    preparer supplies fresh initial history and initialized moderator state. *)
val rebuild
  :  Session_state.t
  -> Agent_protocol.Id.Prompt_revision.t
  -> (Session_state.t, Agent_protocol.Error.t) result

(** [upgrade state revision] retains history and generation while replacing the
    pinned revision and clearing moderator/shell state for detached preparation. *)
val upgrade : Session_state.t -> Agent_protocol.Id.Prompt_revision.t -> Session_state.t

(** [archive previous candidate kind] retains the previous revision independently
    of journal/snapshot pruning. Persistence writes the archive before commit. *)
val archive
  :  previous:Session_state.t
  -> Session_state.t
  -> Session_state.Compaction_archive.kind
  -> Session_state.t

val payloads
  :  previous:Session_state.t
  -> Session_state.t
  -> Agent_protocol.Event.Durable.Payload.t list
