(** Verify and scan current state, every retained snapshot and journal segment,
    and all archived states using a shared bounded reader rooted at the session
    directory. Checks fallback anchors, continuity, the exact live journal head,
    archive reference identities/digests and incomplete/corrupt storage. Any error
    invalidates the attempt; no partial reference set is returned.

    This covers historical session storage only. Blob/export consumers, replay
    buffers, idempotency responses and active preparations still need separate
    protection before any deletion. Call within an actor checkpoint while
    publication is serialized. *)
val scan
  :  reader:Agent_store.Retention_reader.t
  -> handle:Agent_store.Session_store.Handle.t
  -> state:Session_state.t
  -> journal_current:Agent_store.Journal_segment.Id.t
  -> transaction_hash:string option
  -> max_file_bytes:int
  -> max_frame_bytes:int
  -> candidates:Agent_protocol.Id.Blob.t list
  -> (Agent_protocol.Id.Blob.t list, Agent_store.Store_error.t) result
