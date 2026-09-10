(** Validated blob consumers and candidate-to-candidate dependency edges.
    This is a reference proof component, not a collector or deletion authority. *)
type t

(** Scan all session-owned and shared temporary blobs under a live storage scope.
    The reader must start at the exact session root; temporary reads share its
    remaining budgets. Candidates must come from the session's validated durable
    private intents. Candidate files may be absent or partially prepared; complete
    data must match its intent and decode as a completion. A shorter candidate
    partial is not a readable completion and supplies no dependency edges.
    Other blobs require complete matching metadata/data. Unknown files, unowned
    partials, identity/digest/JSON errors and exhausted budgets fail the entire
    scan. Complete JSON is decoded before scanning escaped references.
    Caller must retain actor, cache, publisher and storage serialization until
    combining this graph with every other root and using the result. *)
val scan
  :  scope:Blob_store.retention
  -> session:Session_store.Handle.t
  -> reader:Retention_reader.t
  -> intents:Job_result_intent.t list
  -> max_file_bytes:int
  -> (t, Store_error.t) result

(** Candidate IDs reachable from noncandidate blobs or supplied external roots,
    transitively through candidate completion dependencies. A reachable candidate
    without a complete verified copy rejects proof: its dependencies are unknown.
    Unrooted cycles do not keep themselves alive. External IDs outside the
    candidate set are ignored. No partial reference set is returned on error. *)
val references
  :  t
  -> roots:Agent_protocol.Id.Blob.t list
  -> (Agent_protocol.Id.Blob.t list, Store_error.t) result
