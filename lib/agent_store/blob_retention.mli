(** Validated blob consumers and candidate-to-candidate dependency edges.
    This is a reference proof component, not a collector or deletion authority. *)
type t

(** Whether a candidate has verified final data and installed final metadata with
    no remaining temporary data/metadata/partial files needing intent ownership.
    A complete temporary copy or final data alone does not satisfy publication. *)
val published : t -> Agent_protocol.Id.Blob.t -> bool

(** Scan all session-owned and shared temporary blobs under a live storage scope.
    The reader must start at the exact session root; temporary reads share its
    remaining budgets. Candidates must come from the session's validated durable
    private intents. Candidate files may be absent or partially prepared; complete
    data must match its intent and decode as a completion. A shorter candidate
    partial is not a readable completion and supplies no dependency edges.
    Other blobs require complete matching metadata/data. [developer] is minted
    by the SAME live Blob coordinator and developer-root gate. It proves only
    source binding/exclusion: this [scan] itself MUST invoke the complete
    [Developer_result_inventory.Collection.scan] for every historical
    run/epoch/job, private intent and Blob pair and feed original bytes plus
    decoded strings into this candidate graph. An unbound token is
    valid only after the authentic authority/intent/Blob namespaces are proved
    empty; no caller-provided absence flag can bypass this requirement. Unknown files, unowned
    partials, identity/digest/JSON errors and exhausted budgets fail the entire
    scan. Original raw length/digest validation precedes JSON admission. The full
    decoded content must satisfy the shared document depth/field/node profile
    and [max_file_bytes] before recursive reference scanning or completion decoding.
    All file bytes remain charged to the same aggregate reader; no subtree walk
    resets a validation or retention allowance.
    Caller must retain actor, cache, publisher, storage and the issuing
    Session_store Handle's developer-root gate through combining this graph
    with every other root and using the result. *)
val scan
  :  scope:Blob_store.retention
  -> developer:Blob_store.Developer_retention.t
  -> session:Session_store.Handle.t
  -> reader:Retention_reader.t
  -> checkpoint:(unit -> unit)
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
  -> checkpoint:(unit -> unit)
  -> roots:Agent_protocol.Id.Blob.t list
  -> (Agent_protocol.Id.Blob.t list, Store_error.t) result

(** After [references] and all caller roots succeed, retire exact developer
    private markers embedded in this same fully validated graph only when
    their authentic installed terminal agrees with the selected Completion.
    Artifact selections additionally require an exact complete durable Blob;
    Inline selections require no Blob but still retire their exact marker.
    Uses
    [Result_intent_file]'s exact removal with the SAME borrowed Store root,
    reader and Blob coordinator; no recursive Store lease or replacement
    terminal writer. The caller invokes this inside the protected mutation
    phase after its final cancellation checkpoint. An unbound, proved-empty
    developer namespace retires zero markers. Any failed exact cleanup remains
    retryable and cannot authorize orphan Blob deletion. *)
val retire_published_developer_markers
  :  t
  -> scope:Blob_store.retention
  -> developer:Blob_store.Developer_retention.t
  -> reader:Retention_reader.t
  -> checkpoint:(unit -> unit)
  -> (int, Store_error.t) result
