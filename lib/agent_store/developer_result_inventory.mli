(** Private bounded, read-only comparison of all developer intent and Blob
    evidence against authentic historical Store rows. The existing Store,
    Retention_reader and Blob scanner own every authority and I/O operation;
    this module owns only the complete typed join. *)
open! Core

module Key : sig
  type t
end

type intent = private
  { key : Key.t
  ; root : Developer_run_store.Result_root.t
  ; job : Agent_protocol.Developer_job.t
  ; document : Developer_result_intent_document.t
  ; source_document : Document_schema.Document.t
  ; bytes : string
  }

type verified = private
  { intent : intent
  ; completion : Agent_protocol.Completion.t
  ; content : string option
  ; reference : Developer_result_reference.t option
  }

module Evidence : sig
  type t = private
    | No_selection of Developer_run_store.Recovery.Candidate.t
    | Incomplete_selection of Developer_run_store.Recovery.Candidate.t * intent
    | Selected of Developer_run_store.Recovery.Candidate.t * verified
end

(** One coordinator-held, no-follow scan with one Reader/work allowance for
    all actual Store documents, historical epochs/jobs, framed sibling intents,
    temporary/durable Blob roots, metadata, bytes and candidate publication.
    Other genuine run/epoch entries are allowed; every unpaired/foreign/corrupt
    entry rejects the whole result before any evidence escapes. A markerless
    completed Artifact requires the real terminal job and exact durable bytes.
    Missing selected Artifact bytes remain [Incomplete_selection] without a
    fabricated Completion. This is comparison custody, never a terminal write
    or disclosure grant. *)
val scan
  :  sw:Eio.Switch.t
  -> runs:Developer_run_store.t
  -> root:Developer_run_store.Result_root.t
  -> blobs:Blob_store.t
  -> max_entries:int
  -> max_bytes:int
  -> max_work:int
  -> max_result_bytes:int
  -> window:Developer_run_store.Recovery.t
  -> view:Developer_run_store.Recovery.View.t
  -> (Evidence.t list, Agent_protocol.Error.t) result

(** Reuse the canonical startup run/epoch/job, private frame and Blob-copy
    join for the collector's complete cross-owner absence proof. This lower
    entry receives the ALREADY borrowed physical Store inventory; it never
    reacquires the Store gate or Blob coordinator. The caller's single Reader
    and checkpoint cover all original bytes, decoded rows, namespace entries,
    hashes and retained source strings. No partial result escapes on error. *)
module Collection : sig
  type t

  (** Other genuine historical runs/epochs, markerless published results and
      verified empty structural intent directories are valid. Every nonempty
      intent must match an authentic job/epoch/capture and any stage must match
      its exact selected reference. Unlike startup recovery, a missing or
      partial selected stage is NOT a deletion proof: its unknown content may
      reference a conversation candidate, so the entire scan defers/fails.
      Unknown, linked, corrupt, unpaired, conflicting or uncertain durable
      evidence likewise invalidates the whole proof. The scoped developer
      token and its owner-derived paths are furnished only by
      [Blob_store.with_developer_retention] under the same physical root; a
      caller-provided path grants no authority. [inventory] must be the SAME
      physical borrow carried by [developer], not an equal-ID snapshot. *)
  val scan
    :  inventory:Developer_run_store.Retention.t
    -> scope:Blob_store.retention
    -> developer:Blob_store.Developer_retention.t
    -> reader:Retention_reader.t
    -> checkpoint:(unit -> unit)
    -> max_result_bytes:int
    -> max_blob_bytes:int64
    -> (t, Store_error.t) result

  (** Feed every validated original run-document byte string, private frame,
      developer Blob metadata/content and decoded admitted string projection as
      distinct roots to the caller's existing candidate scanner. Original
      metadata bytes are retained/streamed under the same Reader cap rather
      than reconstructed from known typed fields; escaped and extension-field
      references must remain visible. Feed long sources in bounded checkpointed
      chunks. This reports references, never deletion authority by itself. *)
  val feed_candidate_references
    :  t
    -> scanner:Blob_reference_scan.t
    -> checkpoint:(unit -> unit)
    -> (unit, Store_error.t) result

  (** After the caller's complete reference proof, retire ONLY exact private
      markers whose authentic historical job has the SAME installed terminal
      Completion. Artifact additionally requires verified complete durable
      metadata/content; Inline has no Blob requirement. Queued/uncertain
      parent acknowledgements keep their marker. Reuse
      [Result_intent_file.remove_exact_with_retention], charging every
      revalidation read to [reader] without reentering the Store lease. Typed
      partial cleanup leaves records for retry. Do not infer an orphan from a
      Blob ID or a missing frame. The caller enters the Blob coordinator's
      protected mutation continuation while retaining [inventory]. *)
  val retire_published_markers
    :  t
    -> inventory:Developer_run_store.Retention.t
    -> reader:Retention_reader.t
    -> checkpoint:(unit -> unit)
    -> (int, Store_error.t) result
end
