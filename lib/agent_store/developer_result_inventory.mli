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
