open! Core
module P = Agent_protocol
module Project = Chatml_authoring_analysis
module Document = Chatmd_authoring_analysis
module Ledger = Chatml_authoring_context

(** PRIVATE immutable comparison index owned by the single Core authoring
    service. It stores bounded published Complete labels only, never a second
    checked graph or source authority. Failed Checked values remain in their
    lower Project/Document cache for same-run syntax/diagnostic queries but
    receive no certified snapshot row. *)
type t

type row

module Error : sig
  type t =
    | Retention_limit
    | Identity_exhausted
end

(** One owner-local nonce is minted at Core.create. A checked monotonic counter
    gives each successful Complete publication a distinct incarnation string
    <=128 wire bytes; cancelled/unpublished candidates do not enter the index.
    max_rows is the same validated analysis Policy.max_results, not a second
    capacity. *)
val create : owner_nonce:P.Id.Transaction.t -> max_rows:int -> t Or_error.t

val retained_count : t -> int

(** Prepare outside the gate. The full wire Capture must correspond to the
    current lower candidate's real Project/Document stamp; the host validates
    that before calling. Key is the exact context identity (a context has one
    current selected result). Replacement removes the old row. The new label
    and updated immutable index are returned together; no mutation occurs. *)
val prepare_complete
  :  t
  -> identity:Ledger.Identity.t
  -> capture:P.Authoring.Analysis.Capture.t
  -> validated_graph:Chatml_source.Digest.t
  -> (t * P.Authoring.Analysis.Snapshot_id.t, Error.t) result

(** An explicit Failed or Parsed-only current result retires a formerly
    certified row for this same context. Ordinary unavailable without a new
    admitted capture leaves existing current evidence unchanged. *)
val retire : t -> identity:Ledger.Identity.t -> t

(** Currentness pruning when the one Core ledger transitions. For each bounded
    row, compare exact Capture target/generation/fingerprint against the
    retained current lower Project/Document cache; a changed/evicted result
    removes it. Does not reparse or inspect source paths. *)
val transition
  :  t
  -> project:Project.Project_cache.t
  -> document:Document.Document_cache.t
  -> t

(** Exact comparison only. The query service separately repeats Core actor,
    context, selection and source authority and then obtains the real lower
    Checked value from the corresponding cache. A row is not a bearer token. *)
val current
  :  t
  -> identity:Ledger.Identity.t
  -> capture:P.Authoring.Analysis.Capture.t
  -> snapshot:P.Authoring.Analysis.Snapshot_id.t
  -> row option

val capture : row -> P.Authoring.Analysis.Capture.t
val validated_graph : row -> Chatml_source.Digest.t

(** Full context/target/declaration/source/contract/environment comparison
    reused by the separate provisional index; no label or authority check. *)
val same_capture
  :  P.Authoring.Analysis.Capture.t
  -> P.Authoring.Analysis.Capture.t
  -> bool
