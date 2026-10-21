open! Core
module P = Agent_protocol
module Core_owner = Authoring_context_service_core
module Project = Chatml_authoring_analysis
module Document = Chatmd_authoring_analysis
module Ledger = Chatml_authoring_context

(** PRIVATE immutable publication candidate prepared outside the Owner gate.
    These variants are distinct because an authorized successful parser capture
    is useful even if no script Build/checked result exists. A Failed semantic
    check is a checked result with same-run syntax/diagnostics, but has no
    certified snapshot/index row. *)
module Prepared : sig
  type t

  type outcome =
    | No_acquisition
    | Parsed_only
    | Checked_project
    | Checked_document

  val outcome : t -> outcome
  val response : t -> P.Authoring.Analysis.Analyze.t

  (** No new source or checked evidence. Unavailable may be returned without
      changing the previous immutable cache/capture/index. Stale/cancelled
      acquisition never constructs a publishable result. *)
  val unavailable : response:P.Authoring.Analysis.Analyze.t -> t

  (** Real complete Parsed/import closure from explicit Analyze, optional
      independently admitted decoded project metadata (OCH354), and semantic
      Unavailable because the selected Build is absent/invalid. This clears
      any previous checked Document result/index row for this context, but
      publishes genuine parsed structural facts and metadata under the same
      final currentness/CAS. No certified fact leaks from the old selection. *)
  val parsed_only
    :  retained:Core_owner.Retained_document_capture.t
    -> document_after:Document.Document_cache.t
    -> index_after:Authoring_analysis_index.t
    -> templates_after:Authoring_checked_template_cache.t
    -> response:P.Authoring.Analysis.Analyze.t
    -> t

  (** Prepared Project candidate and lower Project_cache.publish result.
      Document cache is transitioned to the returned SAME ledger after
      note_analysis; aggregate policy occupancy is checked outside the gate.
      One Complete result has a bounded new snapshot incarnation/index row;
      Failed has same-run syntax only and evicts an old row. *)
  val checked_project
    :  project_after:Project.Project_cache.t
    -> document_after:Document.Document_cache.t
    -> index_after:Authoring_analysis_index.t
    -> templates_after:Authoring_checked_template_cache.t
    -> response:P.Authoring.Analysis.Analyze.t
    -> t

  (** Real OCH302 Script admission from current Parsed plus independently
      selected exact Build/contract; lower Document_cache.publish result.
      Same genuine Parsed/metadata receipt is published with the cache; no
      independent metadata argument can disagree with it. *)
  val checked_document
    :  retained:Core_owner.Retained_document_capture.t
    -> document_after:Document.Document_cache.t
    -> index_after:Authoring_analysis_index.t
    -> templates_after:Authoring_checked_template_cache.t
    -> response:P.Authoring.Analysis.Analyze.t
    -> t
end

(** On one owner gate, after OCH347 has compared context/generation and both
    cache/capture heads: compare the borrowed immutable index head, then swap
    the prepared cache/capture/index pointers together. No callbacks,
    checkpoint, parser/compiler, wire encoding or allocation proportional to
    source/result size. A changed index head returns Stale_context without
    partial publication. Context transition prunes rows and capture together. *)
val publish_now
  :  Core_owner.t
  -> borrowed_index:Authoring_analysis_index.t
  -> Prepared.t
  -> (P.Authoring.Analysis.Analyze.t, Core_owner.Error.t) result
