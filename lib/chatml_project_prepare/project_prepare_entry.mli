open Core

(** Reconcile supplied file bytes ONLY against an actual captured root member.
    Equal catalog bytes reuse canonical identity; disagreement rejects; absence
    retains supplied host identity. No native read or content-based merging.
    Identity lookup/comparison borrows this attempt's bounded work/cancellation. *)
val resolve
  :  Project_prepare_attempt.t
  -> root:Chatml_catalog.t
  -> Project_prepare_request.Entry.t
  -> (Chatml_project_build.Entry.t, Project_prepare_error.t) Result.t
