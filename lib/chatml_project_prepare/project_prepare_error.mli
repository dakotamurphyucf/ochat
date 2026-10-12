open Core

(** No raw exception, credential, process output or live capability in failures. *)
type problem =
  | Limit_exceeded
  | Invalid_accounting
  | Accounting_overflow
  | Timed_out
  | Authority_denied
  | Conflicting_selection
  | Entry_revision_conflict of Chatml_source.Id.t
  | Roots of Chatml_local_roots.Error.t
  | Source of Chatml_local_source.Error.t
  | Local of Chatml_local_capture.Error.t
  | Git_host of Chatml_git_host.Error.t
  | Git_catalog of Chatml_git_catalog.Error.t
  | Bootstrap of Document_schema.Error.t
  | Declaration of Chatml_project.Error.t
  | Composition of Chatml_project_build.Error.t
  | Tool_admission of Chat_response.Tool_capability.error
  | Stale_selection
  | Publication of Agent_store.Store_error.t
  | Publication_uncertain of Agent_store.Store_error.t

type t

val create : ?declared_at:Chatml_project.Declared_at.t -> problem -> t
val problem : t -> problem
val declared_at : t -> Chatml_project.Declared_at.t option
