open Core
module Model = Project_build_model

(** Exact capture validation and own-catalog reconstruction shared by restoration
    and recomposition. No closure or contract admission has occurred yet. The
    caller retains one Budget through reconstruction and final assembly. *)
module Reconstruction : sig
  type t =
    { receipt : Chatml_project.Lock.t
    ; root : Model.Catalog_input.t
    ; catalogs : Model.Catalog_input.t list
    ; entry : Model.Entry.t
    }
end

val reconstruct
  :  Project_build_budget.t
  -> limits:Model.Limits.t
  -> lock_source:Chatml_source.t
  -> sources:Chatml_source.t list
  -> (Reconstruction.t, Model.Error.t) Result.t

(** Capture-only reconstruction of a selected project. [sources] excludes the
    separately supplied exact [lock_source]. Reject duplicate, missing, surplus
    or conflicting captures and reconcile every receipt fact through the existing
    project decoder, catalog planner, closure and Prepared admission. Optional
    omissions describe this immutable selection, not proven native absence or
    completeness of historical directory enumeration.

    Preserve exact supplied lock bytes; no canonical re-encoding substitutes for
    their captured identity. Restore does not apply fresh Build/update policy.
    No filesystem/cache/process/network lookup or live provenance proof. Sources
    remain package-relative; Git package prefixes remain immutable origin intent.
    Work polls and checked input bounds precede reconstruction; final input bytes
    are charged once by assembly. Checkpoint exceptions propagate unchanged. *)
val restore
  :  Project_build_budget.t
  -> limits:Model.Limits.t
  -> contract:Chatml.Chatml_compiler_contract.t
  -> lock_source:Chatml_source.t
  -> sources:Chatml_source.t list
  -> (Project_build_assembly.t, Model.Error.t) Result.t
