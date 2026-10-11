open Core
module Model = Project_build_model

(** Complete immutable compiler input plus the canonical selected lock capture.
    No acquisition keys, host callbacks or native authority survive assembly. *)
type t

module Lock_input : sig
  type t =
    | Generate of Chatml_source.Id.t
    | Restore of
        { source : Chatml_source.t
        ; receipt : Chatml_project.Lock.t
        }

  (** Restore receives the receipt decoded from [source]. Assembly compares its
      canonical metadata with the recomposed receipt, then retains [source]'s exact
      bytes in Prepared, including accepted JSON whitespace/key ordering. *)
end

(** The caller first validates/canonicalizes the complete declaration closure and
    previous-selection policy. Assembly reconciles entry collisions, materializes
    each catalog's own scopes, captures the finished lock and performs complete
    Prepared admission against the authentic selected compiler contract. *)
val create
  :  Project_build_budget.t
  -> Model.Catalog_input.t
  -> catalogs:Model.Catalog_input.t list
  -> entry:Model.Entry.t
  -> contract:Chatml.Chatml_compiler_contract.t
  -> lock:Lock_input.t
  -> limits:Model.Limits.t
  -> (t, Model.Error.t) Result.t

val prepared : t -> Chatml_prepared_program.t
val lock : t -> Chatml_project.Lock.t
val lock_source : t -> Chatml_source.t
