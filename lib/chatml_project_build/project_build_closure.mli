open Core
module Model = Project_build_model

(** Canonicalize duplicate aliases only when admitted acquisition identity, exact
    bytes, definitions, selection and dependency associations agree. Validate the
    complete selected declaration closure, allowing declaration cycles. Reject
    unused supplied catalogs and conflicting effective namespaces. No executable
    source-edge analysis. The returned input facts stay invocation-local. *)
val select
  :  Project_build_budget.t
  -> Model.Catalog_input.t
  -> catalogs:Model.Catalog_input.t list
  -> (Model.Catalog_input.t list, Model.Error.t) Result.t
