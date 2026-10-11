open Core
module Budget = Chatml_local_capture_budget

type limit = Budget.Limit.t =
  | Directory_entries
  | Source_count
  | Source_bytes
  | Total_source_bytes
  | Path_components
  | Work_steps
[@@deriving equal, sexp_of]

type problem =
  | Declaration of Chatml_project.Error.t
  | Filesystem of
      { failure : Chatml_local_source.Error.t
      ; member : Source.Portable_path.t option
      }
  | Catalog of Chatml_catalog.Error.t
  | Limit_exceeded of
      { limit : limit
      ; maximum : int
      }
  | Accounting_overflow
[@@deriving sexp_of]

type t

val create
  :  Chatml_project.Declared_at.t
  -> ?related:Chatml_project.Declared_at.t
  -> problem
  -> t

val of_budget : Chatml_project.Declared_at.t -> Budget.problem -> t
val problem : t -> problem
val declared_at : t -> Chatml_project.Declared_at.t
val related : t -> Chatml_project.Declared_at.t option
