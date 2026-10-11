open Core

type problem =
  | Declaration of Chatml_project.Error.t
  | Invalid_candidate_name of { basename_prefix : string }
  | Invalid_candidate_path of { path_prefix : string }
  | Undeclared_directory
  | Foreign_observation
  | Orphan_interface of Source.Portable_path.t
  | Ambiguous_module of
      { name : string
      ; first : Source.Portable_path.t
      ; second : Source.Portable_path.t
      }
  | Portable_collision of Source.Portable_path.t * Source.Portable_path.t
  | Conflicting_interfaces of
      { implementation : Source.Portable_path.t
      ; first : Source.Portable_path.t option
      ; second : Source.Portable_path.t option
      }
  | Duplicate_capture of Chatml_source.Id.t
  | Conflicting_capture of Chatml_source.Id.t
  | Wrong_namespace of Chatml_source.Id.t
  | Missing_capture of Source.Portable_path.t
  | Unexpected_capture of Chatml_source.Id.t
  | Invalid_association of Source.Portable_path.t
  | Accounting_overflow
[@@deriving sexp_of]

type t

val problem : t -> problem
val declared_at : t -> Chatml_project.Declared_at.t
val related : t -> Chatml_project.Declared_at.t option

val create
  :  Chatml_project.Declared_at.t
  -> ?related:Chatml_project.Declared_at.t
  -> problem
  -> t
