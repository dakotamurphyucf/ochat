open Core

module Declaration_details = struct
  type t = Chatml_project.Error.t

  let sexp_of_t t =
    [%sexp
      { source = (Chatml_source.id (Chatml_project.Error.source t) : Chatml_source.Id.t)
      ; details = (Chatml_project.Error.details t : Document_schema.Error.t)
      ; field_path = (Chatml_project.Error.field_path t : string list)
      }]
  ;;
end

type problem =
  | Declaration of Declaration_details.t
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

type t =
  { problem : problem
  ; declared_at : Chatml_project.Declared_at.t
  ; related : Chatml_project.Declared_at.t option
  }

let create declared_at ?related problem = { problem; declared_at; related }
let problem t = t.problem
let declared_at t = t.declared_at
let related t = t.related
