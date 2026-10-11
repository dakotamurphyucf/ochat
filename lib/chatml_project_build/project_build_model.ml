open Core
module Captured = Chatml_source
module Project = Chatml_project
module Catalog = Chatml_catalog
module Prepared = Chatml_prepared_program

module Limits = struct
  type t =
    { max_work_steps : int
    ; max_input_bytes : int option
    ; document : Document_schema.Limits.t
    }

  let create ~max_work_steps ~max_input_bytes ~document =
    if max_work_steps <= 0 || Option.exists max_input_bytes ~f:(fun bytes -> bytes <= 0)
    then Or_error.error_string "Project composition limits must be positive"
    else Ok { max_work_steps; max_input_bytes; document }
  ;;

  let max_work_steps t = t.max_work_steps
  let max_input_bytes t = t.max_input_bytes
  let document t = t.document
end

module Error = struct
  type problem =
    | Missing_catalog of Captured.Id.t
    | Unexpected_catalog of Captured.Id.t
    | Namespace_conflict of string
    | Invalid_repository_member of Captured.Id.t
    | Invalid_acquisition_key
    | Invalid_dependency of string
    | Missing_module of Project.Module_ref.t
    | Compiler_requirement
    | Invalid_entry_selection
    | Invalid_lock_identity
    | Lock_mismatch
    | Missing_capture of Captured.Id.t
    | Duplicate_capture of Captured.Id.t
    | Needs_update
    | Limit_exceeded
    | Accounting_overflow
    | Declaration of Project.Error.t
    | Catalog of Catalog.Error.t
    | Prepared of Prepared.Error.t

  type t =
    { problem : problem
    ; declared_at : Project.Declared_at.t option
    }

  let create ?declared_at problem = { problem; declared_at }
  let problem t = t.problem
  let declared_at t = t.declared_at
end

module Catalog_input = struct
  type t =
    { catalog : Catalog.t
    ; origin : Project.Lock.Catalog.origin
    ; acquisition_key : string option
    ; dependencies : Project.Lock.Dependency_selection.t list
    }

  let declaration t = Captured.id (Project.source (Catalog.project t.catalog))
  let declared_at t = Project.declared_at (Catalog.project t.catalog)
end

module Entry = struct
  type t =
    | Declared
    | Captured of Captured.t
end

module Operation = struct
  type t =
    | Initial_prepare
    | Build of { previous_lock : Captured.t }
    | Update of { previous_lock : Captured.t option }
end

module Accounting = struct
  type t = { work_steps : int }

  let create ~work_steps = { work_steps }
  let work_steps t = t.work_steps
end
