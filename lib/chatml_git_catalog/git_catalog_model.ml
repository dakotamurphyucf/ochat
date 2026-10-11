open Core
module Host = Chatml_git_host
module Project = Chatml_project
module Path = Source.Portable_path

module Limits = struct
  type t =
    { max_object_bytes : int
    ; max_total_object_bytes : int
    ; max_objects : int
    ; max_tree_entries : int
    ; max_sources : int
    ; max_source_bytes : int
    ; max_total_source_bytes : int
    ; max_path_components : int
    ; max_work_steps : int
    ; document : Document_schema.Limits.t
    }

  let create
        ~max_object_bytes
        ~max_total_object_bytes
        ~max_objects
        ~max_tree_entries
        ~max_sources
        ~max_source_bytes
        ~max_total_source_bytes
        ~max_path_components
        ~max_work_steps
        ~document
    =
    if
      List.exists
        [ max_object_bytes
        ; max_total_object_bytes
        ; max_objects
        ; max_tree_entries
        ; max_sources
        ; max_source_bytes
        ; max_total_source_bytes
        ; max_path_components
        ; max_work_steps
        ]
        ~f:(fun bound -> bound <= 0)
    then Or_error.error_string "Git catalog limits must be positive"
    else
      Ok
        { max_object_bytes
        ; max_total_object_bytes
        ; max_objects
        ; max_tree_entries
        ; max_sources
        ; max_source_bytes
        ; max_total_source_bytes
        ; max_path_components
        ; max_work_steps
        ; document
        }
  ;;
end

module Accounting = struct
  type t =
    { object_count : int
    ; object_bytes : int
    ; tree_entries : int
    ; source_count : int
    ; source_bytes : int
    ; work_steps : int
    }

  let object_count t = t.object_count
  let object_bytes t = t.object_bytes
  let tree_entries t = t.tree_entries
  let source_count t = t.source_count
  let source_bytes t = t.source_bytes
  let work_steps t = t.work_steps
end

module Error = struct
  type limit =
    | Object_bytes
    | Total_object_bytes
    | Object_count
    | Tree_entries
    | Tree_name_bytes
    | Source_count
    | Source_bytes
    | Total_source_bytes
    | Path_components
    | Repository_member_bytes
    | Work_steps
  [@@deriving equal, sexp_of]

  type problem =
    | Host of Host.Error.t
    | Missing_object of Host.Object_id.t
    | Malformed_object_protocol
    | Object_hash_mismatch of Host.Object_id.t
    | Wrong_object_kind of Host.Object_id.t
    | Invalid_commit
    | Invalid_tree
    | Missing_member of Path.t
    | Unsupported_member_mode of Path.t
    | Limit_exceeded of limit
    | Reader_closed
    | Reader_poisoned
    | Reader_busy
    | Foreign_declaration
    | Invalid_namespace_override
    | Conflicting_capture of Chatml_source.Id.t
    | Bootstrap of Document_schema.Error.t
    | Accounting_overflow
    | Declaration of Project.Error.t
    | Catalog of Chatml_catalog.Error.t

  type t =
    { problem : problem
    ; declared_at : Project.Declared_at.t option
    ; related : Project.Declared_at.t option
    }

  let create ?declared_at ?related problem = { problem; declared_at; related }
  let problem t = t.problem
  let declared_at t = t.declared_at
  let related t = t.related
end
