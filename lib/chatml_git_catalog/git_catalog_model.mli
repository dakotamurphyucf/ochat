open Core
module Host = Chatml_git_host
module Project = Chatml_project
module Path = Source.Portable_path

module Limits : sig
  type t = private
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

  val create
    :  max_object_bytes:int
    -> max_total_object_bytes:int
    -> max_objects:int
    -> max_tree_entries:int
    -> max_sources:int
    -> max_source_bytes:int
    -> max_total_source_bytes:int
    -> max_path_components:int
    -> max_work_steps:int
    -> document:Document_schema.Limits.t
    -> t Or_error.t
end

module Accounting : sig
  type t =
    { object_count : int
    ; object_bytes : int
    ; tree_entries : int
    ; source_count : int
    ; source_bytes : int
    ; work_steps : int
    }

  val object_count : t -> int
  val object_bytes : t -> int
  val tree_entries : t -> int
  val source_count : t -> int
  val source_bytes : t -> int
  val work_steps : t -> int
end

module Error : sig
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

  type t

  val create
    :  ?declared_at:Project.Declared_at.t
    -> ?related:Project.Declared_at.t
    -> problem
    -> t

  val problem : t -> problem
  val declared_at : t -> Project.Declared_at.t option
  val related : t -> Project.Declared_at.t option
end
