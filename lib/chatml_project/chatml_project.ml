module Declared_at = Chatml_project_declaration.Declared_at
module Error = Chatml_project_declaration.Error
module Git_pin = Chatml_project_declaration.Git_pin
module Dependency = Chatml_project_declaration.Dependency
module Module_ref = Chatml_project_declaration.Module_ref
module Source_dir = Chatml_project_declaration.Source_dir
module Module_override = Chatml_project_declaration.Module_override
module Compiler_requirement = Chatml_project_declaration.Compiler_requirement
module Scope_spec = Chatml_project_declaration.Scope_spec
module Entry = Chatml_project_declaration.Entry
module Selection = Chatml_project_declaration.Selection
module Namespace = Chatml_project_namespace

type t = Chatml_project_declaration.t

let source = Chatml_project_declaration.source

module Environment_names = struct
  module Error = Chatml_project_declaration.Environment_names.Error

  let list = Chatml_project_declaration.Environment_names.list
end

module Declared_paths = struct
  type role = Chatml_project_declaration.Declared_paths.role =
    | Source_directory
    | Module_implementation
    | Module_interface
    | Entry_source
    | Local_dependency_project
  [@@deriving equal, sexp_of]

  module Error = Chatml_project_declaration.Declared_paths.Error

  let contains = Chatml_project_declaration.Declared_paths.contains
end

let declared_at = Chatml_project_declaration.declared_at
let namespace = Chatml_project_declaration.namespace
let source_dirs = Chatml_project_declaration.source_dirs
let exclusions = Chatml_project_declaration.exclusions
let module_overrides = Chatml_project_declaration.module_overrides
let dependencies = Chatml_project_declaration.dependencies
let required_tools = Chatml_project_declaration.required_tools
let select = Chatml_project_declaration.select
let decode = Chatml_project_decode.decode

module Lock = Chatml_project_lock
