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

type t = Chatml_project_declaration.t

let source = Chatml_project_declaration.source
let namespace = Chatml_project_declaration.namespace
let source_dirs = Chatml_project_declaration.source_dirs
let exclusions = Chatml_project_declaration.exclusions
let module_overrides = Chatml_project_declaration.module_overrides
let dependencies = Chatml_project_declaration.dependencies
let required_tools = Chatml_project_declaration.required_tools
let select = Chatml_project_declaration.select
let decode = Chatml_project_decode.decode

module Lock = Chatml_project_lock
