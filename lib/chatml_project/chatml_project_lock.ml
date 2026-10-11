module Error = Chatml_project_declaration.Error
module Source_entry = Chatml_project_lock_receipt.Source_entry
module Module_selection = Chatml_project_lock_receipt.Module_selection
module File_scope = Chatml_project_lock_receipt.File_scope
module Catalog = Chatml_project_lock_receipt.Catalog

type t = Chatml_project_lock_receipt.t

let create = Chatml_project_lock_receipt.create
let decode = Chatml_project_lock_codec.decode
let to_string = Chatml_project_lock_codec.to_string
let entry = Chatml_project_lock_receipt.entry
let environment = Chatml_project_lock_receipt.environment
let compiler_family = Chatml_project_lock_receipt.compiler_family
let target_name = Chatml_project_lock_receipt.target_name
let compiler_contract = Chatml_project_lock_receipt.compiler_contract
let required_tools = Chatml_project_lock_receipt.required_tools
let catalogs = Chatml_project_lock_receipt.catalogs
