open! Core
module Programs = Chatml_prompt_programs
module Build = Chatml_project_build
module Capture = Chatml_source

let tagged name detail = Sexp.List [ Sexp.Atom name; detail ]
let id name value = tagged name [%sexp (value : Capture.Id.t)]
let atom name = Sexp.Atom name

let selected error =
  match Build.Error.problem error with
  | Missing_catalog value -> id "Missing_catalog" value
  | Unexpected_catalog value -> id "Unexpected_catalog" value
  | Namespace_conflict value -> tagged "Namespace_conflict" [%sexp (value : string)]
  | Invalid_repository_member value -> id "Invalid_repository_member" value
  | Invalid_acquisition_key -> atom "Invalid_acquisition_key"
  | Invalid_dependency value -> tagged "Invalid_dependency" [%sexp (value : string)]
  | Missing_module value ->
    tagged "Missing_module" [%sexp (Chatml_project.Module_ref.name value : string)]
  | Compiler_requirement -> atom "Compiler_requirement"
  | Invalid_entry_selection -> atom "Invalid_entry_selection"
  | Invalid_lock_identity -> atom "Invalid_lock_identity"
  | Lock_mismatch -> atom "Lock_mismatch"
  | Missing_capture value -> id "Missing_capture" value
  | Duplicate_capture value -> id "Duplicate_capture" value
  | Needs_update -> atom "Needs_update"
  | Limit_exceeded -> atom "Limit_exceeded"
  | Accounting_overflow -> atom "Accounting_overflow"
  | Declaration value ->
    tagged
      "Declaration"
      [%sexp (Chatml_project.Error.details value : Document_schema.Error.t)]
  | Catalog value ->
    tagged
      "Catalog"
      (Chatml_catalog.Error.sexp_of_problem (Chatml_catalog.Error.problem value))
  | Prepared value -> tagged "Prepared" [%sexp (value : Chatml_prepared_program.Error.t)]
;;

let provided = function
  | Chatml_program_document.Error.Document value ->
    tagged "Document" [%sexp (value : Document_schema.Error.t)]
  | Prepared value -> tagged "Prepared" [%sexp (value : Chatml_prepared_program.Error.t)]
  | Invalid_metadata value -> tagged "Invalid_metadata" [%sexp (value : string list)]
  | Missing_source value -> id "Missing_source" value
  | Duplicate_source value -> id "Duplicate_source" value
  | Unexpected_source value -> id "Unexpected_source" value
  | Revision_mismatch value -> id "Revision_mismatch" value
  | Fingerprint_mismatch -> atom "Fingerprint_mismatch"
  | Contract_mismatch -> atom "Contract_mismatch"
  | Limit_exceeded -> atom "Limit_exceeded"
;;

let sexp_of_error = function
  | Programs.Error.Document value ->
    tagged "Document" [%sexp (value : Document_schema.Error.t)]
  | Storage value -> tagged "Storage" [%sexp (value : Agent_store.Store_error.t)]
  | Provided value -> tagged "Provided" (provided value)
  | Selected value -> tagged "Selected" (selected value)
  | Invalid_binding value -> tagged "Invalid_binding" [%sexp (value : string)]
  | Duplicate_program (binding, digest) ->
    tagged
      "Duplicate_program"
      [%sexp (binding : Programs.Binding.t), (Capture.Digest.to_hex digest : string)]
  | Conflicting_source value -> id "Conflicting_source" value
  | Missing_contract value ->
    tagged "Missing_contract" [%sexp (value : Programs.Binding.t)]
  | Unexpected_contract value ->
    tagged "Unexpected_contract" [%sexp (value : Programs.Binding.t)]
  | Contract_mismatch value ->
    tagged "Contract_mismatch" [%sexp (value : Programs.Binding.t)]
  | Script_mismatch value -> tagged "Script_mismatch" [%sexp (value : Programs.Binding.t)]
  | Invalid_inventory value -> tagged "Invalid_inventory" [%sexp (value : string)]
  | Limit_exceeded -> atom "Limit_exceeded"
;;
