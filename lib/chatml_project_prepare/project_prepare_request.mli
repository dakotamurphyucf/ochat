open Core
module Project = Chatml_project
module Build = Chatml_project_build
module Captured = Chatml_source

module Root : sig
  type t =
    | Local of
        { locator : string
        ; project : Source.Portable_path.t
        }
    | Git of
        { repository : string
        ; pin : Project.Git_pin.t
        ; package_root : Source.Portable_path.t option
        ; project : Source.Portable_path.t
        }
end

(** Metadata admission does not validate a declaration or grant authority.
    Captured/previous-lock bytes are immutable inputs and separately bounded by
    composition. The authentic compiler contract is never reconstructed by name. *)
type t = private
  { root : Root.t
  ; operation : Build.Operation.t
  ; selection : Project.Selection.request
  ; entry : Build.Entry.t
  ; contract : Chatml.Chatml_compiler_contract.t
  ; lock_id : Captured.Id.t
  }

val create
  :  Root.t
  -> operation:Build.Operation.t
  -> selection:Project.Selection.request
  -> entry:Build.Entry.t
  -> contract:Chatml.Chatml_compiler_contract.t
  -> lock_id:Captured.Id.t
  -> max_request_bytes:int
  -> t Or_error.t
