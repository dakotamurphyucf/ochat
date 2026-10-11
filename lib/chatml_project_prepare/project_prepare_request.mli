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

module Entry : sig
  type t =
    | Declared
    | Captured_fragment of Captured.t
    | Captured_file of
        { source : Captured.t
        ; root_member : Source.Portable_path.t option
        }
end

(** Metadata admission does not grant native authority. A file member is a
    host-established relationship to the selected admitted root, never inferred
    from retained artifact paths. Actual catalog identity/bytes are reconciled
    after capture. No namespace or source override is invented. *)
type t = private
  { root : Root.t
  ; operation : Build.Operation.t
  ; selection : Project.Selection.request
  ; entry : Entry.t
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

val create_with_entry
  :  Root.t
  -> operation:Build.Operation.t
  -> selection:Project.Selection.request
  -> entry:Entry.t
  -> contract:Chatml.Chatml_compiler_contract.t
  -> lock_id:Captured.Id.t
  -> max_request_bytes:int
  -> t Or_error.t
