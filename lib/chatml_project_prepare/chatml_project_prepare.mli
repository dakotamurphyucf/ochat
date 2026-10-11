open Core
module Project = Chatml_project
module Build = Chatml_project_build
module Roots = Chatml_local_roots
module Git = Chatml_git_host
module Captured = Chatml_source

(** Host-only selected Local/Git preparation. Compiler/queries/resume and
    capture-only restore have no path to this API. No package registry/solver or
    Provided source kind; a supplied complete Prepared graph goes to source custody. *)

module Policy : sig
  (** Literal per-stage host ceilings. Host aggregation derives explicit smaller
      allowances before each stage; it never increases any supplied ceiling or
      translates authored requirements into tool/process/filesystem grants. *)
  type t

  val create
    :  roots:Roots.Limits.t
    -> local:Chatml_local_capture.Limits.t
    -> git_objects:Chatml_git_catalog.Limits.t
    -> git_native:Git.Limits.t
    -> composition:Build.Limits.t
    -> t
end

module Limits : sig
  (** One attempt's maxima. Work is the sum of documented backend/pure adapter
      units, not CPU/syscall proof. Git request bytes count the actual native
      request ledger, exposed through its final immutable receipt. Acquisition
      storage is the sum of admitted peak-payload
      reservations before work, not OS disk quota/cumulative writes. *)
  type t

  val create
    :  max_packages:int
    -> max_dependency_visits:int
    -> max_source_count:int
    -> max_source_bytes:int
    -> max_local_directory_entries:int
    -> max_git_objects:int
    -> max_git_object_bytes:int
    -> max_git_tree_entries:int
    -> max_native_request_bytes:int
    -> max_native_output_bytes:int
    -> max_acquisition_payload_reserved:int
    -> max_work_steps:int
    -> max_prepared_input_bytes:int
    -> wall_time:Time_ns.Span.t
    -> t Or_error.t
end

module Host : sig
  (** Borrowed explicit host authority, not author config. Retain anchor and Git
      store switches until each joined preparation returns. No ambient fs/cwd/
      HOME/PATH/install lookup or credential callback. Distinct exact route grants
      must be unambiguous. Git grants are trusted deployment selections; local
      dependencies use actual descriptor roots and retained identities. *)
  type t

  val create
    :  Roots.Anchor.t list
    -> git_store:Git.Store.t option
    -> git_grants:Git.Grant.t list
    -> max_grants:int
    -> max_grant_bytes:int
    -> t Or_error.t
end

module Root : sig
  (** Explicit locator intent, never authority. The selected project is package-
      relative; Git package_root affects raw tree lookup only. No native identity
      survives in a lock. Request admission bounds strings before scanning. *)
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

module Request : sig
  type t

  (** Exact authentic contract supplied by the existing selected target adapter.
      Captured entry requires root Library own-env selection; Declared uses the
      selected declared entry. Initial/Build/Update use the same pure Selection_gate;
      Build preserves selected code while allowing environment/namespace changes.
      lock_id is an explicit logical custody identity, never a native filename. *)
  val create
    :  Root.t
    -> operation:Build.Operation.t
    -> selection:Project.Selection.request
    -> entry:Build.Entry.t
    -> contract:Chatml.Chatml_compiler_contract.t
    -> lock_id:Captured.Id.t
    -> max_request_bytes:int
    -> t Or_error.t
end
