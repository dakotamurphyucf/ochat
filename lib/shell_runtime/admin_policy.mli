open! Core

(** Host/organization ceilings applied after canonical ChatMD compilation and
    before any shell runtime is instantiated. The policy rejects requested
    authority; it never rewrites the manifest silently. *)

module Limit_ceiling : sig
  type t =
    { wall_time_seconds : float option
    ; idle_time_seconds : float option
    ; max_stdin_bytes : int64 option
    ; max_stdout_bytes : int64 option
    ; max_stderr_bytes : int64 option
    ; max_total_bytes : int64 option
    ; cpu_seconds : float option
    ; memory_bytes : int64 option
    ; file_size_bytes : int64 option
    ; open_files : int option
    }
  [@@deriving sexp, compare, equal, jsonaf]

  val none : t
end

type t =
  { source : string
  ; allowed_read_roots : string list option
  ; allowed_write_roots : string list option
  ; allow_network : bool
  ; allow_child_processes : bool
  ; allow_arbitrary_code : bool
  ; allow_privilege_change : bool
  ; require_sandbox : bool
  ; allow_direct_backend : bool
  ; allow_declared_confinement : bool
  ; allow_external_backends : bool
  ; allow_hooks : bool
  ; allowed_reviewer_kinds : string list option
  ; allowed_approval_scopes : Chatmd_shell_spec.Shell_spec.approval_scope list
  ; allow_raw_prefix_grants : bool
  ; allow_yolo : bool
  ; require_executable_hashes : bool
  ; require_trusted_executables : bool
  ; require_audit : bool
  ; require_durable_audit : bool
  ; require_trusted_source : bool
  ; require_signature : bool
  ; denied_programs : string list
  ; denied_argument_substrings : string list
  ; denied_effects : Chatmd_shell_spec.Shell_spec.process_effect list
  ; limits : Limit_ceiling.t
  }
[@@deriving sexp, compare, equal, jsonaf]

type violation =
  { code : string
  ; runtime_id : string option
  ; requested : string
  ; ceiling : string
  ; policy_source : string
  ; remediation : string
  }
[@@deriving sexp, compare, equal]

(** No administrative restrictions. Explicitly useful for hosts that do not
    install an organization policy. *)
val permissive : t

(** Evaluates effective runtimes, including inherited and built-in profiles. *)
val evaluate
  :  t
  -> manifest:Chatmd_shell_spec.Manifest.t
  -> runtimes:Chatmd_shell_spec.Shell_spec.t list
  -> (unit, violation list) result

(** Runtime hard-deny check. This runs before manifest policy/reviewer logic. *)
val check_context : t -> Shell_access.Context.t -> (unit, violation list) result

val allows_scope : t -> Chatmd_shell_spec.Shell_spec.approval_scope -> bool
