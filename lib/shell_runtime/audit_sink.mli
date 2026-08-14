open! Core

(** Eio-backed JSONL audit sinks for shell executor events. *)

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp, compare, equal]

(** [create_jsonl ~sw ~path ~content ~failure_policy ~secret_filter] opens
    [path] for append and writes one synchronized JSON object per event. *)
val create_jsonl
  :  sw:Eio.Switch.t
  -> path:_ Eio.Path.t
  -> content:Chatmd_shell_spec.Shell_spec.audit_content
  -> failure_policy:Shell_access.Audit.failure_policy
  -> secret_filter:Shell_access.Secret_filter.t
  -> (Shell_access.Audit.t, error) result

(** JSONL sink with SHA-256 chaining across events. *)
val create_chained_jsonl
  :  env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> path:_ Eio.Path.t
  -> content:Chatmd_shell_spec.Shell_spec.audit_content
  -> failure_policy:Shell_access.Audit.failure_policy
  -> secret_filter:Shell_access.Secret_filter.t
  -> (Shell_access.Audit.t, error) result

(** Size-based rotating JSONL sink. [path] is resolved below the standard
    filesystem capability and files are retained as [.1] through
    [.max_files]. *)
val create_rotating_jsonl
  :  env:Eio_unix.Stdenv.base
  -> path:string
  -> max_bytes:int64
  -> max_files:int
  -> content:Chatmd_shell_spec.Shell_spec.audit_content
  -> failure_policy:Shell_access.Audit.failure_policy
  -> secret_filter:Shell_access.Secret_filter.t
  -> integrity_chaining:bool
  -> (Shell_access.Audit.t, error) result

(** Standard chained and rotating sink in a session directory. *)
val create_session
  :  env:Eio_unix.Stdenv.base
  -> session_dir:Eio.Fs.dir_ty Eio.Path.t
  -> content:Chatmd_shell_spec.Shell_spec.audit_content
  -> failure_policy:Shell_access.Audit.failure_policy
  -> secret_filter:Shell_access.Secret_filter.t
  -> (Shell_access.Audit.t, error) result

(** Restores the next registry-wide audit sequence from the standard session
    audit file. Missing or empty files start at zero; malformed tails fail
    closed. The returned atomic is shared by every runtime in the registry. *)
val session_sequence_counter
  : session_dir:Eio.Fs.dir_ty Eio.Path.t -> (int Atomic.t, error) result

(** Host-provided organization collector transport. *)
val create_organization_collector
  :  failure_policy:Shell_access.Audit.failure_policy
  -> send:(string -> (unit, string) result)
  -> Shell_access.Audit.t

(** Writes the same already-sequenced envelope to every sink. *)
val fan_out : Shell_access.Audit.t list -> Shell_access.Audit.t

(** Appends a non-executing management action, such as grant revocation, to
    the same chained envelope format used by executor events. The returned
    value is the assigned sequence. The caller supplies redacted fields only. *)
val append_management_event
  :  env:Eio_unix.Stdenv.base
  -> path:string
  -> session_id:string option
  -> runtime_id:string
  -> manifest_sha256:string
  -> request_id:string
  -> event:string
  -> fields:(string * Jsonaf.t) list
  -> (int64, error) result

(** [create_flow ~flow ~content ~failure_policy ~secret_filter] writes JSONL
    events to an existing Eio sink such as standard error. *)
val create_flow
  :  flow:_ Eio.Flow.sink
  -> content:Chatmd_shell_spec.Shell_spec.audit_content
  -> failure_policy:Shell_access.Audit.failure_policy
  -> secret_filter:Shell_access.Secret_filter.t
  -> Shell_access.Audit.t
