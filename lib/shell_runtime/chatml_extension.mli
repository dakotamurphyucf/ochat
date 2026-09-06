open! Core

(** Compiled and instantiated capability-minimal ChatML shell hooks. *)

type action =
  | Match of bool * string
  | Review_approve
  | Review_approve_for of string
  | Review_deny of string
  | Review_rewrite of string list
  | Review_defer
  | Intercept_continue
  | Intercept_rewrite of string list
  | Intercept_respond of string * string
  | Intercept_reject of string
  | Result_keep
  | Result_replace of string * string
  | Result_reject_disclosure of string
  | Effect_add of string
  | Effect_replace of string list
  | Audit_keep
  | Audit_drop_field of string
  | Audit_replace of string
[@@deriving sexp, compare, equal]

type limits =
  { wall_time_seconds : float
  ; fuel : int
  ; max_tasks : int
  ; max_actions : int
  ; max_value_bytes : int
  ; max_string_bytes : int
  ; max_array_items : int
  ; max_depth : int
  }

type compiled
type instance

val default_limits : limits

val compile
  :  script:Chatmd_shell_spec.Chatmd_script_spec.t
  -> (compiled, Chatmd_shell_spec.Diagnostic.t list) result

(** [with_entrypoint t name] recompiles [t]'s exact source with [name] as the
    kind-specific public entrypoint. *)
val with_entrypoint
  :  compiled
  -> string
  -> (compiled, Chatmd_shell_spec.Diagnostic.t) result

val instantiate
  :  env:Eio_unix.Stdenv.base
  -> ?limits:limits
  -> lifecycle:Chatmd_shell_spec.Shell_spec.lifecycle
  -> compiled
  -> (instance, Chatmd_shell_spec.Diagnostic.t) result

val call
  :  instance
  -> context:Chatml.Chatml_lang.value
  -> event:Chatml.Chatml_lang.value
  -> (action, Chatmd_shell_spec.Diagnostic.t) result

val id : compiled -> string
val kind : compiled -> Chatmd_shell_spec.Chatmd_script_spec.kind
val source_sha256 : compiled -> string
val entrypoint : compiled -> string
val limits : compiled -> limits
val instance_id : instance -> string
val instance_kind : instance -> Chatmd_shell_spec.Chatmd_script_spec.kind
val instance_source_sha256 : instance -> string

(** [snapshot instance] captures durable state, queued internal events, and
    halted state. Invocation-scoped instances reject snapshots. *)
val snapshot : instance -> (Session.Snapshot.t, Chatmd_shell_spec.Diagnostic.t) result

(** [restore instance snapshot] restores an exact persisted state snapshot.
    Legacy snapshots containing only state restore with an empty queue. *)
val restore
  :  instance
  -> Session.Snapshot.t
  -> (unit, Chatmd_shell_spec.Diagnostic.t) result

(** [set_snapshot_handler instance handler] checkpoints every successful
    state transition. A failed checkpoint restores the prior in-memory state
    before the hook returns an error. *)
val set_snapshot_handler
  :  instance
  -> (Session.Snapshot.t -> (unit, string) result)
  -> unit
