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

(** Bounded startup compilation of the same original legacy wrapper and surface.
    Uses the joined compiler; never executes an initializer or substitutes a
    prepared graph. Existing synchronous [compile] remains compatible. *)
val compile_with_limits
  :  env:Eio_unix.Stdenv.base
  -> limits:Chatml_compilation.limits
  -> script:Chatmd_shell_spec.Chatmd_script_spec.t
  -> (compiled, Chatmd_shell_spec.Diagnostic.t list) Result.t

(** Legacy [with_entrypoint] recompiles the original text with the selected ABI
    adapter. Prepared programs select only an already compiled alternative from
    [with_prepared_entrypoints], or return unchanged for the same entrypoint.
    Missing alternatives reject before initialization; no text-only fallback. *)
val with_entrypoint
  :  compiled
  -> string
  -> (compiled, Chatmd_shell_spec.Diagnostic.t) result

(** Attach immutable, already compiled matcher alternatives to the default
    prepared hook. All must have the exact same original declaration, including
    project/environment and limits. Reject legacy programs, non-matcher kinds,
    duplicate/default entrypoints and repeated attachment. No compilation,
    initialization or instance mutation; the selected variant retains its own
    authentic compiler contract and full graph revision. *)
val with_prepared_entrypoints
  :  compiled
  -> alternatives:compiled list
  -> checkpoint:(unit -> unit)
  -> (compiled, Chatmd_shell_spec.Diagnostic.t) result

(** Construct the real config and runner without evaluating initializers. A
    pending instance rejects call, snapshot and restore until activated. *)
val prepare_instance
  :  env:Eio_unix.Stdenv.base
  -> ?limits:limits
  -> lifecycle:Chatmd_shell_spec.Shell_spec.lifecycle
  -> compiled
  -> (instance, Chatmd_shell_spec.Diagnostic.t) result

(** Activate once under the owned runner. Persistent instances initialize;
    Invocation remains lazy per call. Ready activation is idempotent. A failed
    activation is terminal; original exceptions propagate after releasing the
    serialization lock. *)
val activate : instance -> (unit, Chatmd_shell_spec.Diagnostic.t) result

(** Revoke an instance when its actual owner closes. Idempotent and nonyielding;
    closed instances reject activation and all execution/state operations. *)
val close : instance -> unit

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

(** Exact original entry-body digest retained for source custody. *)
val source_sha256 : compiled -> string

(** Saved runtime-state identity. Legacy hooks retain their body digest;
    prepared hooks use the complete immutable graph fingerprint, including
    dependencies, scopes and the compiler contract. *)
val source_revision : compiled -> string

(** Actual immutable prepared execution facts for default and every compiled
    matcher alternative. Legacy text hooks return []. No initialization or
    source recapture; each alternative keeps its own complete graph fingerprint.
    Walks and admission comparisons poll the borrowed checkpoint, whose original
    exceptions propagate. *)
val prepared_hooks
  :  compiled
  -> checkpoint:(unit -> unit)
  -> Chatmd_shell_spec.Manifest.Prepared_hook.t list

val entrypoint : compiled -> string
val default_entrypoint : Chatmd_shell_spec.Chatmd_script_spec.kind -> string
val limits : compiled -> limits
val instance_id : instance -> string
val instance_kind : instance -> Chatmd_shell_spec.Chatmd_script_spec.kind
val instance_source_sha256 : instance -> string
val instance_source_revision : instance -> string

(** Check affinity to the exact immutable declaration used for compilation,
    including its project selection and limits. The compiled entrypoint must be
    the kind's default; changing it requires the existing runtime ABI owner. *)
val matches_script : compiled -> Chatmd_shell_spec.Chatmd_script_spec.t -> bool

(** [snapshot instance] captures durable state, queued internal events, and
    halted state. Invocation-scoped instances reject snapshots. *)
val snapshot : instance -> (Session.Snapshot.t, Chatmd_shell_spec.Diagnostic.t) result

(** [restore instance snapshot] restores an exact persisted state snapshot.
    Legacy snapshots containing only state restore with an empty queue. *)
val restore
  :  instance
  -> Session.Snapshot.t
  -> (unit, Chatmd_shell_spec.Diagnostic.t) result

(** Persist the complete prospective state/queue/halted snapshot BEFORE commit.
    Conversion/validation/cancellation checks precede persistence; successful
    persistence is followed immediately by nonyielding runtime installation.
    The write boundary is protected from asynchronous cancellation. A callback
    Error or exception before success leaves state uncommitted; original
    exceptions propagate after releasing the serialization lock. Successful
    protected persistence and nonyielding installation form the commit boundary.
    Pending owner cancellation can surface when joining the execution scope after
    that commit; persisted and installed state still agree.
    Prepared hooks, and persistent Legacy hooks with a handler, defensively copy
    validated data state before evaluation; module-global/native effects are not
    rolled back. Invocation-scoped hooks do not persist snapshots. *)
val set_snapshot_handler
  :  instance
  -> (Session.Snapshot.t -> (unit, string) result)
  -> unit

(** Project this owner's actual fixed kind-specific runtime metadata once.
    Entry names must be nonempty and at most 1024 bytes; work polls checkpoint.
    Shell hooks require (event,state)->state task with initial_state sharing the
    same grouped state variable. Moderator is a distinct three-argument role and
    rejects here. This static contract grants no runtime installer/authority. *)
val compiler_contract
  :  Chatmd_shell_spec.Chatmd_script_spec.kind
  -> entrypoint:string
  -> checkpoint:(unit -> unit)
  -> (Chatml.Chatml_compiler_contract.t, Chatmd_shell_spec.Diagnostic.t) Result.t

(** Preserve exact original hook source and declared digest. Compile the admitted
    graph under its authentic native role using the existing joined worker and
    limits; match original entry bytes inside its elapsed checkpoint. No wrapper
    source is appended. The caller must supply a legitimately admitted graph for
    this entrypoint/contract (selected roles use real captured-only recomposition).
    Successful compilation does not initialize or authorize a shell runtime. *)
val compile_prepared
  :  Chatml_prepared_program.t
  -> script:Chatmd_shell_spec.Chatmd_script_spec.t
  -> entrypoint:string
  -> limits:Chatml_compilation.limits
  -> max_dependency_depth:int
  -> env:Eio_unix.Stdenv.base
  -> unit
  -> (compiled, Chatmd_shell_spec.Diagnostic.t list) Result.t
