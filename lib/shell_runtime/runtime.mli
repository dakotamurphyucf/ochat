open! Core

(** One instantiated shell runtime and its immutable executor configuration. *)

type executable =
  { path : string
  ; sha256 : string option
  ; trusted : bool
  }
[@@deriving sexp, compare, equal]

type t

type model_completion =
  agent:string -> model:string option -> Model_reviewer.complete option

type file_identity =
  { path : string
  ; verification_path : string
  ; sha256 : string
  ; size : int
  }
[@@deriving sexp, compare, equal]

type error =
  { runtime_id : string
  ; code : string
  ; message : string
  }
[@@deriving sexp, compare, equal]

(** [create] resolves and validates one effective runtime using only [host]
    capabilities, then constructs its [Shell_access.Executor.config]. Trusted
    registry staging may supply a pending-instance factory and an execution
    check. The check composes into the executor's early scope guard, before
    analyzers or other callbacks, including borrowed configurations. Omitting
    these arguments preserves immediate instance initialization/execution. *)
val create
  :  sw:Eio.Switch.t
  -> host:Host.t
  -> manifest:Chatmd_shell_spec.Manifest.t
  -> platform:Chatmd_shell_spec.Shell_spec.platform
  -> approval_provider:Approval_broker.provider
  -> ?admin_policy:Admin_policy.t
  -> ?approval_store:Shell_access.Approval.store
  -> ?audit_sequence:int Atomic.t
  -> ?extensions:Chatml_extension.compiled String.Map.t
  -> ?runtime_instances:Chatml_extension.instance String.Table.t
  -> ?register_extension_instance:
       (runtime_id:string
        -> lifecycle:Chatmd_shell_spec.Shell_spec.lifecycle
        -> Chatml_extension.instance
        -> unit)
  -> ?prepare_extension:
       (lifecycle:Chatmd_shell_spec.Shell_spec.lifecycle
        -> Chatml_extension.compiled
        -> (Chatml_extension.instance, Chatmd_shell_spec.Diagnostic.t) result)
  -> ?check_execution:(unit -> (unit, string) result)
  -> ?worker_runtimes:t String.Map.t
  -> ?model_completion:model_completion
  -> Chatmd_shell_spec.Shell_spec.t
  -> (t, error list) result

val id : t -> string
val spec : t -> Chatmd_shell_spec.Shell_spec.t
val executor_config : t -> Shell_access.Executor.config

(** Overlay verified native caller identity/grants on the original configuration.
    Expired caller services reject instead of using the registration owner's
    approval store. Other resource, policy and implementation identities remain. *)
val executor_config_for_call : t -> (Shell_access.Executor.config, string) result

(** [redact t value] applies the runtime's configured secret filter. *)
val redact : t -> string -> string

val max_stdin_bytes : t -> int
val executable : t -> string -> executable option
val environment_value : t -> string -> string option

val resolve_path
  :  t
  -> source:Chatmd_shell_spec.Source_ref.t
  -> Chatmd_shell_spec.Path_expr.t
  -> (string, Host.error) result

(** [resolve_executable t program] resolves and fingerprints [program] using
    the runtime's configured resolver. *)
val resolve_executable : t -> string -> (Shell_access.Executable.t, string) result

(** [load_file t ~source ~max_bytes path] loads and fingerprints a bounded
    regular file through Eio. *)
val load_file
  :  t
  -> source:Chatmd_shell_spec.Source_ref.t
  -> max_bytes:int
  -> Chatmd_shell_spec.Path_expr.t
  -> (file_identity, Host.error) result

module For_testing : sig
  val matcher_failure
    :  Chatmd_shell_spec.Shell_spec.policy_action
    -> Chatmd_shell_spec.Shell_spec.hook_failure
    -> bool

  val mutable_audit_field : string -> bool

  val replacement_fields
    :  Shell_access.Secret_filter.t
    -> string
    -> (string String.Map.t, string) result
end

(** Trusted registry plumbing after complete native-footprint/grant validation.
    Build new pure code closures and immutable worker-config wrappers using this
    runtime's retained environment/resolver/backends/executables/raw audit sink
    and services. No native resource lookup or acquisition occurs. The original
    source bases govern native path expressions; [specification.source] is the
    new code's diagnostic provenance, not fresh path authority.

    The registry owns [prepare_extension], [runtime_instances] and registration
    through activation/failure cleanup. [check_execution] composes with original
    resource liveness. The borrowed checkpoint polls before retaining new closure
    structures and propagates unchanged. This low-level operation is not a grant
    constructor; callers must use [Registry.rebind_code] for code-view admission. *)
val rebind_code
  :  t
  -> manifest:Chatmd_shell_spec.Manifest.t
  -> specification:Chatmd_shell_spec.Shell_spec.t
  -> extensions:Chatml_extension.compiled String.Map.t
  -> runtime_instances:Chatml_extension.instance String.Table.t
  -> register_extension_instance:
       (runtime_id:string
        -> lifecycle:Chatmd_shell_spec.Shell_spec.lifecycle
        -> Chatml_extension.instance
        -> unit)
  -> prepare_extension:
       (lifecycle:Chatmd_shell_spec.Shell_spec.lifecycle
        -> Chatml_extension.compiled
        -> (Chatml_extension.instance, Chatmd_shell_spec.Diagnostic.t) Result.t)
  -> worker_runtimes:t String.Map.t
  -> check_execution:(unit -> (unit, string) Result.t)
  -> checkpoint:(unit -> unit)
  -> (t, error list) Result.t

(** Add a borrowed execution admission check without changing code, resource
    ownership or native material. Used by the registry's scoped projection. *)
val with_execution_check : t -> check:(unit -> (unit, string) Result.t) -> t
