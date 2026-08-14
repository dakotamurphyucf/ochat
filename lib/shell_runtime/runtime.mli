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
    capabilities, then constructs its [Shell_access.Executor.config]. *)
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
  -> ?worker_runtimes:t String.Map.t
  -> ?model_completion:model_completion
  -> Chatmd_shell_spec.Shell_spec.t
  -> (t, error list) result

val id : t -> string
val spec : t -> Chatmd_shell_spec.Shell_spec.t
val executor_config : t -> Shell_access.Executor.config
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
