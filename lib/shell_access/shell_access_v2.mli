open! Core

(** Policy-driven, capability-constrained process execution for agent runtimes.

    Policy classification is deliberately separate from confinement. An
    executable is resolved and fingerprinted, its likely effects are analyzed,
    a complete immutable plan is authorized, and a backend then executes that
    exact plan. *)

module Command : sig
  type t = private
    { program : string
    ; arguments : string list
    }

  val create : string -> string list -> t
  val equal : t -> t -> bool
  val basename : t -> string
  val to_argv : t -> string list
  val to_string : t -> string
end

module Chain : sig
  type condition =
    | Always
    | On_success
    | On_failure

  type pipeline = Command.t list

  type t = private
    { first : pipeline
    ; rest : (condition * pipeline) list
    }

  type parse_error =
    { offset : int
    ; message : string
    }

  val single : Command.t -> t
  val create : first:pipeline -> rest:(condition * pipeline) list -> (t, string) result
  val commands : t -> Command.t list
  val parse : string -> (t, parse_error) result
  val parse_error_to_string : parse_error -> string
end

module Input : sig
  (** Bounded input supplied to the first command in a request. *)
  type t =
    | Empty
    | Text of string
    | Bytes of Bigstring.t

  (** [byte_length t] returns the exact number of supplied bytes. *)
  val byte_length : t -> int

  (** [sha256 t] returns the input digest, or [None] for [Empty]. *)
  val sha256 : t -> string option
end

module Limits : sig
  type t =
    { wall_time_seconds : float
    ; idle_time_seconds : float option
    ; max_stdin_bytes : int
    ; max_stdout_bytes : int
    ; max_stderr_bytes : int
    ; max_total_bytes : int
    ; cpu_seconds : int option
    ; memory_bytes : int option
    ; file_size_bytes : int option
    ; open_files : int option
    }

  val default : t
end

module Path_util : sig
  (** [canonical ~fs path] resolves an existing absolute path, including every
      symbolic link component. *)
  val canonical : fs:Eio.Fs.dir_ty Eio.Path.t -> string -> string
end

module Capabilities : sig
  type sandbox =
    | Required
    | Preferred
    | Direct_unsafe

  type t =
    { read_roots : string list
    ; write_roots : string list
    ; network : bool
    ; allow_child_processes : bool
    ; allow_arbitrary_code : bool
    ; allow_privilege_change : bool
    ; sandbox : sandbox
    }

  val read_only : roots:string list -> t
  val development : workspace:string -> t
end

module Executable : sig
  type fingerprint =
    { device : int
    ; inode : int
    ; mode : int
    ; uid : int
    ; gid : int
    ; size : int64
    ; mtime : float
    ; sha256 : string
    }

  type t =
    { requested : string
    ; path : string
    ; canonical_path : string
    ; trusted : bool
    ; fingerprint : fingerprint
    }

  val same_fingerprint : fingerprint -> fingerprint -> bool
end

module Resolver : sig
  type t

  type pinned =
    { path : string
    ; sha256 : string option
    ; trusted : bool
    }

  val create
    :  ?trusted_roots:string list
    -> ?search_path:string list
    -> ?executables:(string * pinned) list
    -> unit
    -> t

  val resolve
    :  t
    -> fs:Eio.Fs.dir_ty Eio.Path.t
    -> cwd:string
    -> environment:string array
    -> Command.t
    -> (Executable.t, string) result

  val verify : fs:Eio.Fs.dir_ty Eio.Path.t -> Executable.t -> (unit, string) result
end

module Effect : sig
  type kind =
    | Read_path of string
    | Write_path of string
    | Network
    | Child_processes
    | Arbitrary_code
    | Privilege_change
    | Unknown of string

  type t = kind list

  val analyze : raw_shell:bool -> cwd:string -> Command.t -> t
  val requires_arbitrary_code : t -> bool

  val check_capabilities
    :  Capabilities.t
    -> fs:Eio.Fs.dir_ty Eio.Path.t
    -> cwd:string
    -> t
    -> (unit, string) result

  val to_strings : t -> string list
end

module Context : sig
  type origin =
    | Tool
    | Moderator
    | Host of string

  type request_kind =
    | Structured
    | Script_file
    | Raw_shell

  type stdin_kind =
    | Empty
    | Pipeline
    | Supplied

  type t =
    { request_id : string
    ; runtime_id : string
    ; manifest_sha256 : string
    ; command : Command.t
    ; executable : Executable.t
    ; cwd : string
    ; environment : string array
    ; request_kind : request_kind
    ; stdin_kind : stdin_kind
    ; stdin_sha256 : string option
    ; stdin_bytes : int
    ; script_sha256 : string option
    ; script_preview : string option
    ; origin : origin
    ; effects : Effect.t
    ; capabilities : Capabilities.t
    ; policy_action : string option
    ; policy_matches : string list
    ; session_id : string option
    }
end

module Analyzer : sig
  type result =
    | Add of Effect.t
    | Replace of Effect.t

  type t = Context.t -> (result, string) Result.t
end

module Matcher : sig
  type t

  val any : t
  val program : string -> t
  val basename : string -> t
  val resolved_path : string -> t
  val trusted_executable : t
  val program_regex : string -> (t, string) result
  val argv_prefix : string list -> t
  val argument : string -> t
  val argument_contains : string -> t
  val has_effect : (Effect.kind -> bool) -> t
  val no_unknown_effects : t
  val request_kind : Context.request_kind -> t
  val all : t list -> t
  val any_of : t list -> t
  val negate : t -> t
  val custom : (Context.t -> bool) -> t
  val matches : t -> Context.t -> bool
end

module Policy : sig
  type action =
    | Allow
    | Ask
    | Deny

  type rule
  type t

  type match_ =
    { rule_id : string
    ; action : action
    ; reason : string option
    }

  type decision =
    { action : action
    ; matches : match_ list
    ; reason : string
    }

  val rule : id:string -> action:action -> ?reason:string -> Matcher.t -> rule
  val create : ?default:action -> rule list -> t
  val conservative_default : unit -> t
  val evaluate : t -> Context.t -> decision
  val string_of_action : action -> string
end

module Secret_filter : sig
  type t

  val create : ?replacement:string -> string list -> t
  val empty : t
  val redact : t -> string -> string
  val redact_command : t -> Command.t -> string
end

module Approval : sig
  type identity =
    { manifest_sha256 : string
    ; runtime_id : string
    ; request_kind : Context.request_kind
    ; command_hash : string
    ; executable_sha256 : string
    ; argv : string list
    ; cwd_sha256 : string
    ; environment_sha256 : string
    ; stdin_sha256 : string option
    ; stdin_bytes : int
    ; script_sha256 : string option
    }

  type scope =
    | Once
    | Exact_session of { expires_at : float option }
    | Prefix_session of
        { prefix : string list
        ; expires_at : float option
        }
    | Durable_exact of { expires_at : float option }

  type request =
    { context : Context.t
    ; policy : Policy.decision
    ; identity : identity
    ; display_command : string
    ; rationale : string option
    }

  type response =
    | Approve
    | Approve_for of scope
    | Deny of string
    | Rewrite of Command.t

  type reviewer_metadata =
    { reviewer_id : string
    ; reviewer_kind : string
    ; model : string option
    ; input_tokens : int option
    ; output_tokens : int option
    ; latency_ms : int option
    }

  type review =
    { response : response
    ; metadata : reviewer_metadata option
    }

  type reviewer = request -> response
  type reviewer_with_metadata = request -> review
  type store

  (** [create_store] constructs the executor-facing approval boundary. When
      callbacks are omitted it uses a process-local memory store. Callback
      errors fail closed and are reported as execution errors. *)
  val create_store
    :  ?lookup:(now:float -> session_id:string option -> identity -> (bool, string) result)
    -> ?remember:
         (session_id:string option
          -> identity
          -> scope
          -> reviewer_metadata option
          -> (unit, string) result)
    -> unit
    -> store

  val prompt : request -> string
  val response_of_json : string -> (response, string) result
  val reviewer_of_llm : complete:(string -> (string, string) result) -> reviewer
end

module Interceptor : sig
  type command_result =
    { command : Command.t
    ; executable : Executable.t option
    ; status : Eio.Process.exit_status
    ; stdout : string
    ; stderr : string
    ; stdout_truncated : bool
    ; stderr_truncated : bool
    ; intercepted_by : string option
    ; untrusted_output : bool
    }

  type before =
    | Continue
    | Rewrite of Command.t
    | Respond of command_result
    | Reject of string

  type kind =
    | Trusted_substitute
    | Output_filter

  type t

  val trusted_substitute : name:string -> before:(Context.t -> before) -> t
  val output_filter : name:string -> after:(command_result -> command_result) -> t
  val name : t -> string
  val kind : t -> kind
end

module Audit : sig
  type termination =
    | Timed_out of float
    | Idle_timed_out of float
    | Output_limit_exceeded of int
    | Cancelled

  type event =
    | Resolved of Context.t
    | Policy_decided of Context.t * Policy.decision
    | Approval_requested of Approval.request
    | Approval_answered of Approval.request * string
    | Reviewer_completed of Approval.request * Approval.reviewer_metadata * string
    | Intercepted of string * Context.t
    | Plan_created of string * string * Context.t
    | Started of string * Context.t * int option
    | Output of string * Context.t * [ `Stdout | `Stderr ] * int
    | Finished of string * Context.t * Eio.Process.exit_status
    | Terminated of string * Context.t * termination
    | Rejected of Context.t * string

  type envelope =
    { sequence : int64
    ; timestamp : float
    ; session_id : string option
    ; runtime_id : string
    ; manifest_sha256 : string
    ; request_id : string
    ; plan_id : string option
    ; event : event
    ; dropped_fields : String.Set.t
    ; replacement_fields : string String.Map.t
    }

  type failure_policy =
    | Ignore_failure
    | Deny_start
    | Terminate_runtime

  type t

  val create : failure_policy:failure_policy -> (envelope -> (unit, string) result) -> t
  val filter : t -> (envelope -> (envelope option, string) result) -> t
  val write : t -> envelope -> (unit, string) result
  val failure_policy : t -> failure_policy
  val ignore : t
  val context : event -> Context.t
end

module Execution_plan : sig
  type t =
    { id : string
    ; context : Context.t
    ; limits : Limits.t
    ; environment : string array
    ; cwd : string
    ; resource_runner : Executable.t option
    }
end

module Backend : sig
  type confinement =
    | Verified
    | Declared
    | Unconfined

  type spawn =
    { executable : string
    ; argv : string list
    ; environment : string array
    }

  type simulated =
    { status : Eio.Process.exit_status
    ; stdout : string
    ; stderr : string
    }

  type repeated_flag = { flag : string }

  type atom =
    | Literal of string
    | Cwd
    | Target_executable
    | Command_argv
    | Read_roots of repeated_flag
    | Write_roots of repeated_flag
    | Network_flag of string
    | Resource_limit_args

  type t

  val name : t -> string
  val confinement : t -> confinement
  val availability : t -> fs:Eio.Fs.dir_ty Eio.Path.t -> (unit, string) result
  val available : t -> fs:Eio.Fs.dir_ty Eio.Path.t -> bool
  val direct : t
  val macos_seatbelt : t
  val linux_bubblewrap : ?executable:string -> unit -> t

  val external_
    :  name:string
    -> wrapper:Executable.t
    -> confinement:confinement
    -> accept_declared_confinement:bool
    -> atom list
    -> (t, string) result

  val fake
    :  name:string
    -> (Execution_plan.t -> stdin:string -> (simulated, string) result)
    -> t

  module For_testing : sig
    val prepare
      :  t
      -> fs:Eio.Fs.dir_ty Eio.Path.t
      -> Execution_plan.t
      -> (spawn, string) result
  end
end

module Request : sig
  type raw_shell =
    { executable : string
    ; arguments_before_script : string list
    ; script : string
    }

  type script_file =
    { command : Command.t
    ; path : string
    ; source_sha256 : string
    ; executable_sha256 : string
    ; max_source_bytes : int
    }

  type t =
    | Structured of Chain.t
    | Script_file of script_file
    | Raw_shell of raw_shell

  val command : Command.t -> t
  val custom_tool : command_line:string -> arguments:string list -> (t, string) result

  val script_file
    :  executable:string
    -> arguments:string list
    -> path:string
    -> source_sha256:string
    -> executable_sha256:string
    -> max_source_bytes:int
    -> t

  val raw_shell : ?arguments_before_script:string list -> executable:string -> string -> t
end

module Executor : sig
  type config

  type invocation =
    { request : Request.t
    ; input : Input.t
    ; rationale : string option
    ; origin : Context.origin
    }

  type error =
    | Permission_required of Approval.request
    | Denied of string
    | Resolution_error of string
    | Capability_violation of string
    | Sandbox_unavailable of string
    | Interceptor_rejected of string
    | Spawn_error of string
    | Executable_changed of string
    | Script_changed of string
    | Audit_unavailable of string
    | Timed_out of float
    | Idle_timed_out of float
    | Stdin_limit_exceeded of int
    | Output_limit_exceeded of int

  type result =
    { request_id : string
    ; commands : Interceptor.command_result list
    ; status : Eio.Process.exit_status
    ; stdout : string
    ; stderr : string
    ; backend : string
    }

  val config
    :  env:Eio_unix.Stdenv.base
    -> runtime_id:string
    -> manifest_sha256:string
    -> policy:Policy.t
    -> capabilities:Capabilities.t
    -> ?resolver:Resolver.t
    -> ?reviewer:Approval.reviewer
    -> ?reviewer_with_metadata:Approval.reviewer_with_metadata
    -> ?approval_store:Approval.store
    -> ?administrative_check:(Context.t -> (unit, string) Result.t)
    -> ?analyzers:Analyzer.t list
    -> ?interceptors:Interceptor.t list
    -> ?backends:Backend.t list
    -> ?cwd:Eio.Fs.dir_ty Eio.Path.t
    -> ?process_env:string array
    -> ?limits:Limits.t
    -> ?resource_runner:string
    -> ?secret_filter:Secret_filter.t
    -> ?audit:Audit.t
    -> ?audit_sequence:int Atomic.t
    -> ?session_id:string
    -> ?pipefail:bool
    -> unit
    -> config

  (** [run config invocation] authorizes and executes [invocation]. Supplied
      input is bounded and included in approval identity before execution. *)
  val run : config -> invocation -> (result, error) Result.t

  val error_to_string : error -> string
end
