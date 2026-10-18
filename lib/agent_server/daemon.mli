open! Core

(** Top-level owner of one durable agent-server instance. Construction does
    not start transport listeners; callers may bind any enabled transport to
    the returned dispatcher. *)

type status =
  | Starting
  | Ready
  | Draining
  | Stopped
  | Failed of Agent_protocol.Error.t
[@@deriving sexp]

type startup_mode =
  | Execute
  | On_demand
  | Operator_only
[@@deriving equal, sexp]

module Developer_configuration : sig
  type t

  module Shell_approvals : sig
    type t

    module Limits : sig
      type t

      val create
        :  max_pending:int
        -> max_request_bytes:int
        -> max_display_bytes:int
        -> max_decision_reason_bytes:int
        -> wait_seconds:float
        -> (t, Agent_protocol.Error.t) result
    end

    (** Opt-in process-local Once approvals for this Developer host.
        Command policies come from the same configuration's existing
        receipt_read/command_control decisions. Job policies are separate
        trusted daemon decisions on a fresh Root-current Job snapshot; a
        pending item or command receipt does not authorize its response. *)
    val create
      :  limits:Limits.t
      -> job_read:(Operator_authorization.t -> Agent_protocol.Developer_job.t -> bool)
      -> job_respond:(Operator_authorization.t -> Agent_protocol.Developer_job.t -> bool)
      -> t
  end

  (** Disabled by default. This configures an owner, not a supplied broker,
      Session grant or Assume_approved provider. *)
  val with_shell_approvals : t -> Shell_approvals.t -> t

  module Remote_content : sig
    type t

    (** Optional daemon policy for exact remote content origins. Absence denies
        child document/image network reads before I/O. Bounds are positive,
        finite and validated at construction. *)
    val create
      :  allowed_origins:string list
      -> max_response_bytes:int
      -> max_wall_seconds:float
      -> t Core.Or_error.t
  end

  module Repl : sig
    type t

    (** Validated, inert REPL actor policy. It selects no new language ABI and
        grants no source, method, Task or native authority. *)
    val create
      :  scope_limits:Chatml_configured_lookup.Repl_scope.Limits.t
      -> root_limits:Chatml_dependency_session.Roots.Limits.t
      -> typing_limits:Chatml_repl_typing.Limits.t
      -> evaluator_limits:Chatml_repl_execution.Limits.t
      -> execution_policy:Chatml_execution.policy
      -> max_instances:int
      -> max_keys_per_instance:int
      -> max_submissions_per_epoch:int
      -> max_source_bytes:int
      -> max_receipt_bytes:int
      -> max_total_receipt_bytes:int
      -> max_task_origins:int
      -> (t, Agent_protocol.Error.t) result
  end

  (** Opt in one process-local REPL actor under the same installed Developer
      host and capture owner. The base configuration leaves REPL disabled. *)
  val with_repl : t -> Repl.t -> t

  module Limits : sig
    type t

    val create
      :  store:Agent_store.Developer_run_store.Limits.t
      -> roots:Developer_root_owner.Limits.t
      -> max_active_commands:int
      -> disclosure:Developer_result_disclosure.Limits.t
      -> compilation:Chatml_compilation.policy
      -> max_dependency_depth:int
      -> execution:Chatml_execution.limits
      -> max_output_bytes:int
      -> receipt:Document_schema.Limits.t
      -> max_agent_sources:int
      -> max_agent_source_work_steps:int
      -> max_native_bindings:int
      -> workspace_cleanup:Agent_session.Workspace_cleanup.Limits.t
      -> max_agent_cache_entries:int
      -> (t, Agent_protocol.Error.t) result

    (** [create] uses eight active synchronous Agent calls by default. Change
        only this developer selected policy through a validated receiver-first
        copy before installing the daemon. The first Saved child counts as one;
        accepted values are 1 through 64. *)
    val with_max_active_agent_calls
      :  t
      -> max_active_calls:int
      -> (t, Agent_protocol.Error.t) result
  end

  (** Trusted daemon policy. The daemon shares its existing child source
      policy, catalogs, Store, Blob owner, workspace lease and scheduler with
      Developer commands. Predicate callbacks are nonyielding and rechecked
      after waits; they grant neither source nor method permission. *)
  val create
    :  capture_policy:Authoring_context_service.Developer.Target_policy.t
    -> capture_limits:Authoring_context_service.Developer.Limits.t
    -> limits:Limits.t
    -> result_disclosure:Developer_result_disclosure.Policy.t
    -> receipt_read:
         (Operator_authorization.t -> Agent_store.Developer_command_row.t -> bool)
    -> command_control:
         (Operator_authorization.t -> Agent_store.Developer_command_row.t -> bool)
    -> publisher_limits:Agent_store.Developer_result_publisher.Limits.t
    -> publisher_storage:Agent_store.Developer_result_publisher.Storage.t
    -> model_target:Inference.Request.Target.t option
    -> authorize_native:
         (Agent_protocol.Host_invocation.t
          -> Chat_response.Tool_capability.binding
          -> (unit, Agent_protocol.Error.t) result)
    -> prepare_output:
         (Openai.Responses.Tool_output.Output.t
          -> (Jsonaf.t, Agent_protocol.Error.t) result)
    -> (t, Agent_protocol.Error.t) result

  (** Attach an explicitly admitted remote child-content policy before daemon
      construction. Omission keeps remote reads disabled. *)
  val with_remote_content : t -> Remote_content.t -> t
end

type options =
  { startup_mode : startup_mode
  ; root_invocation :
      Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.t
    (** Trusted root invocation for newly captured catalog definitions, including
        reload. Defaults to Ui. Retained session Spec remains authoritative; this
        does not reinterpret an existing revision or select a role by digest. *)
  ; implementation_name : string
  ; implementation_version : string
  ; features : string list
  ; authoring : Authoring_installation.Host_policy.t option
    (** [Some] installs one real bounded authoring service. The daemon default
        is supplied-document/core/lexical/local assistance with no native read
        or upload grant; [None] explicitly disables authoring. Optional native
        source, upload and selected semantic policies are trusted host inputs,
        never inferred from feature strings or client request identities. The
        installed handler set, not this options record alone, is advertised. *)
  ; developer : Developer_configuration.t option
    (** Optional callable developer service. Some requires Authoring plus
        Execute or On_demand; None is the default and advertises no developer
        command transport. Wire methods belong to OCH267. *)
  ; extension_host : Agent_protocol.Extension_capabilities.host
  ; protocol_limits : Agent_protocol.Initialize.Limits.t
  ; timing : Agent_protocol.Initialize.Timing.t
  ; factory_limits : Session_factory.limits
  ; seed_recovery_limits : Session_factory.Seed_recovery_limits.t
    (** Positive complete-invocation first-Created/receipt recovery profile shared
        by Execute/On_demand startup and Pending original command reconciliation.
        No runtime/source recapture occurs; limits do not enlarge transport bytes. *)
  ; retained_artifact_limits : Retained_job_artifacts.Limits.t
    (** Aggregate startup-only session/archive/job inventory and collector traversal
        bounds. Used before installing a loader or recovering actors. Existing
        snapshot/journal/archive byte ceilings remain per-file. Incomplete protection
        skips all artifact pruning; it does not weaken ordinary recovery validation. *)
  ; quota_limits : Agent_session.Quota_manager.limits
  ; reviewer_resolver : Catalog_builder.reviewer_resolver option
  ; policy_evaluator_resolver : Catalog_builder.policy_evaluator_resolver option
  ; inference_policy : Session_factory.inference_policy
  ; qualify_chatml_extensions : bool
    (** Enable the shared extension runtime; true by default. False is an explicit
        embedding compatibility override and suppresses extension discovery.
        Individual tools still require ChatMD declarations and normal authority. *)
  ; session_helpers : Agent_session.Session_management_channel.grant list
    (** Trusted host opt-ins for named shell tools using the private helper
        channel. Empty by default. Each grant must validate its helper's actual
        filesystem/environment boundary against this host's credentials/control
        endpoints. Alternatively use [Config.Server.session_helpers]; these two
        grant sources cannot be combined. *)
  ; independent_lifetime_policy : string option
    (** Explicit trusted host policy revision authorizing independent lifetime.
        None denies it. The revision is hashed into private admissions; changing
        it invalidates earlier grants. This is not a model-supplied bearer token
        and does not enable extensions when the host override disables them. *)
  ; chatml_runtime_policy : Chat_response.Runtime_semantics.policy
    (** Initial policy for newly qualified runtimes. Existing sessions retain
        their recorded policy across reload/restart. No CLI/configuration flag. *)
  ; authoring_validation_host : Chat_response.Authoring_validation.host option
    (** Optional target identity/policy for readonly helpers. With
        extensions enabled, None selects the installed native
        targets/catalog. An explicit host retains its target/compiler/source
        restrictions. This does not grant tool execution authority. *)
  ; content_limits : Chatmd_prompt_content.Limits.t option
    (** Fixed host policy for capturing documents during catalog startup/reload
        and restoring them from session artifacts. Capture borrows the catalog's
        original source scope; stored metadata never acquires source authority or
        selects these limits. None rejects captured imports and content-bearing
        stored revisions, while ordinary dynamic imports retain their behavior. *)
  ; script_preparation : Agent_session.Runtime_script_preparation.t option
    (** Host compilation, role planning and semantic graph restoration ceilings.
        Required for owned prompt revisions, including parser-only custody.
        Independent of storage admission and readonly authoring helper policy. *)
  ; provider_operator_factory : Provider_operator_port.factory option
    (** Optional host-owned provider service. Initialize once with actual server
        identity; missing service advertises no provider.operator capability.
        Unconfigured hosts may expose explicit setup/status without inference. *)
  ; oauth_resolver : (string -> Authenticator.bearer_validator option) option
  ; oauth_actor_resolver : (string -> Authenticator.actor_validator option) option
  ; proxy_actor_policy :
      (now:(unit -> Agent_protocol.Timestamp.t)
       -> Authenticator.Request_identity.t
       -> principal:Agent_protocol.Principal.t
       -> (Operator_authorization.t, Agent_protocol.Error.t) result)
        option
  }

type t

module For_testing : sig
  (** Read-only projection of this daemon's actual JobScheduler wait deadline,
      in seconds on the daemon's injected wall clock. The observation grants
      no polling, candidate or claim control. *)
  val job_scheduler_wait_deadline : t -> float option
end

module Developer : sig
  module Configuration = Developer_configuration

  module Approval : sig
    module Pending = Developer_approval_owner.Pending

    type decision = Developer_approval_owner.decision =
      | Approve_once
      | Deny of string

    (** Read only current process-local pending approvals. On restart, ended
        epoch or foreign run the list cannot resurrect an effect. *)
    val pending
      :  t
      -> actor:Operator_authorization.t
      -> run_id:Agent_protocol.Id.Developer_run.t
      -> (Pending.t list, Agent_protocol.Error.t) result

    val respond
      :  t
      -> actor:Operator_authorization.t
      -> run_id:Agent_protocol.Id.Developer_run.t
      -> id:Agent_protocol.Id.Permission.t
      -> decision
      -> (unit, Agent_protocol.Error.t) result

    val cancel
      :  t
      -> actor:Operator_authorization.t
      -> run_id:Agent_protocol.Id.Developer_run.t
      -> id:Agent_protocol.Id.Permission.t
      -> (unit, Agent_protocol.Error.t) result
  end

  module Request : sig
    type t

    module Route : sig
      type t =
        | Local_project of
            { root_index : int
            ; dependency_root_indices : int list
            ; project : Source.Portable_path.t
            ; selection : Chatml_project.Selection.request
            }
        | Local_source of
            { root_index : int
            ; member : Source.Portable_path.t
            }
        | Authoring_project of { expected : Agent_protocol.Authoring.Context.Expected.t }
        | Document_script of
            { expected : Agent_protocol.Authoring.Context.Expected.t
            ; selector : Agent_protocol.Authoring.Analysis.Target.selector
            }
    end

    module Fresh_source : sig
      type t =
        | Local_project of
            { root : Authoring_source_host.Root.t
            ; dependency_roots : Authoring_source_host.Root.t list
            }
        | Local_source of { root : Authoring_source_host.Root.t }
        | Authoring_project of { connection : Connection_context.t }
        | Document_script of { connection : Connection_context.t }
    end

    val create
      :  route:Route.t
      -> kind:Agent_store.Developer_command_receipt.Kind.t
      -> target:Chatml.Chatml_compiler_contract.Target.t
      -> prompt:Agent_protocol.Id.Prompt_definition.t option
      -> workspace:Agent_protocol.Id.Workspace_definition.t option
      -> entrypoint:string
      -> input:Jsonaf.t
      -> fixtures:Chatml_developer_capture.Fixture.t option
      -> requested_execution:Chatml_execution.limits option
      -> requested_max_output_bytes:int option
      -> bounds:Chatml_developer_capture.Json_bounds.t
      -> checkpoint:(unit -> unit)
      -> (t, Agent_protocol.Error.t) result
  end

  module View : sig
    type t

    val id : t -> Agent_store.Developer_command_receipt.Id.t
    val owner : t -> Agent_protocol.Execution_owner.developer
    val kind : t -> Agent_store.Developer_command_receipt.Kind.t
    val phase : t -> Agent_store.Developer_command_row.Phase.t
    val receipt : t -> Agent_store.Developer_command_receipt.t option
    val capture_fingerprint : t -> Chatml_source.Digest.t
    val request_fingerprint : t -> Chatml_source.Digest.t
  end

  (** The daemon checks that this exact host was installed with the single
      serviceable Job_scheduler. There is no transport parsing or independent
      worker queue here. Fresh source capabilities are needed only for a true
      original-key miss; an identical historical retry may omit them. *)
  val submit
    :  t
    -> actor:Operator_authorization.t
    -> run_id:Agent_protocol.Id.Developer_run.t
    -> key:Agent_protocol.Idempotency_key.t
    -> request:Request.t
    -> fresh_source:Request.Fresh_source.t option
    -> (View.t, Agent_protocol.Error.t) result

  val observe
    :  t
    -> actor:Operator_authorization.t
    -> run_id:Agent_protocol.Id.Developer_run.t
    -> key:Agent_protocol.Idempotency_key.t
    -> (View.t option, Agent_protocol.Error.t) result

  val await_terminal
    :  t
    -> actor:Operator_authorization.t
    -> run_id:Agent_protocol.Id.Developer_run.t
    -> key:Agent_protocol.Idempotency_key.t
    -> (View.t, Agent_protocol.Error.t) result

  val cancel_and_wait
    :  t
    -> actor:Operator_authorization.t
    -> run_id:Agent_protocol.Id.Developer_run.t
    -> key:Agent_protocol.Idempotency_key.t
    -> (View.t, Agent_protocol.Error.t) result

  (** Process-local developer REPL over this daemon's authenticated source
      owner. Instances, epochs, revisions and Task handles expire at reset,
      close or daemon restart. The Standard surface keeps Task construction
      inert; executing a Task requires a separate authorized Host action. *)
  module Repl : sig
    module Original : sig
      type t

      val create
        :  route:Request.Route.t
        -> target:Chatml.Chatml_compiler_contract.Target.t
        -> prompt:Agent_protocol.Id.Prompt_definition.t
        -> workspace:Agent_protocol.Id.Workspace_definition.t
        -> requested_execution:Chatml_execution.limits option
        -> requested_max_output_bytes:int option
        -> checkpoint:(unit -> unit)
        -> (t, Agent_protocol.Error.t) result
    end

    module Instance : sig
      type t

      val equal : t -> t -> bool
    end

    module Epoch : sig
      type t

      val equal : t -> t -> bool
    end

    module Revision : sig
      type t

      val equal : t -> t -> bool
    end

    module View : sig
      type t

      val instance : t -> Instance.t
      val epoch : t -> Epoch.t
      val revision : t -> Revision.t
      val run_id : t -> Agent_protocol.Id.Developer_run.t
      val capture_fingerprint : t -> Chatml_source.Digest.t
    end

    module Submission : sig
      type t

      module Diagnostic : sig
        type t

        val message : t -> string
        val location : t -> Chatml_source.Location.t option
      end

      type status =
        | Need_more of
            Chatml.Chatml_parse.Submission.Need_more.reason * Chatml_source.Location.t
        | Rejected of Diagnostic.t
        | Finished
        | Stopped of Diagnostic.t option

      type details =
        | Available
        | Output_limited

      (** [status] preserves the execution outcome even when oversized detail
          is omitted. [completed_statements] counts successful statements;
          a failed final receipt remains visible when its detail fits. *)
      val status : t -> status

      val details : t -> details
      val completed_statements : t -> int
      val revision : t -> Revision.t
      val statements : t -> Chatml_repl_execution.Receipt.t list
      val task_handles : t -> Chatml_repl_execution.Task_handle.t list
    end

    (** Same run/key/original request reuses its authenticated instance without
        recapture; changed payload conflicts. A current actor with the same
        principal/authentication kind must still pass source and method policy. *)
    val create
      :  t
      -> actor:Operator_authorization.t
      -> run_id:Agent_protocol.Id.Developer_run.t
      -> key:Agent_protocol.Idempotency_key.t
      -> original:Original.t
      -> fresh_source:Request.Fresh_source.t option
      -> (View.t, Agent_protocol.Error.t) result

    (** Optimistic epoch/revision and exact-key admission. Need_more and
        Rejected execute no statement; earlier successes survive a later
        failure. Same-key retry never executes again. Unexpected callback
        exceptions preserve their original backtrace for the submitting
        waiter; a bounded partial outcome and consumed key remain observable. *)
    val submit
      :  t
      -> actor:Operator_authorization.t
      -> instance:Instance.t
      -> epoch:Epoch.t
      -> expected_revision:Revision.t
      -> key:Agent_protocol.Idempotency_key.t
      -> source:string
      -> (Submission.t, Agent_protocol.Error.t) result

    val observe
      :  t
      -> actor:Operator_authorization.t
      -> instance:Instance.t
      -> key:Agent_protocol.Idempotency_key.t
      -> (Submission.t option, Agent_protocol.Error.t) result

    val cancel_and_wait
      :  t
      -> actor:Operator_authorization.t
      -> instance:Instance.t
      -> key:Agent_protocol.Idempotency_key.t
      -> (Submission.t, Agent_protocol.Error.t) result

    (** Signal and join the active statement before invalidating this epoch's
        evaluator and Task handles. Reset retains prior key evidence; close
        consumes the reserved final ledger slot even when work keys are full. *)
    val reset_and_wait
      :  t
      -> actor:Operator_authorization.t
      -> instance:Instance.t
      -> expected_epoch:Epoch.t
      -> key:Agent_protocol.Idempotency_key.t
      -> (View.t, Agent_protocol.Error.t) result

    val close_and_wait
      :  t
      -> actor:Operator_authorization.t
      -> instance:Instance.t
      -> key:Agent_protocol.Idempotency_key.t
      -> (unit, Agent_protocol.Error.t) result
  end
end

(** The default inference policy permits storage inspection but refuses target
    capture, target changes and runtime resolution; legacy migration callbacks
    are absent. Executing composition roots must supply an explicit host policy. *)
val default_options : options

val start
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> config:Config.t
  -> tool_dir:string
  -> ?home:string
  -> process_start_identity:string option
  -> ?options:options
  -> ?before_activation:
       (Agent_store.Session_store.t -> (unit, Agent_protocol.Error.t) result)
       (** Trusted composition hook after exclusive root ownership and before
           catalog/runtime construction. Failure closes the owned store. *)
  -> unit
  -> (t, Agent_protocol.Error.t) result

val status : t -> status
val dispatcher : t -> Dispatcher.t
val registry : t -> Session_registry.t
val store : t -> Agent_store.Session_store.t

(** Internal qualified host services; access is not caller authorization. *)
val factory : t -> Session_factory.t

val blob_store : t -> Agent_store.Blob_store.t
val prompts : t -> Agent_session.Prompt_catalog.t

module Shell_inspection_selection : sig
  type t =
    | Current
    | Retained of
        { revision_id : Agent_protocol.Id.Prompt_revision.t
        ; invocation : Agent_session.Runtime_script_preparation.Invocation.t
        }
end

(** Trusted host inspection of one actual catalog-owned captured revision.
    Current selects that definition's actual captured Ready role. Retained uses
    existing definition-affine restoration and a trusted explicit invocation;
    authentic stored binding/contract selection must agree or inspection rejects.
    No current-host role inference or recapture fallback for historical code.
    Host script/content policies remain fixed; filenames/digests never select roles.
    No session, actor, initializer, native tool setup, provider or new source
    acquisition. This query does not grant execution and is not an unauthenticated
    transport RPC. The explicit Operator_only CLI owner can print/sign complete
    requested identity even when its security check rejects. *)
val inspect_prompt_shell
  :  t
  -> definition_id:Agent_protocol.Id.Prompt_definition.t
  -> selection:Shell_inspection_selection.t
  -> checkpoint:(unit -> unit)
  -> ( Chat_response.Agent_runtime.prepared_shell_inspection
       , Agent_protocol.Error.t )
       Result.t

val workspaces : t -> Agent_session.Workspace_catalog.t

(** Transport-compatible unit close facade. The typed Command_handler close
    joins authoring before session detach. A finite failed join is recorded in
    daemon health/status and retains the authoring/source/upload owners for
    shutdown retry; this callback never claims successful cleanup or releases
    a borrowed owner. Unexpected exceptions retain their original backtrace. *)
val close_connection : t -> Connection_context.t -> unit

(** [reload_config] validates and atomically publishes catalog changes.
    Server/listener/storage changes are rejected as restart-required. *)
val reload_config : t -> (Config_diff.t, Config.Diagnostic.t list) result

(** Original-record proof for autonomous operator work. Static authentication
    captures exact expiry. Legacy custom/proxy authentication without an explicit
    actor policy remains usable for ordinary RPC but fails closed for providers. *)
val authenticate_http_actor
  :  t
  -> Authenticator.Request_identity.t
  -> string option
  -> (Operator_authorization.t, Agent_protocol.Error.t) result

(** Authenticates one HTTP bearer credential according to the validated
    listener configuration. [None] is accepted only in explicit development
    anonymous mode. *)
val authenticate_http_bearer
  :  t
  -> string option
  -> (Agent_protocol.Principal.t, Agent_protocol.Error.t) result

(** [authenticate_http] evaluates trusted reverse-proxy identity first, then
    configured static/OAuth bearer validators, then explicit development
    anonymous mode. *)
val authenticate_http
  :  t
  -> Authenticator.Request_identity.t
  -> string option
  -> (Agent_protocol.Principal.t, Agent_protocol.Error.t) result

(** [import_legacy t ~principal ~source_id ~source_path ~legacy request]
    imports and registers a stopped legacy session without opening a client
    transport. *)
val import_legacy
  :  t
  -> principal:Agent_protocol.Principal.t
  -> source_id:string
  -> source_path:string
  -> legacy:Session.t
  -> Agent_protocol.Session.Create_request.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Returns the current daemon health projection without requiring an RPC
    connection. *)
val health : t -> include_details:bool -> Agent_protocol.Health.Response.t

(** Stops accepting semantic work, terminates loaded sessions, and releases
    the durable data-root lock. It is idempotent.
    Close new scheduler admission and cancel running jobs, allowing already-admitted
    timer and job-completion callbacks to finish within the configured shutdown
    grace. Grace expiry cancels runtimes normally; interrupted handler receipts
    remain non-replayable. Actor/resource cleanup always runs under cancellation
    protection and can outlive that grace. *)
val shutdown : t -> (unit, Agent_protocol.Error.t) result

(** Explicit trusted-host execution selection, requiring current retained principal
    visibility and owner scope plus exact canonical/lifecycle anchor. Does not imply
    session.start; Operator_only rejects it. Reads/replay never invoke it. *)
val select_session
  :  t
  -> principal:Agent_protocol.Principal.t
  -> expected:Agent_protocol.Session_lifecycle.Expected.t
  -> (Session_registry.entry, Agent_protocol.Error.t) Result.t
