open! Core
module Source = Runtime_source_preparation

(** Constructs the live ChatMD runtime shared by daemon and embedded session
    hosts. All filesystem coordinates are explicit runtime paths. *)

type moderator_drain =
  { moderator_snapshot : Jsonaf.t option
  ; runtime_requests : Chat_response.Moderation.Runtime_request.t list
  ; notifications : string list
  ; remaining_events : bool
  }

type model_job_outcome =
  | Model_succeeded of Jsonaf.t
  | Model_failed of string

(** Native resources and compiled, non-evaluated extension definitions. No
    operation worker, moderator manager, history or actor service is installed. *)
type resources = private
  { source_lease : Prompt_source_lease.t
  ; native : Chat_response.Agent_runtime.t
  ; definition : Chat_response.Extension_compiler.definition option
  ; managed : Chat_response.Managed_tool_registry.t option
  ; authoring : Authoring_runtime.t option
  }

(** Explicit host compilation/planning/restoration policy, shared by resource
    preparation and full runtime construction. *)
module Script_preparation = Runtime_script_preparation

(** On [prepare_resources], [prepare_authored_resources], [build] and
    [build_with_extensions], [startup = Some context] borrows the host's exact
    original compilation limits/deadline for joined preparation. [None] starts an
    independent preparation. Returned resources/runtime never retain the context;
    activation follows complete preflight outside the compilation timer. *)

(** Reconstruct an authored revision's resources using the same native/extension
    registration path as normal runtime construction, including contextual one-off
    and validation helpers. Verify the stored source tree first. Does not convert
    prompt messages, reserve history or invoke a moderator/model. Owned parser/
    program custody requires [Some script_preparation], including parser-only
    revisions. Planning uses the original parser before helper augmentation;
    native resources consume its admitted document projection. Compilation and
    restoration share one startup deadline. [root_invocation] is the explicitly
    retained root contract: both captured-role restoration and native legacy
    compilation use it; neither source nor the current frontend selects a role.
    After complete preflight, native shell hooks may activate under their separate
    execution controls. Authorized native
    setup (including shell policy checks and MCP connections) can perform IO;
    this is not the no-effect authoring validator.


    Resources belong to [sw]. The host must retain it through every borrower and
    release it after failed preparation. This does not authorize delegation:
    selection, current authority and owner-aware parent policy remain required.
    Inherited stateful managed tools must still reject unavailable delegation.
    Caller supplies the ancestor's original admitted paths, not a child's broader
    roots. Independent generated-ancestor reconstruction remains a separate step.
    [native_registrations] supplies host-admitted authored wrappers alongside the
    standard helpers. Explicit declarations select them; duplicate/mismatched
    registrations reject. An empty list supplies no additional implementations. *)
val prepare_resources
  :  startup:Chat_response.Startup_compilation.t option
  -> agent_source:Captured_agent_runner.Source_binding.t option
  -> root_invocation:
       Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.t
  -> script_preparation:Script_preparation.t option
  -> native_registrations:Chat_response.Agent_runtime.native_registration list
  -> native_service_revision:string option
  -> env:Eio_unix.Stdenv.base
  -> inference_context:Inference_runtime.Context.t
  -> inference_identity:Chat_response.Neutral_turn.Identity.t
  -> on_inference_attempt:(Inference_runtime.Attempt.t -> unit)
  -> on_inference_completion:(Inference_client.Completion.t -> unit)
  -> on_inference_observation:(Inference.Observation.t -> unit)
  -> sw:Eio.Switch.t
  -> paths:Runtime_paths.t
  -> storage_paths:Runtime_paths.t
  -> revision:Prompt_revision.t
  -> session_id:Agent_protocol.Id.Session.t
  -> one_off_policy:Chat_response.One_off_request.policy
  -> authoring_validation_host:Chat_response.Authoring_validation.host option
  -> manifest_authorizer:Shell_runtime.Manifest_authorizer.t
  -> approval_provider:Shell_runtime.Approval_broker.provider
  -> approval_store:Shell_access.Approval.store
  -> (resources, Agent_protocol.Error.t) result

type authored_resources = private
  { source : Authored_agent_source.t
  ; revision : Prompt_revision.t
  ; resources : resources
  ; agent_source : Captured_agent_runner.Source_binding.t option
  ; code_authorizer : Prompt_revision.t -> Shell_runtime.Manifest_authorizer.t
  }

(** The actual constructor's original source loan and root-authorized specialist
    manifest policy. These borrow the same existing resource lifetime. Selected
    code never derives new authority from its artifact directory, source metadata
    or a nested private Root. The authorizer checks the selected complete prepared
    execution identity through the original live grant store and host policy. *)
val authored_source_binding
  :  authored_resources
  -> Captured_agent_runner.Source_binding.t option

val authored_code_authorizer
  :  authored_resources
  -> Prompt_revision.t
  -> Shell_runtime.Manifest_authorizer.t

(** Prepare the named specialist's private resources from its defining parent's
    captured source tree. Retains the authored parser, source-relative paths and
    host-supplied workspace/tool/storage capabilities. Only [prompt_dir] is re-rooted
    to the specialist's captured source directory. Does not prepare/expose the
    parent's public tools or install an artifact/session. Script preparation and
    native hook activation follow [prepare_resources]. [content_limits] explicitly
    bounds retained document restoration; no implicit content policy is selected.

    Native setup follows [prepare_resources], including explicit shell admission
    and MCP connection effects. The host must authorize this private closure and
    retain [sw] through its consumers' lifetimes. The returned preparation revision
    is not a persisted child identity. Invocation mediation, wrapper binding and
    durable creation still belong to the owning service. Moderators and their
    handlers compile against the delegated tool-mediated surface; legacy scripts
    and direct Process/Model recipes reject before any initializer runs.
    [native_registrations] can supply already admitted nested authored wrappers;
    it does not prepare their resources or authorize recursive delegation.
    [manifest_authorizer] receives the captured specialist revision so the host
    can bind admission to its private shell manifest, while retaining the original
    root's operator/workspace/principal authority. It must not grant execution
    approvals or derive authority from model-supplied arguments. *)
val prepare_authored_resources
  :  startup:Chat_response.Startup_compilation.t option
  -> agent_source:Captured_agent_runner.Source_binding.t option
  -> script_preparation:Script_preparation.t option
  -> content_limits:Chatmd_prompt_content.Limits.t option
  -> native_registrations:Chat_response.Agent_runtime.native_registration list
  -> parent_revision:Prompt_revision.t
  -> tool_name:string
  -> native_service_revision:string option
  -> env:Eio_unix.Stdenv.base
  -> inference_context:Inference_runtime.Context.t
  -> inference_identity:Chat_response.Neutral_turn.Identity.t
  -> on_inference_attempt:(Inference_runtime.Attempt.t -> unit)
  -> on_inference_completion:(Inference_client.Completion.t -> unit)
  -> on_inference_observation:(Inference.Observation.t -> unit)
  -> sw:Eio.Switch.t
  -> paths:Runtime_paths.t
  -> storage_paths:Runtime_paths.t
  -> session_id:Agent_protocol.Id.Session.t
  -> one_off_policy:Chat_response.One_off_request.policy
  -> authoring_validation_host:Chat_response.Authoring_validation.host option
  -> manifest_authorizer:(Prompt_revision.t -> Shell_runtime.Manifest_authorizer.t)
  -> approval_provider:Shell_runtime.Approval_broker.provider
  -> approval_store:Shell_access.Approval.store
  -> (authored_resources, Agent_protocol.Error.t) result

(** Build the exact role-selected source previously admitted by
    [Authored_source_preparation.with_prepared], without recapture or role
    selection. Nested wrappers must have been planned against that same revision
    owner. [startup] borrows the original invocation across capture and all native
    compilation; incompatible original policy limits reject before resource
    effects. It never escapes. Native setup, approval and lifetime rules are those
    of [prepare_authored_resources]. *)
val prepare_authored_resources_from_source
  :  Authored_source_preparation.t
  -> startup:Chat_response.Startup_compilation.t
  -> agent_source:Captured_agent_runner.Source_binding.t option
  -> native_registrations:Chat_response.Agent_runtime.native_registration list
  -> native_service_revision:string option
  -> env:Eio_unix.Stdenv.base
  -> inference_context:Inference_runtime.Context.t
  -> inference_identity:Chat_response.Neutral_turn.Identity.t
  -> on_inference_attempt:(Inference_runtime.Attempt.t -> unit)
  -> on_inference_completion:(Inference_client.Completion.t -> unit)
  -> on_inference_observation:(Inference.Observation.t -> unit)
  -> sw:Eio.Switch.t
  -> paths:Runtime_paths.t
  -> storage_paths:Runtime_paths.t
  -> session_id:Agent_protocol.Id.Session.t
  -> one_off_policy:Chat_response.One_off_request.policy
  -> authoring_validation_host:Chat_response.Authoring_validation.host option
  -> manifest_authorizer:(Prompt_revision.t -> Shell_runtime.Manifest_authorizer.t)
  -> approval_provider:Shell_runtime.Approval_broker.provider
  -> approval_store:Shell_access.Approval.store
  -> (authored_resources, Agent_protocol.Error.t) result

(** Narrow an already prepared ancestor's exact resources using an admitted
    generated definition. Shares native implementations and captures standalone
    private dependency closures without initializing any ancestor/child scripts.
    Stateful managed dependencies reject. This constructs no independent scope:
    the root resource switch must outlive the whole inherited chain. Restoration
    must first rebind stored capability pins to the parent's fresh resources via
    Generated_definition.restore. Current delegation/lifetime authority checks and
    policy mediation remain the host's responsibility. *)
val inherit_resources
  :  parent:resources
  -> definition:Generated_definition.t
  -> (resources, Agent_protocol.Error.t) result

(** Host preparation for external queue ingress. Compare [before] and save
    [snapshot] atomically with the delivery receipt under actor ownership. An
    error leaves the live queue unchanged; success must mean durable acceptance. *)
type prepare_enqueue =
  before:Session.Moderator_state.Identity_snapshot.t
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> (unit, Agent_protocol.Error.t) result

type extension_services =
  { runtime_policy : Chat_response.Runtime_semantics.policy
    (** Captured host policy shared by foreground and idle execution. *)
  ; native_service_revision : string option
    (** Host native-service grant identity in resource fingerprints. Resource-only
        restoration must use the same identity; changing it invalidates old
        delegated bindings rather than silently granting new operations. *)
  ; script_tools : Chat_response.Agent_runtime.t -> Script_tool_calls.t
    (** Bind shared native policy/disclosure to the exact constructed runtime.
        This service owns generic native tool approval; delegated shell tools
        still use the authorized shell runtime's policy and approval broker. *)
  ; standalone_execution_limits :
      Chat_response.Extension_compiler.t -> Chatml_execution.limits
    (** Host-selected execution policy for captured standalone scripts. *)
  ; one_off_policy : Chat_response.One_off_request.policy
    (** Host ceiling for explicitly declared run_chatml. Supplying policy never
        adds the tool to a document that did not declare it. *)
  ; authoring_validation_host : Chat_response.Authoring_validation.host option
    (** Optional readonly-helper target identity/policy. Qualified construction
        supplies installed defaults when omitted. An explicit host retains its
        target/compiler/source restrictions; retrieval grants no effect authority. *)
  ; initial_agent_guidance : Chat_response.Agent_guidance.t option
    (** Only genuine constructor initialization; never a turn-owner fallback. *)
  ; claim_lifecycle : event:Chat_response.Moderation.Event.t -> Moderator_event.claim
    (** Actual running-idle actor ownership. Never manufacture an operation. *)
  ; run_actions :
      Agent_protocol.Moderator_execution.t
      -> (Run_action_service.t option, Agent_protocol.Error.t) result
  ; lifecycle_started : Agent_protocol.Invocation.observer -> bool
    (** Whether the current source/generation has a completed lifecycle receipt.
        An initial prepared checkpoint alone does not mean startup executed. *)
  ; history : unit -> History_entry.t list
  ; standalone_completion :
      tools:Script_tool_calls.t
      -> Agent_protocol.Job.t
      -> (unit, Agent_protocol.Error.t) result
  ; idle_notifications :
      source:Agent_protocol.Invocation.observer option
      -> tools:Script_tool_calls.t
      -> unit
      -> (bool, Agent_protocol.Error.t) result
    (** Current canonical history, read after an actor claim without entering the manager. *)
  ; notification_input :
      source:Agent_protocol.Invocation.observer option
      -> tools:Script_tool_calls.t
      -> operation_id:Agent_protocol.Id.Operation.t
      -> unit
      -> ( Chat_response.In_memory_stream.Safe_point_input.batch
           , Agent_protocol.Error.t )
           result
  ; initial_notification_input :
      source:Agent_protocol.Invocation.observer option
      -> tools:Script_tool_calls.t
      -> operation_id:Agent_protocol.Id.Operation.t
      -> unit
      -> ( Chat_response.In_memory_stream.Safe_point_input.batch
           , Agent_protocol.Error.t )
           result
    (** Host checks the loaded runtime pin, prepares current disclosure and commits
        through the actor at this operation's post-tool input boundary. *)
  }

type moderator_activation =
  { pending : unit -> bool
  ; startup_pending : unit -> bool
  ; run : unit -> (bool, Agent_protocol.Error.t) result
  }

type background_executor =
  { policy : Chat_response.One_off_request.policy
  ; now : unit -> Agent_protocol.Timestamp.t
  ; run :
      job:Agent_protocol.Job.t
      -> deadline:Agent_protocol.Timestamp.t
      -> execute:Native_tool_invocation.executor
      -> moderator_execute:Native_tool_invocation.moderator_executor
      -> claim_event:(event:Chat_response.Moderation.Event.t -> Moderator_event.claim)
      -> is_halted:(unit -> bool)
      -> request:Chat_response.Background_request.t
      -> (Background_execution.result, Agent_protocol.Error.t) result
  }

module Authored_start : sig
  (** Runtime-bound startup DATA, never an admission or permission. Legacy data
      comes only from this runtime's actual host-backed startup/resume ACK;
      extension data retains the actual unconsumed Session_start lifecycle.
      The Actor separately validates the installed physical worker/Manager,
      current full checkpoint/source and its exact held owner. *)
  type t

  type disposition =
    | Legacy_acknowledged
    | Extension_pending
    | No_callback

  val disposition : t -> disposition
  val worker : t -> Operation_worker.t
  val manager : t -> Chat_response.Moderator_manager.t option

  (** Recheck factual eligibility, without authorizing execution. Resumed,
      completed, failed or interrupted extension startup is unavailable. Legacy
      cached ACK data is not compared with later checkpoint bytes. *)
  val check_pending : t -> (unit, Agent_protocol.Error.t) result

  (** Legacy/no-callback data performs no effect. The extension uses its existing
      lifecycle coordinator and bound runtime services with this exact supplied
      Actor claim. Only actual Activated succeeds; Unavailable/Already_active
      reject. Exceptions/cancellation propagate through existing coordinator
      cleanup; no initializer or lifecycle callback is replayed here. *)
  val run
    :  t
    -> claim:(event:Chat_response.Moderation.Event.t -> Moderator_event.claim)
    -> (unit, Agent_protocol.Error.t) result
end

module Runtime_resume : sig
  (** Runtime-bound factual resume data, never admission or permission. The
      Actor separately checks the exact installed worker, Manager, source,
      current checkpoint and existing Run continuation. *)
  type t

  val worker : t -> Operation_worker.t
  val manager : t -> Chat_response.Moderator_manager.t option

  (** Extension data requires this runtime's unconsumed Session_resume lifecycle.
      Legacy data requires its actual host-backed constructor ACK selected from
      a restored moderator snapshot. A runtime without a moderator has no
      callback. Fresh legacy startup and unowned encoding fallback reject;
      acknowledged legacy data is not compared with later checkpoint bytes. *)
  val check_pending : t -> (unit, Agent_protocol.Error.t) result

  (** Recheck factual eligibility. Legacy and absent callbacks perform no effect.
      The extension uses the same lifecycle coordinator and bound services with
      the exact supplied Actor claim; only actual Activated succeeds.
      Unavailable/Already_active reject. Exceptions/cancellation propagate.

      Callback Wait/Finish requests are already durable in that real claim.
      This unit result permits no synthetic Continue: the Actor must inspect
      and honor the current same Run before attaching another Operation. *)
  val run
    :  t
    -> claim:(event:Chat_response.Moderation.Event.t -> Moderator_event.claim)
    -> (unit, Agent_protocol.Error.t) result
end

type t =
  { source_lease : Prompt_source_lease.t
  ; worker : Operation_worker.t
  ; inference_execution : Inference_client.Execution.t
  ; now : unit -> Agent_protocol.Timestamp.t
    (** Host wall clock shared by foreground, idle and delegated moderation and
        persisted workflow deadlines. Monotonic execution limits are separate. *)
  ; parse_user_content :
      id:History_entry.Id.t
      -> Agent_protocol.Session.Message_content.t
      -> (History_entry.t, Agent_protocol.Error.t) result
  ; parse_prepared_input :
      execution:Runtime_input_preparation.Execution.t
      -> tracking:Runtime_input_preparation.Tracking.t
      -> (Runtime_input_preparation.Prepared_input.t, Agent_protocol.Error.t) result
    (** Explicit selected input execution and lexical graph. The host finishes
      the graph before recording the returned DATA; no future Operation or
      constructor graph is borrowed. Typed execution failures are retained and
      cancellation propagates. Ordinary parse_user_content is unchanged. *)
  ; initial_history : History_entry.t list
  ; initial_prompt_entry_count : int
  ; reserved_history_through : int
  ; moderator_snapshot : unit -> Jsonaf.t option
    (** Latest successfully published startup checkpoint. Runtime record wrappers
        share this owner-backed getter; copying the runtime does not freeze an
        earlier checkpoint. Internal-event drains return their own snapshot values. *)
  ; moderator_manager : Chat_response.Moderator_manager.t option
  ; model_recipes :
      Model_job_admission.t
      -> Chat_response.Moderation.Capabilities.model_recipe String.Map.t
  ; moderator_tools : Openai.Responses.Request.Tool.t list
  ; idle_notifications : (unit -> (bool, Agent_protocol.Error.t) result) option
    (** Qualified bounded idle data/wake producer using the pinned actor and
        current selected disclosure services. Called after activation. *)
  ; moderator_script_tools : Script_tool_calls.t option
    (** Host policy/disclosure services for v1 moderator native calls. Normal
        construction leaves this absent until v1 admission is installed. *)
  ; standalone_completion :
      (Agent_protocol.Job.t -> (unit, Agent_protocol.Error.t) result) option
    (** Pinned host adapter for a root standalone Pending job. Admission performs
        checked artifact loading and atomically saves the intent/delivery marker. *)
  ; background_executor : background_executor option
    (** Qualified generic executor. Requires the actor's actual job-attempt native,
        moderator-tool and ordinary-event services. *)
  ; moderator_activation : moderator_activation option
    (** Deferred owned activation after installing the initial checkpoint. *)
  ; automatic_turn_policy : Chat_response.Runtime_semantics.policy option
    (** Enable durable accounting with this exact captured policy at installation.
        Absent for unqualified runtimes. *)
  ; check_execution : (unit -> (unit, Agent_protocol.Error.t) result) option
    (** Delegated authority gate for owner-managed work, including idle callbacks.
        This does not authorize disclosure of retained history or replace leases. *)
  ; ancestor_capabilities :
      (Agent_protocol.Id.Session.t
       -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result)
        option
    (** Internal factory lookup of the retained original ancestor bindings. Allows
        descendants below an independent lifetime to validate against reconstructed
        resources without activating stopped ancestors. Use only under this runtime's
        lease; the lookup supplies resources, not permission to execute them. *)
  ; activity : Runtime_activity.t option
    (** Generated execution belongs to the runtime's construction switch even
        when invoked by a foreground/scheduler fiber from another switch. The
        owner must run direct moderator/event work through this scope too. *)
  ; native_runtime : Chat_response.Agent_runtime.t option
    (** Internal qualified host resource source for delegation. Access only while
        holding the owning runtime lease; this is not permission to invoke tools.
        Absent for unqualified construction. *)
  ; authored_start : unit -> (Authored_start.t, Agent_protocol.Error.t) result
    (** Runtime-bound factual selector. Use [authored_start] on the runtime; fake
        runtimes without actual startup ownership must return unavailable. *)
  ; runtime_resume : unit -> (Runtime_resume.t, Agent_protocol.Error.t) result
    (** Runtime-bound cold resume selector. Loaded reuse never calls this.
        Fake runtimes without actual resume ownership must return unavailable. *)
  ; start_moderator : unit -> (Jsonaf.t option, Agent_protocol.Error.t) result
    (** First startup remains construction-fenced. After an actual host-backed
        legacy startup ACK, repeated calls return that exact acknowledged DATA
        without re-entering its retired construction owner. A Ready legacy
        restoration without Init returns its identity snapshot only; its actual
        Session_resume ACK belongs to installed moderator_activation, joined
        before any foreground callback. This snapshot is not a startup/resume
        receipt. Extension startup snapshots remain construction-fenced. *)
  ; enqueue_internal_event :
      ?prepare:prepare_enqueue
      -> Jsonaf.t
      -> (Jsonaf.t option, Agent_protocol.Error.t) result
    (** Raw event JSON passes existing bounded Session snapshot admission before
        runtime value conversion or [prepare]. Rejection leaves the live moderator
        snapshot/queue unchanged and uses the existing Invalid_state error mapping. *)
  ; legacy_events :
      (max_events:int
       -> history:History_entry.t list
       -> (Chat_response.Moderation.Outcome.t list, Agent_protocol.Error.t) result)
        option
    (** Installed legacy idle adapter: each exact head has its own Event ACK.
        Notifications/request intents are already durable. No foreground
        operation, constructor fallback or provider-before-model hook is exposed.
        None retains explicit standalone embedding behavior. *)
  ; drain_internal_events :
      History_entry.t list -> (moderator_drain, Agent_protocol.Error.t) result
  ; execute_model_job :
      allocate_inference:Inference_allocation.Port.t
      -> inference_context:Inference_runtime.Context.t
      -> capture_recipe_target:
           (Inference.Request.Target.t -> (unit, Agent_protocol.Error.t) result)
      -> recipe:string
      -> payload:Jsonaf.t
      -> (model_job_outcome, Agent_protocol.Error.t) result
  ; enqueue_model_job_completion :
      ?prepare:prepare_enqueue
      -> Agent_protocol.Job.t
      -> (Jsonaf.t option, Agent_protocol.Error.t) result
  ; close : unit -> unit
    (** Caller cancels/drains foreground and borrowed background workers first.
        Closes and joins graph-owned inference channels, then persists cache. *)
  }

(** Select the actual runtime-bound startup DATA. Does not load,
    initialize or admit work; absence is represented as No_callback and does not
    broaden held Run admission. Unacknowledged/non-host legacy startup rejects. *)
val authored_start : t -> (Authored_start.t, Agent_protocol.Error.t) result

val runtime_resume : t -> (Runtime_resume.t, Agent_protocol.Error.t) result

(** Read the pinned runtime's moderator source DATA without loading resources,
    acquiring a Manager gate or granting authority. No manager returns None;
    legacy mediation rejects. The caller must already retain this exact runtime
    and separately validate the current Actor lifecycle/source. *)
val moderation_source
  :  t
  -> (Agent_protocol.Invocation.observer option, Agent_protocol.Error.t) result

type schedule_services = Legacy_callback_runtime.Services.schedules =
  { after_ms : delay_ms:int -> payload:Jsonaf.t -> (string, string) result
  ; cancel : id:string -> (unit, string) result
  }

type job_services = Runtime_model_services.factory

(** One actual callback owner, shared by Tool, Schedule and Model. Saved
    checkpoint is exact acknowledged DATA, not an effect-currentness check.
    [is_halted] checks this same live owner and actual failure/halt state; it does
    not relax interactive permission eligibility. The claim saves notifications
    with its full checkpoint and request intent. No ambient fallback is allowed. *)
type legacy_callback_services = Legacy_callback_runtime.Services.t =
  { jobs : Runtime_model_services.t
  ; schedules : schedule_services
  ; execute : Native_tool_invocation.executor
  ; executing : Agent_protocol.Moderator_execution.t
  ; check_current : unit -> (unit, Agent_protocol.Error.t) result
  ; is_halted : unit -> bool
  ; checkpoint : unit -> (Jsonaf.t option, Agent_protocol.Error.t) result
  }

type legacy_callback_claim = Legacy_callback_runtime.claim

(** Lifecycle selection DATA, never callback authority. [Constructor] requires
    the actual Init callback owner. [Installed_resume] is only for an initialized
    legacy checkpoint restored without an Init owner; its Session_resume waits
    for the exact runtime worker/Manager installation and the ordinary installed
    callback claim. Missing original custody stays unavailable. *)
module Legacy_startup : sig
  type t =
    | Constructor
    | Installed_resume
end

(** Constructor and installed claims have separate exact lifetimes. [claim]
    requires the actual initialization owner. [installed_claim] requires the
    supplied operation or eligible idle boundary/Queue_head and must never reuse constructor
    services or its additional construction checkpoint. Installed Model services
    use the supplied actual admission port; Tool and Model effects retain their
    exact callback and execution checks. [history] reads the actual Actor's current
    canonical history before native-tool boundary moderation; the later exact callback claim still checks
    current ownership. Host-backed legacy foreground and idle adapters use these
    services one acknowledged callback at a time. *)
type legacy_construction =
  { startup : Legacy_startup.t
  ; claim : legacy_callback_claim
  ; installed_claim :
      operation_id:Agent_protocol.Id.Operation.t option -> legacy_callback_claim
  ; history : unit -> (History_entry.t list, Agent_protocol.Error.t) result
  ; script_tools : Chat_response.Agent_runtime.t -> Script_tool_calls.t
  }

(** Encode an already prepared identity snapshot in the durable moderator
    envelope, without re-entering the live manager. *)
val encode_moderator_snapshot : Session.Moderator_state.Identity_snapshot.t -> Jsonaf.t

(** [moderator_snapshot_has_queued_events] inspects a durable identity snapshot
    without constructing a live runtime. *)
val moderator_snapshot_has_queued_events
  :  Jsonaf.t option
  -> (bool, Agent_protocol.Error.t) result

(** Inspect committed moderator termination without entering the live manager.
    No snapshot means false; malformed snapshots fail closed. *)
val moderator_snapshot_is_halted
  :  Jsonaf.t option
  -> (bool, Agent_protocol.Error.t) result

(** Read the installed source identity without entering the live manager. *)
val moderator_snapshot_observer
  :  Jsonaf.t option
  -> (Agent_protocol.Invocation.observer option, Agent_protocol.Error.t) result

(** [construction_checkpoint] is supplied only by the actual scoped held
    constructor, after concrete actor issuance. It repeats the owning target and
    controller fence before native acquisition, after yielded native setup and
    before initial content, Manager top-level and startup. It grants no custody
    and cannot replace host admission. None preserves ordinary construction.

    [build ... ~paths ~storage_paths ...] constructs a runtime with public path
    substitutions from [paths] and private cache/response IO from [storage_paths].
    Supply the same paths outside detached administrative preparation. Initializer
    effects on external tools/files are not rolled back by session transactions. *)
val build
  :  construction_checkpoint:(unit -> (unit, Agent_protocol.Error.t) result) option
  -> startup:Chat_response.Startup_compilation.t option
  -> legacy_construction:legacy_construction option
  -> agent_source:Captured_agent_runner.Source_binding.t option
  -> root_invocation:
       Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.t
  -> script_preparation:Script_preparation.t option
  -> sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> inference_context:Inference_runtime.Context.t
  -> inference_identity:Chat_response.Neutral_turn.Identity.t
  -> bind_inference_identity:
       (Inference_allocation.Port.t -> Chat_response.Neutral_turn.Identity.t)
  -> on_inference_attempt:(Inference_runtime.Attempt.t -> unit)
  -> on_inference_completion:(Inference_client.Completion.t -> unit)
  -> on_inference_observation:(Inference.Observation.t -> unit)
  -> paths:Runtime_paths.t
  -> storage_paths:Runtime_paths.t
  -> revision:Prompt_revision.t
  -> session_id:Agent_protocol.Id.Session.t
  -> history_namespace:string
  -> next_history_sequence:int
  -> existing_history:History_entry.t list option
  -> existing_moderator_snapshot:Jsonaf.t option
  -> moderator_reservation_size:int
  -> manifest_authorizer:Shell_runtime.Manifest_authorizer.t
  -> approval_provider:Shell_runtime.Approval_broker.provider
  -> approval_store:Shell_access.Approval.store
  -> permission_profile:Permission_policy.t
  -> review_permission:
       (Permission_policy.invocation
        -> (Permission_reviewer.Decision.t, Permission_reviewer.Error.t) result)
  -> schedule_services:schedule_services
  -> job_services:job_services
  -> (t, Agent_protocol.Error.t) result

(** Construct an extensibility-aware runtime from the captured prompt revision
    and the actual authorized native resources. Installs owned foreground dispatch
    and event services. [start_moderator] returns the prepared initial checkpoint;
    startup/resume effects wait for [moderator_activation] or the first foreground
    operation, after actor installation. Prepared standalone tools run with their
    selected native subset, host execution limits, owned pre hooks and Script-origin
    observations. They may run without a moderator or alongside extensibility-v1;
    a legacy moderator combination is rejected before initialization. Background
    jobs use explicit actor execution services; public launch and completion
    delivery remain under implementation. Public feature negotiation is unchanged;
    ordinary hosts continue using [build]. *)
val build_with_extensions
  :  construction_checkpoint:(unit -> (unit, Agent_protocol.Error.t) result) option
  -> startup:Chat_response.Startup_compilation.t option
  -> legacy_construction:legacy_construction option
  -> agent_source:Captured_agent_runner.Source_binding.t option
  -> root_invocation:
       Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.t
  -> script_preparation:Script_preparation.t option
  -> native_registrations:Chat_response.Agent_runtime.native_registration list
  -> services:extension_services
  -> sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> inference_context:Inference_runtime.Context.t
  -> inference_identity:Chat_response.Neutral_turn.Identity.t
  -> bind_inference_identity:
       (Inference_allocation.Port.t -> Chat_response.Neutral_turn.Identity.t)
  -> on_inference_attempt:(Inference_runtime.Attempt.t -> unit)
  -> on_inference_completion:(Inference_client.Completion.t -> unit)
  -> on_inference_observation:(Inference.Observation.t -> unit)
  -> paths:Runtime_paths.t
  -> storage_paths:Runtime_paths.t
  -> revision:Prompt_revision.t
  -> session_id:Agent_protocol.Id.Session.t
  -> history_namespace:string
  -> next_history_sequence:int
  -> existing_history:History_entry.t list option
  -> existing_moderator_snapshot:Jsonaf.t option
  -> moderator_reservation_size:int
  -> manifest_authorizer:Shell_runtime.Manifest_authorizer.t
  -> approval_provider:Shell_runtime.Approval_broker.provider
  -> approval_store:Shell_access.Approval.store
  -> permission_profile:Permission_policy.t
  -> review_permission:
       (Permission_policy.invocation
        -> (Permission_reviewer.Decision.t, Permission_reviewer.Error.t) result)
  -> schedule_services:schedule_services
  -> job_services:job_services
  -> (t, Agent_protocol.Error.t) result

(** Captured standalone implementations and a live lookup of the parent's checked
    execution registry. The host retains parent resources and mediates policy. *)
type inherited_managed =
  { delegation : Chat_response.Managed_tool_registry.delegation
  ; current : unit -> Chat_response.Tool_capability.t
  }

(** Construct a generated child through the same worker, moderator, invocation,
    notification and background services. Verify the immutable generated tree,
    retain exact selected parent native bindings, and consume the already compiled
    delegated moderator definition. No native/shell/MCP declarations are rebuilt.
    [inherited_managed] installs checked standalone implementations, including
    private dependencies, without copying parent lifecycle scripts or state.
    Stateful managed dependencies still reject. Input accepts
    plain text; implicit ChatMD resource loading and direct model recipes reject.

    This is runtime construction, not session admission. The owning coordinator
    must have admitted the child and its model settings before initialization,
    keep inherited resources alive, and supply services mediating current parent
    restrictions as well as the child's own policy. A live parent registry alone
    is not proof of delegability. General model-visible exposure remains gated. *)
val build_generated
  :  construction_checkpoint:(unit -> (unit, Agent_protocol.Error.t) result) option
  -> services:extension_services
  -> definition:Generated_definition.t
  -> artifact_store:Agent_store.Prompt_artifact_store.t
  -> parent_runtime:Chat_response.Agent_runtime.t
  -> inherited_managed:inherited_managed option
  -> authority:Delegation_authority.t
  -> sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> inference_context:Inference_runtime.Context.t
  -> inference_identity:Chat_response.Neutral_turn.Identity.t
  -> bind_inference_identity:
       (Inference_allocation.Port.t -> Chat_response.Neutral_turn.Identity.t)
  -> on_inference_attempt:(Inference_runtime.Attempt.t -> unit)
  -> on_inference_completion:(Inference_client.Completion.t -> unit)
  -> on_inference_observation:(Inference.Observation.t -> unit)
  -> paths:Runtime_paths.t
  -> storage_paths:Runtime_paths.t
  -> session_id:Agent_protocol.Id.Session.t
  -> history_namespace:string
  -> next_history_sequence:int
  -> existing_history:History_entry.t list option
  -> existing_moderator_snapshot:Jsonaf.t option
  -> moderator_reservation_size:int
  -> manifest_authorizer:Shell_runtime.Manifest_authorizer.t
  -> approval_provider:Shell_runtime.Approval_broker.provider
  -> approval_store:Shell_access.Approval.store
  -> permission_profile:Permission_policy.t
  -> review_permission:
       (Permission_policy.invocation
        -> (Permission_reviewer.Decision.t, Permission_reviewer.Error.t) result)
  -> schedule_services:schedule_services
  -> job_services:job_services
  -> (t, Agent_protocol.Error.t) result

(** Construct an authored child using the shared session worker and a fresh
    moderator manager, retaining the specialist's preadmitted private resources.
    Verifies both captured trees, exact source identity and the guard's live
    private registry before initialization. The host must retain the preparation
    switch and provide actual-caller policy/approval services. Authored
    reconstruction requires canonical stored history, except for genuinely fresh
    Pending initialization admitted by the Factory. [existing_history = None]
    requires either the captured child's actual held Run and initialization owner,
    or an ordinary child's actual source-bound [Runtime_construction] selecting
    [Authored_child], its [Scoped] initialization route and admitted Init scope.
    Source-kind data alone grants no construction or initialization authority.
    The same build/initial-history allocator consumes the option; no synthetic
    stored history or second initialization path is introduced. This path never
    reconstructs tools or implicitly executes prompt content while allocating
    initial history.

    [code] supplies already preflighted selected code over these exact original
    private resources. The constructor checks its original/selected affinity,
    uses its selected moderator/managed handlers and registration preparers, and
    enters its source/shell views separately at each actual dispatch. None keeps
    the original specialist identity check and resources for genuine legacy
    construction; it cannot admit a changed source as the original code.

    The specialist's own moderator-handled tools dispatch to its own manager.
    Delegated moderators use the tool-mediated compiler surface: legacy moderator
    scripts, direct Process/Model recipes and implicit ChatMD input loading are
    unavailable. Parent restrictions, activity cancellation, notifications and
    background work use the same guards as generated children. This does not
    create/publish a durable session or install root authored-tool registrations. *)
val build_authored_child
  :  construction_checkpoint:(unit -> (unit, Agent_protocol.Error.t) result) option
  -> services:extension_services
  -> revision:Prompt_revision.t
  -> prepared:authored_resources
  -> code:Authored_code_preparation.t option
  -> authority:Delegation_authority.t
  -> existing_history:History_entry.t list option
  -> sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> inference_context:Inference_runtime.Context.t
  -> inference_identity:Chat_response.Neutral_turn.Identity.t
  -> bind_inference_identity:
       (Inference_allocation.Port.t -> Chat_response.Neutral_turn.Identity.t)
  -> on_inference_attempt:(Inference_runtime.Attempt.t -> unit)
  -> on_inference_completion:(Inference_client.Completion.t -> unit)
  -> on_inference_observation:(Inference.Observation.t -> unit)
  -> paths:Runtime_paths.t
  -> storage_paths:Runtime_paths.t
  -> session_id:Agent_protocol.Id.Session.t
  -> history_namespace:string
  -> next_history_sequence:int
  -> existing_moderator_snapshot:Jsonaf.t option
  -> moderator_reservation_size:int
  -> manifest_authorizer:Shell_runtime.Manifest_authorizer.t
  -> approval_provider:Shell_runtime.Approval_broker.provider
  -> approval_store:Shell_access.Approval.store
  -> permission_profile:Permission_policy.t
  -> review_permission:
       (Permission_policy.invocation
        -> (Permission_reviewer.Decision.t, Permission_reviewer.Error.t) result)
  -> schedule_services:schedule_services
  -> job_services:job_services
  -> (t, Agent_protocol.Error.t) result

(** Borrowed selected inference execution. Its host tracking ports and lifetime
    belong to this runtime; install and retire it with the foreground worker. *)
val inference_execution : t -> Inference_client.Execution.t

(** [agent_source] binds each actual restored revision before native registration.
    None preserves legacy native construction. Some requires explicit semantic
    policy and owned revision; it uses this constructor's actual shell policies.
    The binding retains the explicit host content ceiling, never inferred from
    stored metadata. Bound ports borrow host Store/source owners
    through the resource switch. Fresh persistent specialist rebind remains a
    separate Authored/code-view operation.

    Host-only attachment of actual admitted original-source loan. Factory validates
    receipt/current catalog before native setup; runtime/private resources share
    one root loan under actual ancestor lifetime. No tool/native grant is created.
    Live runtime close first joins its existing borrowers, then releases only an
    owned loan. Existing constructors default to legacy/no source authority. *)
val with_source_lease : t -> source_lease:Prompt_source_lease.t -> t Or_error.t

val with_resource_source_lease
  :  resources
  -> source_lease:Prompt_source_lease.t
  -> resources Or_error.t

val with_authored_source_lease
  :  authored_resources
  -> source_lease:Prompt_source_lease.t
  -> authored_resources Or_error.t

(** The existing exact Model completion event payload, shared by runtime enqueue
    and Actor's factual queue-append validator. This allocates no execution or
    continuation authority. Caller selects an actual terminal Model Job. *)
val model_job_event
  :  Agent_protocol.Job.t
  -> (Chatml.Chatml_lang.value, Agent_protocol.Error.t) result
