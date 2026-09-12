open! Core

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

type model_post_stream = Chat_response.In_memory_stream.post_stream

(** Native resources and compiled, non-evaluated extension definitions. No
    operation worker, moderator manager, history or actor service is installed. *)
type resources = private
  { native : Chat_response.Agent_runtime.t
  ; definition : Chat_response.Extension_compiler.definition option
  ; managed : Chat_response.Managed_tool_registry.t option
  ; authoring : Authoring_runtime.t option
  }

(** Reconstruct an authored revision's resources using the same native/extension
    registration path as normal runtime construction, including contextual one-off
    and validation helpers. Verify the stored source tree first. Does not convert
    prompt messages, reserve history, evaluate ChatML initializers or invoke a
    moderator/model. Authorized native setup (including shell policy checks and
    MCP connections) can perform IO; this is not the no-effect authoring validator.

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
  :  native_registrations:Chat_response.Agent_runtime.native_registration list
  -> native_service_revision:string option
  -> env:Eio_unix.Stdenv.base
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
  }

(** Prepare the named specialist's private resources from its defining parent's
    captured source tree. Retains the authored parser, source-relative paths and
    host-supplied workspace/tool/storage capabilities. Only [prompt_dir] is re-rooted
    to the specialist's captured source directory. Does not prepare/expose the
    parent's public tools, install an artifact/session or initialize scripts.

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
  :  native_registrations:Chat_response.Agent_runtime.native_registration list
  -> parent_revision:Prompt_revision.t
  -> tool_name:string
  -> native_service_revision:string option
  -> env:Eio_unix.Stdenv.base
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
  ; claim_lifecycle : event:Chat_response.Moderation.Event.t -> Moderator_event.claim
    (** Actual running-idle actor ownership. Never manufacture an operation. *)
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

type t =
  { worker : Operation_worker.t
  ; parse_user_content :
      id:History_entry.Id.t
      -> Agent_protocol.Session.Message_content.t
      -> (History_entry.t, Agent_protocol.Error.t) result
  ; initial_history : History_entry.t list
  ; initial_prompt_entry_count : int
  ; reserved_history_through : int
  ; mutable moderator_snapshot : Jsonaf.t option
  ; moderator_manager : Chat_response.Moderator_manager.t option
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
  ; start_moderator : unit -> (Jsonaf.t option, Agent_protocol.Error.t) result
  ; enqueue_internal_event :
      ?prepare:prepare_enqueue
      -> Jsonaf.t
      -> (Jsonaf.t option, Agent_protocol.Error.t) result
  ; drain_internal_events :
      History_entry.t list -> (moderator_drain, Agent_protocol.Error.t) result
  ; execute_model_job :
      recipe:string
      -> payload:Jsonaf.t
      -> (model_job_outcome, Agent_protocol.Error.t) result
  ; enqueue_model_job_completion :
      ?prepare:prepare_enqueue
      -> Agent_protocol.Job.t
      -> (Jsonaf.t option, Agent_protocol.Error.t) result
  ; close : unit -> unit
  }

type schedule_services =
  { after_ms : delay_ms:int -> payload:Jsonaf.t -> (string, string) result
  ; cancel : id:string -> (unit, string) result
  }

type job_services =
  { spawn_model : recipe:string -> payload:Jsonaf.t -> (string, string) result
  ; call_model :
      recipe:string
      -> payload:Jsonaf.t
      -> execute:
           (unit
            -> (Chat_response.Moderation.Capabilities.model_call_result, string) result)
      -> (Chat_response.Moderation.Capabilities.model_call_result, string) result
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

(** [build ... ~paths ~storage_paths ...] constructs a runtime with public path
    substitutions from [paths] and private cache/response IO from [storage_paths].
    Supply the same paths outside detached administrative preparation. Initializer
    effects on external tools/files are not rolled back by session transactions. *)
val build
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
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
  -> model_post_stream:model_post_stream option
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
  :  native_registrations:Chat_response.Agent_runtime.native_registration list
  -> services:extension_services
  -> sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
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
  -> model_post_stream:model_post_stream option
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
  :  services:extension_services
  -> definition:Generated_definition.t
  -> artifact_store:Agent_store.Prompt_artifact_store.t
  -> parent_runtime:Chat_response.Agent_runtime.t
  -> inherited_managed:inherited_managed option
  -> authority:Delegation_authority.t
  -> sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
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
  -> model_post_stream:model_post_stream option
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
    switch and provide actual-caller policy/approval services and canonical stored
    history. This path never reconstructs tools or implicitly executes prompt
    content while allocating initial history.

    The specialist's own moderator-handled tools dispatch to its own manager.
    Delegated moderators use the tool-mediated compiler surface: legacy moderator
    scripts, direct Process/Model recipes and implicit ChatMD input loading are
    unavailable. Parent restrictions, activity cancellation, notifications and
    background work use the same guards as generated children. This does not
    create/publish a durable session or install root authored-tool registrations. *)
val build_authored_child
  :  services:extension_services
  -> revision:Prompt_revision.t
  -> prepared:authored_resources
  -> authority:Delegation_authority.t
  -> history:History_entry.t list
  -> sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
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
  -> model_post_stream:model_post_stream option
  -> review_permission:
       (Permission_policy.invocation
        -> (Permission_reviewer.Decision.t, Permission_reviewer.Error.t) result)
  -> schedule_services:schedule_services
  -> job_services:job_services
  -> (t, Agent_protocol.Error.t) result
