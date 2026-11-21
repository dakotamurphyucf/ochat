open! Core
module Close = Runtime_close

(** Read-only facts retained by the actual physical Product attachment. No
    allocation, enrollment, execution or cleanup authority is public. *)
module Product_callbacks : sig
  type t

  module Operation_snapshot : sig
    type t

    val receipt : t -> Callback_cleanup.Operation.t
    val registration_closed : t -> bool

    (** None means the Product genuinely has no Manager. Some filters only
        children of its actual Manager; registration/terminal are original. *)
    val callbacks : t -> Callback_cleanup.Cleanup_snapshot.t option

    val terminal : t -> Callback_cleanup.Operation.terminal_outcome option
    val terminal_diagnostic : t -> Cleanup_diagnostic.t option

    (** Frozen lookup. SAME original Completion and write-once slot returns SAME
        carrier; unknown or Manager-filtered claims return None. *)
    val completion_diagnostic
      :  t
      -> completion:Callback_cleanup.Completion.t
      -> slot:Callback_cleanup.Completion_snapshot.Failure_slot.kind
      -> Cleanup_diagnostic.t option

    val diagnostics : t -> Cleanup_diagnostic.t list

    (** Domain-checked fresh view of THIS captured original Operation only. It
        cannot enroll a later Product operation or mutate the old snapshot. *)
    val refresh : t -> (t, Agent_protocol.Error.t) Result.t
  end

  module Snapshot : sig
    type t

    val operations : t -> Operation_snapshot.t list
  end

  (** Domain-checked live read, including after Product physical close. No Actor
      RPC or cleanup runs. Frozen facts/carriers are immutable. Only genuinely
      positive parents compact; missing/failed raw terminal remains retained. *)
  val snapshot : t -> (Snapshot.t, Agent_protocol.Error.t) Result.t
end

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

(** Retained ancestor bindings under the original ancestor scope. Normal use
    checks strict resource custody. Policy restoration may compare only opaque
    evidence for the exact promoting owner, with strict custody for other owners. *)
type ancestor_capabilities =
  { for_execution :
      Agent_protocol.Id.Session.t
      -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result
  ; for_policy_restoration :
      promoting:Agent_protocol.Id.Session.t
      -> Agent_protocol.Id.Session.t
      -> (Delegation_authority.Binding_evidence.t, Agent_protocol.Error.t) result
  }

(** Retained physical Source lookup under the actual ancestor native scopes.
    A callback receives the SAME selected Source while its existing custody is
    current. This is neither path permission nor policy admission. It retains no
    first dependent's execution check and performs no load or preparation.
    Custody checks may wait. A callback Error or original exception remains
    primary if the physical view expires; only callback success is postchecked.
    Sources delivered here still need current membership/policy validation. *)
type ancestor_sources =
  { with_source :
      'a.
      Agent_protocol.Id.Session.t
      -> f:(source:Source.t -> ('a, Agent_protocol.Error.t) result)
      -> ('a, Agent_protocol.Error.t) result
  }

(** Native resources and compiled, non-evaluated extension definitions. No
    operation worker, moderator manager, history or actor service is installed. *)
type resources = private
  { source_lease : Prompt_source_lease.t
  ; native : Chat_response.Agent_runtime.t
  ; definition : Chat_response.Extension_compiler.definition option
  ; managed : Chat_response.Managed_tool_registry.t option
  ; authoring : Authoring_runtime.t option
  ; ancestor_capabilities : ancestor_capabilities option
  ; ancestor_sources : ancestor_sources option
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

(** The specialist's original successful preparation. [prepared_revision] is
    immutable Source classification captured in that SAME program preparation,
    before any future child Reference. It supplies no source or execution grant
    and adds no physical owner; later binding requires the actual admitted input. *)
type authored_resources = private
  { source : Authored_agent_source.t
  ; revision : Prompt_revision.t
  ; prepared_revision : Source.Prepared_revision.t
  ; resources : resources
  ; agent_source : Captured_agent_runner.Source_binding.t option
  ; code_authorizer : Prompt_revision.t -> Shell_runtime.Manifest_authorizer.t
  }

(** Captured standalone implementations and a live lookup of the parent's checked
    execution registry. The host retains parent resources and mediates policy. *)
type inherited_managed =
  { delegation : Chat_response.Managed_tool_registry.delegation
  ; current : unit -> Chat_response.Tool_capability.t
  }

module Native_identity : sig
  type t

  (** Issued privately once at actual prepared entry-unit allocation. Product
      identity is a later attachment identity; it cannot substitute for this. *)
  val equal : t -> t -> bool
end

(** Lexical validation purpose; grants no execution or effect authority. Every resource
    operation revalidates exact prefix, original authority/family and caller
    checkpoint. No successful validation is cached. *)
module Native_preparation : sig
  type t =
    | Original_execution
    | Resource_setup of
        { prefix : Delegation_authority.Resource_path.Prefix.t
        ; check_resource : unit -> (unit, Agent_protocol.Error.t) result
        }
end

module Native_input : sig
  (** Closed source families with their EXISTING admitted borrowed values.
      These inputs and Source.t are not consumer authorization. Captured carries
      the real retained recurrence source and its existing preparation authority;
      artifact metadata alone cannot create a captured executable input. Generated
      source_lease is the actual parent's source view retained under the same CPS
      as parent_runtime. Preparation borrows that view without claiming or closing
      the parent's root, and preserves missing/unavailable/revoked source state. *)
  type family =
    | Authored of
        { revision : Prompt_revision.t
        ; root_invocation :
            Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.t
        ; agent_source : Captured_agent_runner.Source_binding.t option
        ; script_preparation : Script_preparation.t option
        ; native_registrations : Chat_response.Agent_runtime.native_registration list
        }
    | Captured of
        { captured : Captured_recurrence_source.t
        ; script_preparation : Script_preparation.t option
        ; authority : Delegation_authority.t
        ; agent_source : Captured_agent_runner.Source_binding.t option
        ; native_registrations : Chat_response.Agent_runtime.native_registration list
        }
    | Generated of
        { definition : Generated_definition.t
        ; artifact_store : Agent_store.Prompt_artifact_store.t
        ; parent_runtime : Chat_response.Agent_runtime.t
        ; source_lease : Prompt_source_lease.t
        ; inherited_managed : inherited_managed option
        ; authority : Delegation_authority.t
        }
    | Authored_child of
        { revision : Prompt_revision.t
        ; prepared : authored_resources
        ; code : Authored_code_preparation.t option
        ; authority : Delegation_authority.t
        }

  type t

  (** Validate the selected executable family, exact artifact publication and
      invocation, including the actual child Reference when the selection carries
      one. Authored_child can also bind a genuine Captured Authored_tool selection
      only with its exact specialist selector, artifact, actual Ui child root, Delegated_ui runtime
      invocation and full private authority Reference. It retains the borrowed specialist
      preparation and captured Product family; it is never an Artifact/root
      input or a new source-loan association. Physical preparation checks the actual target session/profile, tree,
      capabilities and original authority. This creates no permission and does
      not admit Generated_artifact DATA as an executable definition. *)
  val create : source:Source.t -> family:family -> (t, Agent_protocol.Error.t) result
end

module Native_host : sig
  type t =
    { preparation : Native_preparation.t
      (** Lexical construction purpose only. Not retained in Prepared_native,
          resources, original authority, Ctx, registered functions or Use. *)
    ; check_source : unit -> (unit, Agent_protocol.Error.t) result
      (** Lexical actual setup/per-consumer guard. Call before/after constructor
          waits; never retain it in Prepared_native, Ctx, native bindings,
          Product or future consumer guards after this setup stage ends. *)
    ; startup : Chat_response.Startup_compilation.t option
    ; env : Eio_unix.Stdenv.base
    ; inference_context : Inference_runtime.Context.t
    ; inference_identity : Chat_response.Neutral_turn.Identity.t
    ; bind_inference_identity :
        Inference_allocation.Port.t -> Chat_response.Neutral_turn.Identity.t
    ; on_inference_attempt : Inference_runtime.Attempt.t -> unit
    ; on_inference_completion : Inference_client.Completion.t -> unit
    ; on_inference_observation : Inference.Observation.t -> unit
    ; paths : Runtime_paths.t
    ; storage_paths : Runtime_paths.t
    ; session_id : Agent_protocol.Id.Session.t
    ; permission_profile : Permission_policy.t
    ; native_service_revision : string option
    ; one_off_policy : Chat_response.One_off_request.policy option
    ; authoring_validation_host : Chat_response.Authoring_validation.host option
    ; initial_agent_guidance : Chat_response.Agent_guidance.t option
    ; ancestor_capabilities : ancestor_capabilities option
      (** Original actual retained capability lookup, owned by the common native
          lifetime across adoption. Never the lexical [check_source] closure. *)
    ; ancestor_sources : ancestor_sources option
      (** Matching physical Source scopes under that SAME retained lifetime.
          Root hosts without an ancestor scope supply [None]. *)
    ; manifest_authorizer : Shell_runtime.Manifest_authorizer.t
    ; approval_provider : Shell_runtime.Approval_broker.provider
    ; approval_store : Shell_access.Approval.store
    }
end

module Prepared_native : sig
  (** One entry-owned native unit: existing resources record, original selected
      family, entry cache/Ctx/Inference Session/Root_binding and actual graph
      callback provenance. It does not copy or rebind an ancestor/specialist's
      native closures to the entry context. Their original authority/context and
      actual ancestor CPS loans remain live for the complete dependent lifetime.

      Generated authoring setup is retained in the final resources record and
      is not repeated by attachment. Initial guidance is installed in the entry
      Ctx before native closures are created. No Manager/history/worker/lifecycle/
      Task/provider execution is added by this preparation. Existing authorized shell/MCP/native setup may do IO.
      A Pending source is not made Ready. The caller attaches partial scope and
      graph and Construction custody to the Cell before invoking this yielding
      operation. On
      Error/exception/cancellation the caller still owns that scope and must join
      it; preparation does not release borrowed ancestor resources. *)
  type t

  module Construction : sig
    (** Partial common-leg custody inside the same Cell owner. Capture this
        actual holder in the Factory close-attempt closure before prepare can
        yield. It binds one creating domain/switch, permits one preparation, and
        survives Error/exception/cancellation without losing close-leg receipts. *)
    type t

    val create : sw:Eio.Switch.t -> t

    (** Trusted final common cleanup after preparation has joined. Same closer
        as the returned prepared unit; successful Root/Session/cache legs are
        never repeated, failed legs remain available for explicit serial retry,
        including after Eio has consumed switch finalizers. Concurrent prepared-
        unit close and switch finalization join the same close attempt. Raises
        for a foreign domain or close while preparation is still running. Does
        not stop/join the switch or release the actual ancestor loans/graph. *)
    val close : t -> unit

    (** SAME actual shared release attempt, observed before raw projection.
        Concurrent callers receive the same immutable observation/carriers;
        a later failed retry has its own actual carrier. Domain/preparation
        invariant failures retain the same raising behavior as [close]. *)
    val close_reported : t -> unit Close_observation.t
  end

  val prepare
    :  Native_input.t
    -> construction:Construction.t
    -> host:Native_host.t
    -> (t, Agent_protocol.Error.t) result

  val identity : t -> Native_identity.t

  (** SAME already validated executable Native_input source, with creating-domain
      and actual common-unit liveness checks. No source IO, recompile, native
      capability, permission or closer. Ordinary private units with no retained
      selection reject. Factory must still compare the current metadata/private
      source, profile and actual native key before every promotion. *)
  val selected_source : t -> (Source.t, Agent_protocol.Error.t) result

  (** Pure domain/open-unit/exact selected-source association, not current
      consumer/grant authorization. Every borrow and promotion separately uses
      the fresh Owner admission plus fixed Factory Store/source checks. *)
  val check
    :  t
    -> source:Source.t
    -> session_id:Agent_protocol.Id.Session.t
    -> permission_profile_digest:string
    -> (unit, Agent_protocol.Error.t) result

  module Delegation : sig
    (** SAME original prepared execution/source context, never lexical
        Native_setup or a new consumer's authority. These native bindings and
        inherited managed registry remain backed by the unit's retained complete
        ancestor CPS until final Cell release. No cleanup ownership is supplied. *)
    type t = private
      { authority : Delegation_authority.t
      ; selected : Chat_response.Tool_capability.t
      ; inherited_managed : inherited_managed option
      }
  end

  (** Creating domain and actual common-unit liveness, then the original closed
      source context. None is authored root only. Caller must hold actual native/
      promotion custody throughout use, repeat current private/source/grant checks
      around waits and use [check_execution] for current original authority. The
      projection alone grants no effect, disclosure, Caller or policy permission;
      it never reconstructs Independent ancestry or rebinds inherited closures. *)
  val with_delegation
    :  t
    -> f:(Delegation.t option -> ('a, Agent_protocol.Error.t) result)
    -> ('a, Agent_protocol.Error.t) result

  (** Trusted Cell CPS with an actual active Native_lease. No Manager/runtime or
      execution capability is supplied; callers retain the loan through all use.
      The original resources record is not rebuilt or mirrored. *)
  val with_resources
    :  t
    -> f:(resources -> ('a, Agent_protocol.Error.t) result)
    -> ('a, Agent_protocol.Error.t) result

  (** Trusted Factory prepublication association with the actual authored/captured-root
      source loan, using the existing one-time attachment claim. Same domain and
      open unit are required. Mutates this SAME prepared unit; native, identity,
      context and release holder are unchanged. A successful nonlegacy attachment
      is visible to every alias; every second association, including legacy,
      rejects. Failed checks or claim leave the unit unchanged. The actual root
      remains owned/closed by its physical child switch; this unit holds only a
      borrowed view with no root-close responsibility. Legacy/absent intent stays legacy and never
      gains live-root authority. The actual source switch owns root lifetime;
      this operation does not close/reopen it or authorize source reads. Generated
      and authored-child units preserve their original borrowed source loan and
      reject this root-only method before any claim. Captured roots require the
      actual Captured_recurrence_source and existing preparation authority at
      Native_input creation/preparation; root association creates no occurrence
      Run or lifecycle authority. *)
  val attach_source_lease : t -> source_lease:Prompt_source_lease.t -> unit Or_error.t

  (** Trusted final Cell election only, after all native/Product refs and actual
      borrowers join. Owns the one shared physical release; all original close
      legs attempted independently, first exception/rawtrace preserved. Ordinary
      wrappers with no external native loans retain their existing grouped
      attachment/cache close sequence at the Product election. Not callable by failed promotion cleanup
      while another native loan survives. Source/switch/graph joins stay with the
      actual Cell/Factory lifetime holder, not an escaped lexical switch. *)
  val close : t -> unit

  (** Observation of the SAME common release used by Construction and switch
      finalization. Reading/projecting it never repeats successful physical legs
      or consumes diagnostic delivery. Foreign-domain misuse still raises. *)
  val close_reported : t -> unit Close_observation.t
end

module Product_attachment : sig
  (** Partial attachment custody inside the SAME Cell. Allocate and attach to
      Promotion before the yielding Builder call. The holder records the actual
      moderator executor before Manager construction can yield. No shared native
      resources or second physical owner are created. *)
  type t

  (** Domain/open-native checks and local allocation only. No callback or IO. *)
  val create : Prepared_native.t -> (t, Agent_protocol.Error.t) result

  val native_identity : t -> Native_identity.t

  (** Trusted Cell cleanup, after attachment construction and installed work
      have joined. Only the actual attachment-local executor wakeup registration
      is removed; the Manager has no close primitive and the worker is an inert
      record. Idempotent after success. Does not close common native resources,
      cache, Session, Root_binding, switch, graph or ancestor loans. Raises on a
      foreign domain or an attempt before construction has joined. *)
  val close : t -> unit
end

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
      -> consume:
           Notification_delivery.t Operation_worker.Capabilities.notification_consumer
      -> unit
      -> ( Chat_response.In_memory_stream.Safe_point_input.batch
           , Agent_protocol.Error.t )
           result
  ; initial_notification_input :
      source:Agent_protocol.Invocation.observer option
      -> tools:Script_tool_calls.t
      -> consume:
           Notification_delivery.idle Operation_worker.Capabilities.notification_consumer
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
  ; run :
      ?capture_cleanup:(Callback_cleanup.Completion.t -> unit)
      -> unit
      -> (bool, Agent_protocol.Error.t) result
    (** This invocation's actual legacy claims only. Forward to the current
        lifetime owner; never retain the capture for a later activation. Extension
        activation has no legacy Completion to capture. *)
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

(** Verified Builder classification and immutable Actor installation affinity.
    No raw issuer is exported. These facts grant no permission or execution. *)
module Unmoderated_source : sig
  type t

  module Binding : sig
    type source = t
    type t

    (** Pure domain-first same-worker and complete accepted Spec basis check. *)
    val check
      :  t
      -> state:Session_state.t
      -> worker:Operation_worker.t
      -> manager:Chat_response.Moderator_manager.t option
      -> (Policy_cause_data.Caller_source.t, Agent_protocol.Error.t) result

    (** Read-only warm no-op validation of the SAME original Builder fact.
        Never replaces or reissues the current binding. *)
    val check_source
      :  t
      -> source
      -> state:Session_state.t
      -> worker:Operation_worker.t
      -> manager:Chat_response.Moderator_manager.t option
      -> (unit, Agent_protocol.Error.t) result

    (** Prepare the exact checked configuration transition's replacement before
        persistence. Installation belongs to its same known State ACK; an old
        Caller/Bound is never renewed. No callback, mutation or authority. *)
    val reconfigure
      :  t
      -> previous:Session_state.t
      -> candidate:Session_state.t
      -> worker:Operation_worker.t
      -> manager:Chat_response.Moderator_manager.t option
      -> (t, Agent_protocol.Error.t) result
  end

  (** Bind a verified actual Artifact/final-worker fact to the Actor's accepted
      install State. All existing source/owner/Stop/admission checks remain
      mandatory. Manager absence alone cannot issue this value. *)
  val bind
    :  t
    -> state:Session_state.t
    -> worker:Operation_worker.t
    -> manager:Chat_response.Moderator_manager.t option
    -> (Binding.t, Agent_protocol.Error.t) result
end

type t =
  { source_lease : Prompt_source_lease.t
  ; worker : Operation_worker.t
  ; product_callbacks : Product_callbacks.t option
    (** SAME attachment-owned original Operation observer across retained bindings
        and convenience builds. None is actual non-Product compatibility. *)
  ; unmoderated_source : Unmoderated_source.t option
    (** Actual verified absence and this final physical worker; None remains
        unknown. Cold/warm Builder bindings issue their own matching fact. *)
  ; inference_execution : Inference_client.Execution.t
  ; now : unit -> Agent_protocol.Timestamp.t
    (** Host wall clock shared by foreground, idle and delegated moderation and
        persisted workflow deadlines. Monotonic execution limits are separate. *)
  ; parse_user_content :
      ?asset_capture:Chat_response.Converter_asset_capture.t
      -> id:History_entry.Id.t
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
  ; initial_installed_originals : Recovery_installed_originals.Initial.t option
    (** Address-only custody after admission against the exact initial prefix. *)
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
  ; check_ancestry :
      (unit -> (Delegation_authority.Checked_ancestry.t, Agent_protocol.Error.t) result)
        option
    (** Same actual delegated authority used by check_execution, under this
        runtime's real resource lease. May yield; invoke outside Actor mailboxes
        and Owner/Registry locks. None for ordinary/root constructors, never
        evidence that a delegated Spec has no inherited governance. *)
  ; ancestor_capabilities : ancestor_capabilities option
    (** Internal factory lookup of the retained original ancestor bindings. Allows
        descendants below an independent lifetime to validate against reconstructed
        resources without activating stopped ancestors. Use only under this runtime's
        lease; the lookup supplies resources, not permission to execute them. *)
  ; retained_ancestor_sources : ancestor_sources option
    (** Immutable ancestor sources from the same prepared native resources. Use
        each source inside its callback while holding this runtime's lease. *)
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
      (capture_cleanup:(Callback_cleanup.Completion.t -> unit) option
       -> max_events:int
       -> history:History_entry.t list
       -> (Chat_response.Moderation.Outcome.t list, Agent_protocol.Error.t) result)
        option
    (** The current optional capture belongs only to this invocation's actual
        acquired claims; adapters must not retain it across calls.
        Installed legacy idle adapter: each exact head has its own Event ACK.
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
  ; close : Close.t
    (** Caller cancels/drains foreground and borrowed background workers first.
        Closes and joins graph-owned inference channels, then persists cache. *)
  }

(** Execute the runtime's sole closer and preserve its actual raw exception.
    Reporting forwards the same invocation's immutable groups and primary;
    neither accessor consumes diagnostic presentation or changes custody. *)
val close : t -> unit

val close_reported : t -> unit Close_observation.t

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
  ; policy_caller : unit -> (Delegated_policy_caller.t, Agent_protocol.Error.t) result
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
      ?capture_cleanup:(Callback_cleanup.Completion.t -> unit)
      -> operation_id:Agent_protocol.Id.Operation.t option
      -> legacy_callback_claim
    (** Forward the current option to the SAME Actor's actual claim transport.
        Foreground callbacks use their original Operation owner; idle callers
        retain acquired Completions in their own invocation lifetime. No capture
        is synthesized for rejection without an actual installed claim. *)
  ; history : unit -> (History_entry.t list, Agent_protocol.Error.t) result
  ; script_tools : Chat_response.Agent_runtime.t -> Script_tool_calls.t
  }

type runtime = t

module Product_identity : sig
  (** Physical resource identity, never an execution grant or resource lease. *)
  type t

  val equal : t -> t -> bool
end

(** Factual completion of one still-live retained binding. No execution grant,
    callback runner, decoder or public issuer. *)
module Retained_lifecycle : sig
  type t

  (** Pure same-domain negative check against the actual binding/Product,
      completed lifecycle owner and SAME prepared source/worker/Manager.
      Full source/generation and target must match the Actor's actual State.
      Does not refresh Store, run callbacks or prove current Goal/Run authority. *)
  val check
    :  t
    -> source:Source.t
    -> expected:Session_state.t
    -> worker:Operation_worker.t
    -> manager:Chat_response.Moderator_manager.t option
    -> (unit, Agent_protocol.Error.t) result
end

module Pending_legacy_resume : sig
  type t

  module Inference : sig
    (** One genuine current initialization graph supplies the identity, exact
        allocator binding and all factual callbacks together. This record grants
        no inference authority; the actual allocation port is still mandatory. *)
    type t =
      { identity : Inference_client.Identity.t
      ; bind_identity : Inference_allocation.Port.t -> Inference_client.Identity.t
      ; on_attempt : Inference_runtime.Attempt.t -> unit
      ; on_observation : Inference.Observation.t -> unit
      ; on_completion : Inference_client.Completion.t -> unit
      }
  end

  (** Consume the original pending gate under the genuine current scoped
      Constructor host. Installed_resume is rejected. Only the winning actual
      checkpoint ACK completes the shared owner, without yielding; failure is
      sticky and cancellation propagates. No Manager, allocator, source or
      callback principal is reconstructed from stored data. The lexical inference
      binding is forced only inside the same-gate Ready callback and is never
      retained after this call. Cached scoped ACK/ordinary winner/Failed does not
      force it; the Factory must own any graph before its creation can yield. *)
  val run_scoped
    :  t
    -> host:legacy_construction
    -> inference:(Inference.t, Agent_protocol.Error.t) result Lazy.t
    -> (Runtime_resume.t, Agent_protocol.Error.t) result
end

module Attachment : sig
  type t

  type origin =
    | Cold
    | Retained

  module Resume : sig
    type t =
      | Continue_live
      | Pending of Runtime_resume.t
      | Pending_legacy of Pending_legacy_resume.t
  end

  (** Invoke this attachment's ONE stored actual closer once. Joins/retained
      retries follow that closer's existing contract. The immutable observation
      forwards actual diagnostic groups and exact raw exit, never custody. The
      runtime's legacy [close] projects this SAME implementation. *)
  val close_reported : t -> unit Close_observation.t

  val runtime : t -> runtime
  val origin : t -> origin
  val product : t -> Product_identity.t option

  (** Actual Product-backed native affinity while its binding is live. Raw cold
      adapters return None. No native resource or execution grant is supplied. *)
  val native_identity : t -> Native_identity.t option

  (** Legacy host adaptation states only cold behavior. It cannot manufacture
      retained resource identity or skip an actual pending callback. *)
  val cold : runtime -> t

  (** Factual selection of the original lifecycle owner, bound to this worker.
      Completed ordinary activation selects [Continue_live]. A completed scoped
      legacy Resume retains the same worker-bound factual [Pending] result until
      installation; reading it does not consume it. Failure preserves its original
      error. Ready remains its actual callback kind, never converted from start
      to resume. *)
  val runtime_resume : t -> (Resume.t, Agent_protocol.Error.t) result

  (** Only the actual retained Binding can issue this witness. Ready/scoped ACK
      return None; Failed preserves its original error. Cold adapters never
      issue one. A completed owner is read directly; no callback/cache forcing.
      Closing its binding irreversibly retires every witness before yielding. *)
  val retained_lifecycle
    :  t
    -> (Retained_lifecycle.t option, Agent_protocol.Error.t) result
end

(** Factual affinity of one still-open physical policy Product. It is independent
    of retired worker bindings and grants no original or execution admission. *)
module Policy_access : sig
  type t

  (** Pure same-domain, completed extension lifecycle and exact current
      session/generation/spec/full installed source/Stop basis check. No Manager
      snapshot, lock, source preparation, callback forcing or legacy check. *)
  val check : t -> expected:Session_state.t -> (unit, Agent_protocol.Error.t) result
end

(** Affinity of an explicitly restored physical extension Product whose genuine
    ordinary Session_resume remains pending. This is distinct from an actually
    Completed in-memory lifecycle and grants no source or execution admission. *)
module Restored_policy_access : sig
  type t

  val product_identity : t -> Product_identity.t
  val basis : t -> Policy_restore_basis.t

  (** Same domain, still-open actual Product and pending resume owner, and SAME
      current selected lifecycle/source/resource/Stop DATA. No Manager read or
      callback execution; the actual serialized claim checks current checkpoint. *)
  val check : t -> expected:Session_state.t -> (unit, Agent_protocol.Error.t) result
end

module Product : sig
  type t

  module Identity = Product_identity

  val identity : t -> Identity.t
  val native_identity : t -> Native_identity.t

  (** Same-domain/open Product and the SAME actual preattached holder after
      successful construction. Physical association, not permission or lifecycle
      completion. A different attachment sharing the native unit is rejected. *)
  val check_attachment
    :  t
    -> attachment:Product_attachment.t
    -> (unit, Agent_protocol.Error.t) result

  (** Same-domain/open Product and its SAME common native unit's immutable
      selected Source. Caller retains the actual Cell loan during dependent use.
      Missing selection is typed unavailable, never No_moderator; no source IO,
      preparation, source grant or cleanup owner is created. *)
  val selected_source : t -> (Source.t, Agent_protocol.Error.t) result

  (** Loaded-first bridge over this Product's actual common resources. The
      trusted Cell caller retains its real Product lease throughout the callback.
      Checks the Product and common unit remain open; no common closer or new
      unit escapes and no admission is supplied. *)
  val with_native_resources
    :  t
    -> f:(resources -> ('a, Agent_protocol.Error.t) result)
    -> ('a, Agent_protocol.Error.t) result

  (** Trusted Factory factual issuer from its SAME physical Cell runtime, after
      exact Cell lease/key/profile validation. Requires original extension owner
      Completed; Ready cannot activate and Failed preserves its original error.
      No snapshot is read here: actual serialized event capture/claim validates
      Manager checkpoint identity. This is no source or permission grant and does
      not validate arbitrary manufactured runtime-record closures. *)
  val policy_access
    :  t
    -> runtime:runtime
    -> expected:Session_state.t
    -> (Policy_access.t, Agent_protocol.Error.t) result

  (** Factory-only factual issuer after actual policy construction admission.
      Checks this physical Product's exact prepared source/target, worker and
      extension Manager, genuinely pending Session_resume, original complete raw
      checkpoint basis and current known Manager snapshot. No callback, new Init
      ACK, worker installation or permission is supplied. Existing constructors
      and their real lifecycle state are unchanged. The Factory separately owns
      the actual source stage, writer/Handle and final Cell publication. *)
  val restored_policy_access
    :  t
    -> runtime:runtime
    -> source:Source.t
    -> basis:Policy_restore_basis.t
    -> expected:Session_state.t
    -> (Restored_policy_access.t, Agent_protocol.Error.t) result

  module Binding : sig
    type t

    (** Raw actual binding for Factory preparation only. Before publication,
        Factory must attach the exact resource-lease finalizer below. *)
    val runtime : t -> runtime

    (** Checked actual Product identity, requiring its creating domain, open
        Product and live binding. Pending ordinary initialization is permitted;
        this is affinity only, never lifecycle completion or admission. *)
    val product_identity : t -> (Product_identity.t, Agent_protocol.Error.t) result

    (** Preserve the actual Factory source/guard wrappers on this same worker,
        Manager and native runtime, and install its resource finalizer before
        escape. This is trusted Factory wrapping: physical worker, Manager and
        native identity are checked, not every runtime record closure or source
        field. Final Factory/source/ACK validation is still required. A foreign
        physical view is rejected. [close] only releases the actual host
        binding/resource lease; it is not an admission callback. *)
    val attach
      :  t
      -> runtime:runtime
      -> close:(unit -> unit)
      -> (Attachment.t, Agent_protocol.Error.t) result

    (** Same factual Binding checks and retirement position as [attach], with
        ONLY one actual reported closer. No second raw callback or observer is
        stored. Factory forwards the original Cell/stage groups unchanged;
        simple [attach] adapts its single raw closer to this implementation. *)
    val attach_reported
      :  t
      -> runtime:runtime
      -> close:(unit -> unit Close_observation.t)
      -> (Attachment.t, Agent_protocol.Error.t) result
  end

  (** New worker view of the same physical services and allocator. Checks the
      known Manager identity state against the supplied checkpoint without
      restoring it, and retains the supplied full checkpoint unchanged. Factory
      must establish custody of that checkpoint, including its queue metadata
      and unknown fields. [source] and [expected] are the actual Factory-selected
      construction source and State: target, construction family/artifact/session
      must match this Product. They are factual affinity, never permission.
      Does not compile, reserve history, run Init, rotate source or grant execution. Factory must separately
      retain and validate its exact source/Init ACK and actual resource lease. *)
  val bind
    :  t
    -> source:Source.t
    -> expected:Session_state.t
    -> history:History_entry.t list
    -> moderator_checkpoint:Jsonaf.t option
    -> (Binding.t, Agent_protocol.Error.t) result

  (** Ordinary authored-root counterpart using the actual verified retained
      revision rather than a fabricated deferred Source.t. Same physical
      Manager/checkpoint/new-worker checks as [bind], no compile, restore, Init,
      history reservation or original grant. The factual lifecycle basis must
      match an actual authored Source at any later native installation; other
      source families retain their existing exact prepared-source identity. *)
  val bind_authored
    :  t
    -> revision:Prompt_revision.t
    -> expected:Session_state.t
    -> history:History_entry.t list
    -> moderator_checkpoint:Jsonaf.t option
    -> (Binding.t, Agent_protocol.Error.t) result

  (** Close this Product once. A shared native promotion closes attachment-local
      state only; the Cell's last combined reference owns common native release.
      Existing nonshared constructors retain their full physical close sequence.
      Factory owns election/private-switch join and graph factual finish retries. *)
  val close : t -> unit

  (** SAME cached original physical Product producer, including when called
      through a first/rebound binding or retained Cell partial fallback. Joins
      do not run another physical close or mint another diagnostic carrier. *)
  val close_reported : t -> unit Close_observation.t
end

(** Attach Manager/history/script services and a dormant worker to the SAME
    prepared entry resources/context/Session/Root_binding/graph. Native creation,
    inherited preparation, Generated authoring setup and initial guidance are
    not repeated. Borrowed ancestor closures keep their originating context and
    existing authority, including Generated inherited currentness.

    The actual Cell Promotion must own [attachment] before this call can yield.
    Error/exception/cancellation leaves partial attachment and shared native
    custody with that Cell; no common close or replacement construction occurs.
    Each holder permits one construction attempt. The actual Factory checkpoint
    and source/session/profile association are checked before attachment; this
    does not replace ordinary initialization or genuine policy restoration
    admission. Existing module/initial-state expression semantics are retained.
    Worker allocation does not install or execute it. A shared Product's close
    releases attachment-local state only, after all its work joins.
    The initial runtime carries the SAME prepared resource's borrowed source view;
    it neither claims nor closes another root owner. Retained binding creation
    keeps its existing separate Factory source-attachment contract. *)
val build_product_from_native
  :  Prepared_native.t
  -> attachment:Product_attachment.t
  -> source:Source.t
  -> construction_checkpoint:(unit -> (unit, Agent_protocol.Error.t) result)
  -> legacy_construction:legacy_construction option
  -> services:extension_services option
  -> history_namespace:string
  -> next_history_sequence:int
  -> existing_history:History_entry.t list option
  -> existing_moderator_snapshot:Jsonaf.t option
  -> moderator_reservation_size:int
  -> permission_profile:Permission_policy.t
  -> review_permission:
       (Permission_policy.invocation
        -> (Permission_reviewer.Decision.t, Permission_reviewer.Error.t) result)
  -> schedule_services:schedule_services
  -> job_services:job_services
  -> (Product.t * Product.Binding.t, Agent_protocol.Error.t) result

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

(** Build a captured Artifact with its defining root grammar and actual private
    delegation authority. The captured origin must match that authority's actual
    immutable admission. Native/resource preparation, services, legacy callbacks,
    worker and effect checkpoints retain the same authority. The caller must keep
    the actual parent-resource lease for the runtime lifetime; its own original
    Artifact source-read lease remains separate. Captured metadata alone grants
    no execution. *)
val build_captured_artifact
  :  captured:Captured_recurrence_source.t
  -> authority:Delegation_authority.t
  -> construction_checkpoint:(unit -> (unit, Agent_protocol.Error.t) result) option
  -> startup:Chat_response.Startup_compilation.t option
  -> legacy_construction:legacy_construction option
  -> agent_source:Captured_agent_runner.Source_binding.t option
  -> script_preparation:Script_preparation.t option
  -> native_registrations:Chat_response.Agent_runtime.native_registration list
  -> services:extension_services option
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

(** Compatibility alias for {!Model_job_event.legacy}. Runtime enqueue selects
    {!Model_job_event.render}'s format from its actual compiled Manager; the
    Actor independently uses the same projection for its exact queue append.
    This allocates no execution or continuation authority. *)
val model_job_event
  :  Agent_protocol.Job.t
  -> (Chatml.Chatml_lang.value, Agent_protocol.Error.t) result

(** Product-returning siblings share the sole implementation of the existing
    build families and source checks. [ancestor_capabilities] is the original
    retained higher-ancestor binding view supplied by the host, preserved through
    every Product binding; [None] supplies no higher-ancestor view. It does not
    replace current delegation authorization. [ancestor_sources] retains matching
    physical Source scopes on that SAME Product, including later native loans;
    [None] supplies no higher Source view. Required [selected_source] Some is
    checked against the actual family/artifact before allocation and retained at
    construction; None preserves direct callers but [Product.selected_source]
    then returns unavailable.
    Existing build functions retain sole-product close ownership; these host
    siblings return the actual first binding for Factory lease attachment. *)
val build_product
  :  ancestor_capabilities:ancestor_capabilities option
  -> ancestor_sources:ancestor_sources option
  -> selected_source:Source.t option
  -> construction_checkpoint:(unit -> (unit, Agent_protocol.Error.t) result) option
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
  -> (Product.t * Product.Binding.t, Agent_protocol.Error.t) result

val build_product_with_extensions
  :  ancestor_capabilities:ancestor_capabilities option
  -> ancestor_sources:ancestor_sources option
  -> selected_source:Source.t option
  -> construction_checkpoint:(unit -> (unit, Agent_protocol.Error.t) result) option
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
  -> (Product.t * Product.Binding.t, Agent_protocol.Error.t) result

val build_product_captured_artifact
  :  ancestor_capabilities:ancestor_capabilities option
  -> ancestor_sources:ancestor_sources option
  -> selected_source:Source.t option
  -> captured:Captured_recurrence_source.t
  -> authority:Delegation_authority.t
  -> construction_checkpoint:(unit -> (unit, Agent_protocol.Error.t) result) option
  -> startup:Chat_response.Startup_compilation.t option
  -> legacy_construction:legacy_construction option
  -> agent_source:Captured_agent_runner.Source_binding.t option
  -> script_preparation:Script_preparation.t option
  -> native_registrations:Chat_response.Agent_runtime.native_registration list
  -> services:extension_services option
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
  -> (Product.t * Product.Binding.t, Agent_protocol.Error.t) result

val build_product_generated
  :  ancestor_capabilities:ancestor_capabilities option
  -> ancestor_sources:ancestor_sources option
  -> selected_source:Source.t option
  -> construction_checkpoint:(unit -> (unit, Agent_protocol.Error.t) result) option
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
  -> (Product.t * Product.Binding.t, Agent_protocol.Error.t) result

val build_product_authored_child
  :  ancestor_capabilities:ancestor_capabilities option
  -> ancestor_sources:ancestor_sources option
  -> selected_source:Source.t option
  -> construction_checkpoint:(unit -> (unit, Agent_protocol.Error.t) result) option
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
  -> (Product.t * Product.Binding.t, Agent_protocol.Error.t) result
