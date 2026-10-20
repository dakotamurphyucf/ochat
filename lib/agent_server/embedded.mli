open! Core

(** Process-local host using the same daemon composition, session actor, and
    client protocol path as networked sessions. *)

type options =
  { prompt_file : string
  ; workspace : string
  ; tool_dir : string
  ; home : string option
  ; storage : Local_storage.t
  ; start_immediately : bool
  ; permission_profile : Config.Permission_profile.t
  ; attachment_mode : Agent_protocol.Session.attachment_mode
  ; event_capacity : int
  }

type t

val default_permission_profile : Config.Permission_profile.t

(** Pure specification for a new session in the catalog installed by
    [open_local_host] with these options. Uses the existing embedded prompt and
    workspace identities, process-bound liveness, selected persistence and
    start intent. This constructs no host, session, attachment or authority;
    normal protocol admission still validates it against the actual host. *)
val session_spec
  :  options
  -> (Agent_protocol.Session.Spec.t, Agent_protocol.Error.t) Result.t

(** Explicit process-local authorization permits the compiled shell manifests to
    load. Ordinary tool approval, shell command policy, capability checks and
    sandbox enforcement remain unchanged. With [false], startup requires a grant.
    This does not change daemon configuration or persist an operator grant. *)
val interactive_permission_profile
  :  authorize_shell_manifest:bool
  -> Config.Permission_profile.t

(** [daemon_options] supplies the shared runtime's trusted host configuration,
    including provider adapters, policy and internal extension qualification.
    Defaults match [Daemon.default_options]. The embedded host always derives
    extension host metadata from explicit [storage], keeps process-bound liveness and
    starts no network listener. A durable data root preserves data; it does not
    keep jobs running after the embedding process exits. [local_policy] is an
    opt-in parsed local fragment: startup binds all its principal selectors to
    the one Store-owned embedded operator before source/catalog work, and adds
    only its requested Developer method scopes to that same actual principal.
    It conflicts with a separate [project_sources] argument rather than
    overriding it. Omission preserves the generated local Config and scopes.
    Startup initializes the Unix cryptographic RNG before allocating IDs or a
    transient data root. *)
val start
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> ?daemon_options:Daemon.options
  -> ?root_invocation:
       Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.t
  -> ?project_sources:Project_source_configuration.t
  -> ?local_policy:Config_validator.Embedded_local.t
  -> ?authoring_package_files:string list
       (** Absolute paths captured through the shared bounded loader before store
         creation. Packages configure the same host as daemon configuration;
         they do not enable extensions or widen selected tool authority. *)
  -> ?authoring_budget:Chat_response.Authoring_validation.context_budget
       (** Query defaults/ceiling and automatic insertion budget, shared with
           daemon configuration and inherited by delegated sessions. *)
  -> options
  -> (t, Agent_protocol.Error.t) result

(** A host owner independent of session creation. Operator_only starts no
    recovered runtime/scheduler and rejects activating session operations.
    Durable local operator identity requires private OS-owned root permissions. *)
type host

val open_host
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> ?daemon_options:Daemon.options
  -> startup_mode:Daemon.startup_mode
  -> config:Config.t
  -> tool_dir:string
  -> home:string
  -> event_capacity:int
  -> unit
  -> (host, Agent_protocol.Error.t) result

(** Reuse the exact local composition and actual host cleanup owner without
    creating a replacement session. [root_invocation] overrides the trusted daemon
    option for new captures only; omission preserves that option (Ui by default).
    [project_sources] supplies existing operator-selected source declarations to
    normal native admission. Omission keeps local-only authority. Absolute roots
    and Git grants are validated by the existing source host; labels, artifacts
    and the current directory supply no authority. Retained Spec/code/target win
    on attachment regardless of the current host selection.

    [start] and [with_local_host] forward these same choices. *)
val open_local_host
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> ?daemon_options:Daemon.options
  -> ?root_invocation:
       Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.t
  -> ?project_sources:Project_source_configuration.t
  -> ?local_policy:Config_validator.Embedded_local.t
  -> ?authoring_package_files:string list
  -> ?authoring_budget:Chat_response.Authoring_validation.context_budget
  -> options
  -> (host, Agent_protocol.Error.t) result

(** Caller owns open host. Select exact current anchor before attachment. Saved
    prompt/workspace/permissions are authoritative; mode only requests attachment
    access and cannot replace them.
    Stopped selection does not start. Failure leaves caller owning this host. *)
val attach_retained
  :  host
  -> mode:Agent_protocol.Session.attachment_mode
  -> expected:Agent_protocol.Session_lifecycle.Expected.t
  -> (t, Agent_protocol.Error.t) result

(** Scope one local host through a callback without transferring its owner out.
    On rejection/exception preserve the original failure and protected cleanup;
    failed cleanup fails the actual owning Switch with retained diagnostics.
    On success close this host before returning the callback value. The callback
    must not return the host, its connection, or resources owned by it. *)
val with_local_host
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> ?daemon_options:Daemon.options
  -> ?root_invocation:
       Chat_response.Script_contract_plan.Program.Role.Moderator_invocation.t
  -> ?project_sources:Project_source_configuration.t
  -> ?local_policy:Config_validator.Embedded_local.t
  -> ?authoring_package_files:string list
  -> ?authoring_budget:Chat_response.Authoring_validation.context_budget
  -> options
  -> f:(host -> ('a, Agent_protocol.Error.t) Result.t)
  -> ('a, Agent_protocol.Error.t) Result.t

(** Borrow the host-owned, already initialized local connection on its owning
    Eio domain. Developer and REPL methods are usable only when installed and
    selected by this physical Initialize. Claim at most one notification lease;
    host cleanup closes this connection before releasing daemon and Store. *)
val host_connection : host -> Agent_client.Connection.t

(** Trusted local composition currently grants the same compiled local scopes
    as [start]; it is not a configurable network-principal mapping. Operator-only
    dispatch further restricts executable methods. Provider scopes belong to the
    separate operator service contract. *)
val host_principal : host -> Agent_protocol.Principal.t

val host_dispatcher : host -> Dispatcher.t

module Script_selection : sig
  (** Actual frozen source classification, not command or native authority. *)
  type t = private
    { session : Agent_protocol.Session.t
    ; moderator : Chat_response.Script_contract_plan.Program.Role.t option
    }
end

(** Read the exactly anchored session through this host's genuine local principal
    and the nonactivating registry reader. Restore its actual definition-affine
    revision and use Spec's retained root invocation with the same fixed host
    preparation policy. Validate the actual planner/custody, then project the
    moderator role using native preparation's precedence: one versioned moderator
    before legacy, otherwise ordinary/hook-only. Multiple versioned moderators,
    missing/foreign custody and legacy absence of authoritative Parsed reject.

    Recheck the complete lifecycle/session anchor before delivery. No session
    attachment, runtime activation, initializer, provider, source recapture or
    filename/digest inference. Subsequent command admission still validates its
    actual positions and capabilities. Policy/Startup and original checkpoint
    exception lifetimes follow Runtime_script_preparation.with_revision_plan. *)
val session_script_selection
  :  host
  -> expected:Agent_protocol.Session_lifecycle.Expected.t
  -> checkpoint:(unit -> unit)
  -> (Script_selection.t, Agent_protocol.Error.t) Result.t

(** Second phase after the caller's exact Session.start on its existing handle.
    Join the actual retained runtime's lifecycle activation through its existing
    Execution_gate; completed activation is never replayed. Requires the same
    session/generation/captured source, live read-write attachment and current
    local principal. Phase two selects its current retained configuration/target
    at entry and rejects their changes during the join. Reject owner changes, failed/halted or
    busy outcome. Authorized activation may change history; this returns a NEW
    admission anchor for the subsequent explicit user message. It is not a retry
    or a claim that the start reply remains current. Caller must wait for this
    exact position on its sole reader and retain the final command CAS.

    Borrow/join only this runtime; do not drain unrelated work or recapture code.
    No borrowed checkpoint runs after the final actor observation. Original
    callback failures/cancellation propagate and the lease is released first. *)
val complete_script_activation
  :  host
  -> started:Agent_protocol.Session.t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> checkpoint:(unit -> unit)
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) Result.t

(** Open a distinct, uninitialized caller-owned connection on this host's
    owning Eio domain. The caller must Initialize it, close it, and own its sole
    notification lease. Closing this connection does not close the host's other
    connections. Typed Developer/REPL methods require installed and selected
    features on its own physical Initialize. *)
val connect_host : host -> (Agent_client.Connection.t, Agent_protocol.Error.t) result

val close_host : host -> unit
val connection : t -> Agent_client.Connection.t
val session_id : t -> Agent_protocol.Id.Session.t
val attachment : t -> Agent_protocol.Session.Attachment.t
val dispatcher : t -> Dispatcher.t
val principal : t -> Agent_protocol.Principal.t

(** Compatibility API: caller must retain an open host; closed hosts raise. *)
val connect : t -> Agent_client.Connection.t

val close_connection : t -> Connection_context.t -> unit
val close : t -> unit

(** Open a distinct, uninitialized connection owned by this embedded host. The
    caller borrows it until host close, must Initialize it before typed
    Developer/REPL use, and may claim its sole notification lease. Do not close
    or escape it independently. Acquire and close on the host owning Eio domain.
    Acquisition/adoption do not yield on that domain, so closed admission rejects
    acquisition before constructing a client. Existing [connect] retains its
    caller-owned contract. All host-owned connection cleanup precedes daemon/Store
    release and preserves original failure plus cleanup diagnostics. *)
val connect_owned : t -> (Agent_client.Connection.t, Agent_protocol.Error.t) result

(** Borrow this started session within its existing ownership Switch and close its
    actual host before returning. Callback must not escape usable session-owned
    resources or recursively close the host. Result rejection, exception and
    cancellation preserve the original primary/backtrace and secondary cleanup
    diagnostic using the concrete host owner; repeated cleanup failure fails that
    owning scope. *)
val with_session
  :  t
  -> f:(t -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Promote the exact retained owner after current local-principal authorization.
    Stopped selection does not start a session; reads/replay/gate commits never call it. *)
val select_session
  :  host
  -> Agent_protocol.Session_lifecycle.Expected.t
  -> (unit, Agent_protocol.Error.t) Result.t
