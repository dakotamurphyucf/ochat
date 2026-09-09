open! Core
module CM = Prompt.Chat_markdown
module Moderation = Moderation
module Runtime = Chatml_host_runtime
module Res = Openai.Responses

module Registry : sig
  type artifact
  type t

  val empty : t
  val artifact_count : t -> int

  (** [compile_script registry script] returns the cached compiled artifact for
      [script] or compiles it once and caches the result. *)
  val compile_script
    :  ?surface:Chatml.Chatml_builtin_surface.surface
    -> t
    -> CM.script
    -> (t * artifact, string) result

  (** [of_elements registry elements] compiles any moderator scripts declared
      by [elements]. *)
  val of_elements
    :  ?surface:Chatml.Chatml_builtin_surface.surface
    -> t
    -> CM.top_level_elements list
    -> (t * artifact option, string) result

  (** Bind already validated versioned programs without recompiling or executing.
      The artifact retains the complete definition and exact tool/program bindings. *)
  val of_definition
    :  t
    -> Extension_compiler.definition
    -> (t * artifact option, string) result

  val script_id : artifact -> string
  val source_hash : artifact -> string
end

(** Execution and snapshot/queue access share a process-local execution gate.
    Independent Eio callers serialize. Reentering a held owner or completing a
    cross-owner acquisition cycle returns an explicit error before handler
    execution. Synchronous legacy calls remain supported when uncontended. *)
type t

type pending_ui_request = Runtime.pending_ui_request =
  | Ask_text of { prompt : string }
  | Ask_choice of
      { prompt : string
      ; choices : string array
      }

type subscription

(** [subscribe_committed_changes t ~on_wakeup] subscribes to changes committed
    after subscription. Each change is enqueued before [on_wakeup] runs.
    Wakeup exceptions are isolated from moderation commits. Subscriptions do
    not replay changes restored from snapshots. *)
val subscribe_committed_changes : t -> on_wakeup:(unit -> unit) -> subscription

(** [drain_committed_changes subscription] removes and returns pending changes
    in commit order. *)
val drain_committed_changes : subscription -> Moderation.Overlay_change.t list

(** [unsubscribe subscription] ends [subscription]. It is idempotent and drops
    pending changes. *)
val unsubscribe : subscription -> unit

(** [overlay_revision t] returns the installed identity-overlay revision. *)
val overlay_revision : t -> int

(** Exact compiled v1 source identity for deferred invocation observations.
    Legacy moderators have no invocation observer. This does not enter the
    manager or read mutable script state. *)
val invocation_observer : t -> Agent_protocol.Invocation.observer option

(** The exact admitted definition retained by this v1 manager. Hosts use it to
    bind per-event tool scopes to the same compiled source and native registry.
    Legacy managers return None. No script state is evaluated or borrowed. *)
val extension_definition : t -> Extension_compiler.definition option

(** [create ~artifact ~capabilities ?snapshot ()] instantiates a fresh runtime
    session for [artifact], optionally restoring persisted durable state. *)
val create
  :  artifact:Registry.artifact
  -> capabilities:Moderation.Capabilities.t
  -> ?on_process_run:
       (Runtime.session
        -> command:string
        -> args:Chatml.Chatml_lang.value
        -> (string, string) result)
  -> ?snapshot:Session.Moderator_snapshot.t
  -> unit
  -> (t, string) result

val create_entries
  :  artifact:Registry.artifact
  -> capabilities:Moderation.Capabilities.t
  -> allocator:History_entry.Allocator.t
  -> ?on_process_run:
       (Runtime.session
        -> command:string
        -> args:Chatml.Chatml_lang.value
        -> (string, string) result)
  -> ?snapshot:Session.Moderator_state.Identity_snapshot.t
  -> unit
  -> (t, string) result

(** [uses_allocator t allocator] is [true] when [t] commits canonical
    moderator entries through [allocator]. *)
val uses_allocator : t -> History_entry.Allocator.t -> bool

(** [history_allocator t] returns the allocator used by entry-native
    moderation, if configured. *)
val history_allocator : t -> History_entry.Allocator.t option

(** Read the termination state under the owner execution lock. This is an
    observation, not an authorization reservation across subsequent yields. *)
val is_halted : t -> (bool, string) result

(** [handle_event t ... event] projects the current context, invokes the
    moderator runtime, updates the durable overlay, and returns only the newly
    committed outcome for this host event. Calls that execute, resume, drain,
    enqueue, or snapshot the same manager are serialized. [skip_if_halted]
    defaults to false. When true, a halted runtime returns an End_session request
    without invoking the handler or changing state. The check is made under the
    execution lock, including after waiting for another event to finish. *)
val handle_event
  :  ?skip_if_halted:bool
  -> t
  -> session_id:string
  -> now_ms:int
  -> history:Res.Item.t list
  -> available_tools:Res.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> event:Moderation.Event.t
  -> (Moderation.Outcome.t, string) result

val handle_event_entries
  :  ?skip_if_halted:bool
  -> t
  -> session_id:string
  -> now_ms:int
  -> history:History_entry.t list
  -> available_tools:Res.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> event:Moderation.Event.t
  -> (Moderation.Outcome.t, string) result

(** Execute an ordinary v1 event with a prospective durable state handoff.
    [authorize] runs under the execution lock before handler execution; the host
    must recheck its event ownership and installed source there. [on_tool_call]
    is required and scoped to this execution, with no legacy callback fallback.
    It must enforce current capabilities, policy and durable child ownership.

    All local validation precedes [prepare_event], which receives the complete
    prospective state, queued events, halt and identity overlay. The host must
    atomically persist that snapshot with the event's receipt and runtime intent,
    returning an infallible, non-yielding installer. Error, exception or
    cancellation before commit restores serializable state and discards local
    effects; external effects are not undone or retried. Callbacks must not
    re-enter the manager. The host owns cancellation-safe persistence.

    Internal_event must contain the v1 [Internal_event(tagged_json)] envelope,
    not an arbitrary legacy event value. This delivers the supplied event; it
    does not dequeue or acknowledge an existing queued event. Tool_invoked and
    Tool_observed require their dedicated APIs. Halted sessions and legacy UI
    continuations are rejected. This engine boundary does not acquire an actor
    borrow, impose a host deadline or provide interactive permission ownership. *)
val handle_event_entries_transactional
  :  t
  -> session_id:string
  -> now_ms:int
  -> history:History_entry.t list
  -> available_tools:Res.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> event:Moderation.Event.t
  -> authorize:(unit -> (unit, string) result)
  -> on_tool_call:
       (name:string
        -> args:Jsonaf.t
        -> (Moderation.Capabilities.tool_call_result, string) result)
  -> prepare_event:
       (outcome:Moderation.Outcome.t
        -> snapshot:Session.Moderator_state.Identity_snapshot.t
        -> (unit -> unit, string) result)
  -> (Moderation.Outcome.t, string) result

(** Transactionally consume one queued v1 internal event. Shares validation,
    scoped tool execution and prospective persistence with
    [handle_event_entries_transactional]. [authorize] receives the detached
    selected envelope under the manager lock; the host must validate it against
    its durable queue and claim ownership before handler execution.

    [prepare_event]'s snapshot removes exactly that head, preserves the tail,
    and appends any new emits. The live queue changes only after this preparation
    succeeds. Failure/cancellation leaves state and the entire queue untouched.
    The host must persist failed/interrupted claims to prevent replay of external
    effects; this method does not retry, claim or retire failures itself.
    Returns [Ok None] for an empty queue without calling either callback. *)
val handle_next_event_entries_transactional
  :  t
  -> session_id:string
  -> now_ms:int
  -> history:History_entry.t list
  -> available_tools:Res.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> authorize:(event:Session.Snapshot.t -> (unit, string) result)
  -> on_tool_call:
       (name:string
        -> args:Jsonaf.t
        -> (Moderation.Capabilities.tool_call_result, string) result)
  -> prepare_event:
       (outcome:Moderation.Outcome.t
        -> snapshot:Session.Moderator_state.Identity_snapshot.t
        -> (unit -> unit, string) result)
  -> (Moderation.Outcome.t option, string) result

(** Execute the dedicated extensibility-v1 Tool_invoked event under the manager
    lock. Only a dispatched invocation matching a prepared tool owned by this
    moderator is accepted. [prepare_resolution] receives an immutable prospective
    identity snapshot (new state, full queued events, halt and overlay), allowing
    the host to persist it atomically with [resolved]. All local validation and
    serialization precede this callback. Failure discards buffered state and
    resolution/overlay effects; its returned installer must not fail or yield.
    The callback runs under the manager lock and must not re-enter it. The host
    owns cancellation-safe persistence. This does not perform actor borrowing, authorization, durable
    publication, post-tool routing or terminal-error reconciliation. The owning
    service must supply those boundaries before exposing a model-visible tool.
    [validate_work] checks current Pending work ownership without side effects.
    [authorize] rechecks current authority after acquiring the manager lock and
    validating the invocation, before executing the handler. It must not re-enter
    this manager. Hosts using queued owner handoffs must supply this check.
    [on_tool_call] overrides the legacy callback only during this invocation,
    under the execution lock, and is restored on success, failure or cancellation.
    The host must bind it to the active parent's selected capabilities, persistence
    and policy. It must not synchronously re-enter the manager for pre/post hooks. *)
val handle_invocation_entries
  :  ?authorize:(unit -> (unit, string) result)
  -> ?on_failure:(Moderator_invocation.failure -> unit)
  -> ?on_tool_call:
       (name:string
        -> args:Jsonaf.t
        -> (Moderation.Capabilities.tool_call_result, string) result)
  -> t
  -> invocation:Agent_protocol.Invocation.t
  -> history:History_entry.t list
  -> available_tools:Res.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> now_ms:int
  -> validate_work:(Agent_protocol.Invocation.work -> (unit, string) result)
  -> prepare_resolution:
       (resolved:Agent_protocol.Invocation.t
        -> outcome:Moderation.Outcome.t
        -> snapshot:Session.Moderator_state.Identity_snapshot.t
        -> (unit -> unit, string) result)
  -> (Agent_protocol.Invocation.t * Moderation.Outcome.t, string) result

(** Deliver a source-bound, already claimed nested outcome through Tool_observed.
    It has no provider call ID and cannot resolve the original invocation again.
    The host must hold the exclusive actor borrow and persist [observed] with the
    prospective snapshot in [prepare_observation], returning an infallible,
    non-yielding installer. Local state rolls back on error; external effects do
    not. This method does not itself claim, retry or schedule observations.
    [retain_follow_up] additionally stores coalesced runtime requests in the
    observation acknowledgement. Hosts using it must durably apply that intent
    with the scheduling/stop transition; they must not independently replay both
    the returned requests and the stored intent. Defaults false for the existing
    foreground worker path; idle integration requires the retained-intent path.
    Optional Tool.call routing has the same scoped authority requirements as
    [handle_invocation_entries]. *)
val handle_observation_entries
  :  ?on_tool_call:
       (name:string
        -> args:Jsonaf.t
        -> (Moderation.Capabilities.tool_call_result, string) result)
  -> ?retain_follow_up:bool
  -> t
  -> invocation:Agent_protocol.Invocation.t
  -> history:History_entry.t list
  -> available_tools:Res.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> now_ms:int
  -> prepare_observation:
       (observed:Agent_protocol.Invocation.t
        -> outcome:Moderation.Outcome.t
        -> snapshot:Session.Moderator_state.Identity_snapshot.t
        -> (unit -> unit, string) result)
  -> (Moderation.Outcome.t, string) result

(** [pending_ui_request t] exposes the current live-session approval request,
    if the runtime is suspended waiting for UI input. *)
val pending_ui_request : t -> pending_ui_request option

(** [resume_ui_request t ~response] resumes the suspended moderator execution
    with [response] and returns any newly committed moderation outcomes from
    that resumed execution. *)
val resume_ui_request : t -> response:string -> (Moderation.Outcome.t list, string) result

(** [drain_internal_events t ...] replays queued internal events FIFO through
    phase [internal_event], stopping after [max_events]. *)
val drain_internal_events
  :  ?max_events:int
  -> t
  -> session_id:string
  -> now_ms:int
  -> history:Res.Item.t list
  -> available_tools:Res.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> (Moderation.Outcome.t list, string) result

val drain_internal_events_entries
  :  ?max_events:int
  -> t
  -> session_id:string
  -> now_ms:int
  -> history:History_entry.t list
  -> available_tools:Res.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> (Moderation.Outcome.t list, string) result

val effective_entries : t -> History_entry.t list -> Moderation.Effective_entry.t list
val effective_history_entries : t -> History_entry.t list -> History_entry.t list

(** Reconstruct the effective conversation from a committed identity snapshot,
    without creating a runtime or executing moderator code. *)
val effective_entries_of_snapshot
  :  Session.Moderator_state.Identity_snapshot.t
  -> History_entry.t list
  -> (Moderation.Effective_entry.t list, string) result

(** [effective_items t history] applies the durable moderator overlay to the
    projected canonical history. *)
val effective_items : t -> Res.Item.t list -> Moderation.Item.t list

(** [effective_history t history] applies the durable moderator overlay and
    reconstructs OpenAI response items for model input and downstream
    consumers. *)
val effective_history : t -> Res.Item.t list -> (Res.Item.t list, string) result

(** [snapshot t] extracts a persisted moderator snapshot after any active
    manager execution has completed. *)
val snapshot : t -> (Session.Moderator_snapshot.t, string) result

val identity_snapshot : t -> (Session.Moderator_state.Identity_snapshot.t, string) result

(** Prepare removal of exactly one queue head without executing a handler. The
    complete live checkpoint must equal [expected]. [prepare] must hold an actor
    retirement borrow and atomically persist failed-head retirement with the
    supplied checkpoint. Return an infallible, non-yielding installer. Rejection
    leaves the entire live state/queue unchanged. This engine helper does not
    identify failed receipts or authorize retirement by itself. *)
val retire_queued_event_entries
  :  t
  -> expected:Session.Moderator_state.Identity_snapshot.t
  -> prepare:
       (snapshot:Session.Moderator_state.Identity_snapshot.t
        -> (unit -> unit, string) result)
  -> (unit, string) result

(** [enqueue_internal_event t event] enqueues [event] after any active manager
    execution has completed for later replay via {!drain_internal_events}. *)
val enqueue_internal_event : t -> Chatml.Chatml_lang.value -> (unit, string) result

(** Prepare an external event append under the manager execution lock. The host
    must own the actor checkpoint gate and atomically save the new checkpoint
    with the delivery receipt, comparing [before] to the current durable state.
    Rejection leaves the live queue unchanged. The event is detached from caller
    mutable values before preparation. Save and local installation are protected
    against cancellation; this does not run the event's handler. Versioned managers
    currently accept only validated Internal_event envelopes; legacy job events
    require their versioned completion adapter before admission. *)
val enqueue_internal_event_entries
  :  t
  -> event:Chatml.Chatml_lang.value
  -> prepare:
       (before:Session.Moderator_state.Identity_snapshot.t
        -> snapshot:Session.Moderator_state.Identity_snapshot.t
        -> (unit, string) result)
  -> (Session.Moderator_state.Identity_snapshot.t, string) result
