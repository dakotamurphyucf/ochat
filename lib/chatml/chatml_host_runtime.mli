(** Host-side session runtime for transactional ChatML scripts.

    This module sits above the minimal ChatML core and provides the
    session-oriented runtime described in the moderator design:

    - scripts are compiled once and instantiated per session,
    - [on_event] returns {!Chatml_lang.task} values,
    - the host interprets those tasks via an operation registry,
    - local transactional effects are buffered and committed on success.

    The runtime is intentionally generic: embedders provide concrete
    handlers for tool/model/scheduling integration while the module
    supplies the common execution model.

    The optional UI-only notification and ask-user capability layer is
    documented in [docs-src/chatml-ui-host-capabilities.md]. It remains an
    explicit host capability rather than a generic assumption of every
    moderator runtime embedding.

    Approval suspension is exposed through a host-visible pending-request and
    resume boundary. While UI input is pending, ordinary [handle_event]
    progression is blocked and the suspended handler remains uncommitted until
    resume succeeds. *)

open Chatml.Chatml_lang
module Builtin_surface = Chatml.Chatml_builtin_surface

(** Compiled script artifact produced by {!compile_script}. *)
type compiled_script

(** Per-session runtime instance holding durable state and execution
    queues. *)
type session

(** Host-visible pending UI approval request for a suspended live session. *)
type pending_ui_request =
  | Ask_text of { prompt : string }
  | Ask_choice of
      { prompt : string
      ; choices : string array
      }

(** Diagnostic log level used by the default [Log.*] operation family. *)
type log_level =
  | Debug
  | Info
  | Warn
  | Error_level

(** Structured local turn mutations used by the default [Turn.*]
    handlers.

    The public ChatML moderator surface prefers item-oriented names such
    as [Turn.append_item], [Turn.replace_item], and [Turn.delete_item].
    The legacy [*_message] builtin names remain as aliases and decode to
    these same runtime constructors. *)
type turn_effect =
  | Prepend_system of string
  (** Request a prepended developer instruction. The legacy effect name remains
      compatible with existing ChatML scripts. *)
  | Append_message of value
  | Replace_message of string * value
  | Delete_message of string
  | Halt of string

(** Structured tool-moderation actions used by the default [Tool.*]
    moderation handlers. *)
type tool_moderation =
  | Approve
  | Reject of string
  | Rewrite_args of value
  | Redirect of string * value

(** Structured local effects that can be decoded from committed
    transactional operations after {!handle_event} succeeds. *)
type local_effect =
  | Turn_effect of turn_effect
  | Tool_moderation_effect of tool_moderation
  | Ui_notification of string
  | Emit_internal_event of value
  | Request_compaction
  | Request_turn
  | End_session of string

(** [prepare_commit] validates a completed transaction before the runtime
    commits it. The returned function must be infallible and installs
    host-owned state immediately after the runtime commit. *)
type prepare_commit = local_effects:eff list -> (unit -> unit, string) result

(** Prospective committed state, including retained queued events followed by
    events emitted in this transaction. [handle_next_queued_event] excludes the
    consumed head. Values are borrowed, not copied; the
    callback must not mutate them or re-enter the runtime. *)
type transaction =
  { new_state : value
  ; local_effects : eff list
  ; queued_events : value list
  ; halted : bool
  }

(** Runs after state validation and the legacy [prepare_commit] callback, before
    any runtime commit or installer. A durable host can serialize this proposal
    and persist it atomically with its own records here. All fallible validation
    must precede that persistence. On success, return an infallible installer
    that does not yield. The host owns cancellation-safe persistence and must
    serialize access throughout the callback. On failure no installer runs.
    When combined with this hook, [prepare_commit] must only validate/prepare,
    not publish or persist independently. *)
type prepare_transaction = transaction -> (unit -> unit, string) result

(** Runtime classification of task operations. *)
type op_kind =
  | Local_transactional
  | Local_transactional_with_result of { rollback : value list -> unit }
  (** Records [result :: original_arguments] under the same operation name.
        Useful for host-issued reservation IDs that must be associated with the
        precise surviving effect after Task.catch rollback. The operation may
        reserve resources but must not materialize external work before commit.
        [rollback] receives the recorded arguments, newest first, for effects
        discarded by Task.catch, before its recovery handler runs. It must be
        infallible, idempotent and only release provisional resources. The host
        still owns cleanup on whole-transaction failure, cancellation or rejection.
        Like other transactional operations, this kind cannot be spawned. *)
  | External_sync
  | External_async
  | Diagnostic

(** Closed, default-off standalone operation filter for a host-selected tool
    entrypoint. This admits only the canonical Model.spawn operation when it
    is already installed as External_async; it grants no actor, provider or
    execution authority. Ordinary callers omit the option. *)
type scoped_async_operation = Model_spawn

(** Definition of a host operation exposed to ChatML tasks. *)
type op_def =
  { name : string
  ; kind : op_kind
  ; perform : session -> value list -> (value, string) result
  ; phase_check : string -> (unit, string) result
  }

(** Runtime configuration shared across sessions for a given host/runtime
    embedding. *)
type runtime_config =
  { surface : Builtin_surface.surface
  ; operations : op_def list
  }

(** Bounds task interpretation for one handler call. *)
type execution_limits =
  { fuel : int
  ; max_tasks : int
  }

(** Names of the convention-based script entrypoints. *)
type compiled_entrypoints =
  { initial_state_name : string
  ; on_event_name : string
  }

(** Host callbacks used to build the standard moderator operation
    registry.  Handlers that are left at their defaults either act as
    no-ops for local/diagnostic behavior or return a clear ["... is not
    configured"] error for external integrations. *)
type default_handlers =
  { on_log : session -> level:log_level -> message:string -> (unit, string) result
  ; on_turn_effect : session -> turn_effect -> (unit, string) result
  ; on_tool_moderation : session -> tool_moderation -> (unit, string) result
  ; on_ui_notify : session -> message:string -> (unit, string) result
  ; on_tool_call : session -> name:string -> args:value -> (value, string) result
  ; on_tool_spawn : session -> name:string -> args:value -> (string, string) result
  ; on_model_call : session -> recipe:string -> payload:value -> (value, string) result
  ; on_model_spawn : session -> recipe:string -> payload:value -> (string, string) result
  ; on_process_run : session -> command:string -> args:value -> (string, string) result
  ; on_schedule_after_ms :
      session -> delay_ms:int -> payload:value -> (string, string) result
  ; on_schedule_cancel : session -> id:string -> (unit, string) result
  ; on_request_compaction : session -> (unit, string) result
  ; on_end_session : session -> reason:string -> (unit, string) result
  ; on_request_turn : session -> (unit, string) result
  }

(** Render a log level using the names expected by human-facing
    diagnostics. *)
val string_of_log_level : log_level -> string

(** Phase check that accepts every phase. *)
val allow_all_phases : string -> (unit, string) result

(** Build a phase check that accepts only the listed phase names. *)
val require_phases : string list -> string -> (unit, string) result

(** Default handler bundle used by {!default_operations}. *)
val default_handlers : default_handlers

(** Construct the standard operation registry for moderator runtimes from a
    bundle of host callbacks. *)
val default_operations : ?handlers:default_handlers -> unit -> op_def list

(** Convenience constructor for a runtime configuration using the standard
    moderator surface and default operation registry. *)
val default_runtime_config
  :  ?surface:Builtin_surface.surface
  -> ?handlers:default_handlers
  -> unit
  -> runtime_config

(** Parse, typecheck, and resolve a script once without evaluating initializers
    or performing tasks. The default surface is {!Builtin_surface.moderator_surface}.
    [required_bindings] checks final bindings against host-provided type schemes;
    repeated type variables share one instantiation across the whole contract.
    Missing bindings, wrong arity and incompatible argument/result types fail
    compilation. Requirements use the host type language, so source aliases cannot
    replace the expected types. This check does not authorize execution or bound
    compiler work. [checkpoint] runs before and between compiler stages, including
    after diagnostic formatting. Its exceptions propagate to the caller; it must
    not mutate compiler state. Hosts can use it for cooperative cancellation. *)
val compile_script
  :  ?checkpoint:(unit -> unit)
  -> ?surface:Builtin_surface.surface
  -> ?required_bindings:(string * Chatml.Chatml_builtin_spec.ty) list
  -> source:string
  -> unit
  -> (compiled_script, string) result

type compilation_stage =
  | Parse
  | Typecheck
  | Link
[@@deriving sexp, equal]

(** Original parser/typechecker location and message, plus the legacy rendered
    diagnostic. Spans refer to the exact submitted source, with no path loading. *)
type compilation_diagnostic = private
  { stage : compilation_stage
  ; message : string
  ; span : Source.span option
  ; formatted : string
  ; diagnostic : Chatml_diagnostic.t
  }
[@@deriving sexp_of]

(** Derive all projections from the canonical failure and render at most16KiB.
    Captured sites retain their own bytes; optional text is only a legacy unowned
    adapter. Checkpoints/cancellation propagate; no inventory/control retained. *)
val compilation_diagnostic
  :  Chatml_diagnostic.t
  -> stage:compilation_stage
  -> source_text:string option
  -> checkpoint:(unit -> unit)
  -> compilation_diagnostic

(** Same non-executing compilation as [compile_script], retaining structured
    diagnostic data for validation/authoring consumers. *)
val compile_script_detailed
  :  ?checkpoint:(unit -> unit)
  -> ?surface:Builtin_surface.surface
  -> ?required_bindings:(string * Chatml.Chatml_builtin_spec.ty) list
  -> source:string
  -> unit
  -> (compiled_script, compilation_diagnostic) result

(** Compile exact captured bytes without executing initializers. Structured
    failures retain the captured identity/revision; no live source lookup. *)
val compile_captured_script
  :  Chatml_source.t
  -> ?checkpoint:(unit -> unit)
  -> ?surface:Builtin_surface.surface
  -> ?required_bindings:(string * Chatml.Chatml_builtin_spec.ty) list
  -> unit
  -> (compiled_script, compilation_diagnostic) result

(** Surface recorded on a compiled script artifact. *)
val compiled_surface : compiled_script -> Builtin_surface.surface

(** Instantiate a compiled script in a fresh per-session environment and
    load the configured entrypoints. [control] follows closures into later
    events. A persistent owner must supply a control valid for every execution,
    such as a dynamically scoped host proxy, rather than retaining an expired
    initializer budget. Initial state is checked before diagnostic rendering. *)
val instantiate_session
  :  ?control:execution_control
  -> runtime_config
  -> compiled_script
  -> entrypoints:compiled_entrypoints
  -> (session, string) result

(** Same execution and callback contracts, preserving the canonical structured
    failure until the caller chooses rendering. Cancellation/control exceptions
    propagate after the existing cleanup. *)
val instantiate_session_detailed
  :  ?control:execution_control
  -> runtime_config
  -> compiled_script
  -> entrypoints:compiled_entrypoints
  -> (session, Chatml_diagnostic.t) result

(** Evaluate a fresh program instance and invoke a task-returning entrypoint
    directly, without initial_state/on_event conventions or lifecycle events.
    Only diagnostic and synchronous external operations are installed by default.
    Supplying [prepare_result] also installs result-recording transactional
    operations. After interpretation, the callback receives the final value and
    surviving effects, with recorded results prepended to their arguments. It must
    validate the value/effects and prepare their owning commit before returning an
    infallible, non-yielding installer. Rejection skips installation. The host owns
    abort/release of uncommitted reservations on whole-execution failure; catch
    rollback invokes each operation's provisional-resource cleanup callback.
    Ordinary local session effects, background task dispatch and UI suspension
    remain unavailable.
    [control] follows the environment into closures, builtin callbacks and task
    continuations. Host cancellation/control exceptions propagate after cleanup.
    The host must compile against the intended surface and enforce authority,
    value/allocation/output limits and result validation. No persistent session
    or model request is created by default. Opting into [Model_spawn] permits
    only an installed, physically authorized Model.spawn handler as an immediate
    external effect. A later failure, catch or discarded task cannot undo a
    completed provider request; the host must retain its call-boundary custody. *)
val run_entrypoint
  :  ?control:execution_control
  -> ?scoped_async_operation:scoped_async_operation
  -> ?limits:execution_limits
  -> ?prepare_result:
       (value:value -> local_effects:eff list -> (unit -> unit, string) result)
  -> runtime_config
  -> compiled_script
  -> entrypoint:string
  -> arguments:value list
  -> unit
  -> (value, string) result

(** Same execution and callback contracts, preserving the canonical structured
    failure until the caller chooses rendering. Cancellation/control exceptions
    propagate after the existing cleanup. *)
val run_entrypoint_detailed
  :  ?control:execution_control
  -> ?scoped_async_operation:scoped_async_operation
  -> ?limits:execution_limits
  -> ?prepare_result:
       (value:value -> local_effects:eff list -> (unit -> unit, string) result)
  -> runtime_config
  -> compiled_script
  -> entrypoint:string
  -> arguments:value list
  -> unit
  -> (value, Chatml_diagnostic.t) result

(** Interpret an already evaluated, retained Task through the same standalone
    task/catch/operation and transactional-result kernel as [run_entrypoint_detailed].
    This entry has no function call, synthetic [on_event], program initializer or
    session event. [environment], [task] and [source] are borrowed from one
    serially owned evaluator; the caller must establish that physical pairing.
    [source] is the retained captured statement which returned the Task and is
    used only for legacy unowned-range formatting. Owned failure sites,
    including sites from older closures or configured modules, render against
    their own exact captured source and are never relabelled to this statement.

    Operation filtering and [prepare_result] have the standalone contract above:
    ordinary session mutations/UI suspension are unavailable, a caught local
    reservation is rolled back, and only surviving transactional effects reach
    the infallible installer. Synchronous external effects already completed
    before a later failure are not undone. [control] checks the final value
    before an installer runs; cancellation and unexpected callback exceptions
    retain their original identity/backtrace. This function supplies no source,
    actor, Task-handle or operation authority. The raw value is lent only to
    this lower runtime's caller; a public REPL receipt must project it.
    [check_now] must be the borrowed nonyielding execution-lifetime/deadline
    check from the same active runner, repeated after any yielding
    [prepare_result] and immediately before its installer. It is not host
    actor/source/Store admission. *)
val run_retained_task_detailed
  :  ?control:execution_control
  -> ?scoped_async_operation:scoped_async_operation
  -> ?limits:execution_limits
  -> ?prepare_result:
       (value:value -> local_effects:eff list -> (unit -> unit, string) result)
  -> runtime_config
  -> environment:env
  -> task:Task.t
  -> source:Chatml_source.t
  -> check_now:(unit -> unit)
  -> unit
  -> (value, Chatml_diagnostic.t) result

(** Initialize the linked value dependency closure once for a persistent session.
    Validate exact static installer metadata and compiled selected names before
    effects. Reuse the existing session/interpreter/events; companions restrict
    external exports, while selected local host bindings use private entry scope.
    Every unit captures [control]; persistent owners supply runner_control inside
    run_scoped and serialize subsequent events. No partial session on failure.
    Owned runtime diagnostics and cancellation contracts match detailed legacy
    APIs. Standard moderator targets use canonical roles; Native custom labels
    require an explicit three-argument task function and matching grouped state
    metadata (Contract.Invocation). No runtime inference/alias solving. No
    filesystem/custody routing or initialization during compilation. *)
val instantiate_prepared_session_detailed
  :  runtime_config
  -> Chatml_linked_program.t
  -> entrypoints:compiled_entrypoints
  -> ?control:execution_control
  -> unit
  -> (session, Chatml_diagnostic.t) result

(** Native two-argument hook variant. Validate authentic event/state session
    metadata before any initializer; validate the live installer and instantiate
    the exact graph once. Invoke its ORIGINAL hook as [hook(event,state)] using
    the existing session transaction/queue/suspension engine. No generated source
    or native callback wrapper. Caller serializes the session and owns control
    lifetime exactly as for instantiate_prepared_session_detailed. *)
val instantiate_prepared_event_state_session_detailed
  :  runtime_config
  -> Chatml_linked_program.t
  -> entrypoints:compiled_entrypoints
  -> ?control:execution_control
  -> unit
  -> (session, Chatml_diagnostic.t) Result.t

(** Fresh linked instance and compiled selected task entrypoint. Shared operation
    filtering, task/catch semantics, reservation cleanup and result installation
    are identical to run_entrypoint_detailed. All initializer work inherits the
    same control; selected names cannot expand the compiled dependency closure.
    Contract.Invocation checks declared task-function shape, argument count and
    registered target kind/role before initializer effects. *)
val run_prepared_entrypoint_detailed
  :  runtime_config
  -> Chatml_linked_program.t
  -> entrypoint:string
  -> arguments:value list
  -> ?control:execution_control
  -> ?scoped_async_operation:scoped_async_operation
  -> ?limits:execution_limits
  -> ?prepare_result:
       (value:value -> local_effects:eff list -> (unit -> unit, string) result)
  -> unit
  -> (value, Chatml_diagnostic.t) result

(** Current durable script state for the session. *)
val current_state : session -> value

(** Current phase while a handler is actively running, if any. *)
val current_phase : session -> string option

(** Pending UI approval request for a suspended session, if one exists. *)
val pending_ui_request : session -> pending_ui_request option

(** Local transactional effects buffered during the current handler
    execution. *)
val pending_local_effects : session -> eff list

(** All committed local transactional effects observed so far for the
    session, in execution order. *)
val committed_local_effects : session -> eff list

(** Decode one committed transactional effect into the structured local
    runtime vocabulary. *)
val decode_local_effect : eff -> (local_effect, string) result

(** Decode committed transactional effects in execution order.  This is
    the preferred way for embedders to inspect local turn mutations,
    tool moderation actions, and runtime requests because it reflects the
    runtime's transactional commit semantics. *)
val decode_local_effects : eff list -> (local_effect list, string) result

(** Buffered internal events currently queued for later delivery. *)
val queued_events : session -> value list

(** Borrow the oldest queued value without removing it. The caller must not
    mutate it and must serialize access through event execution and commit. *)
val peek_queued_event : session -> value option

(** Remove and return the oldest queued internal event, if one exists. *)
val take_queued_event : session -> value option

(** Replace the session's durable runtime state from a persisted snapshot.

    Any committed local effects from prior runs are cleared. *)
val restore
  :  session
  -> state:value
  -> queued_events:value list
  -> halted:bool
  -> (unit, string) result

(** Whether the session has been ended by a committed runtime action. *)
val is_halted : session -> bool

(** Append an internal event to the current handler's transactional output
    buffer.  This is primarily useful for custom host operations. *)
val emit_internal_event : session -> value -> (unit, string) result

(** Request session termination from within the current handler's
    transactional context. *)
val request_session_end : session -> reason:string -> (unit, string) result

(** Deliver a host-selected local operation into the current transaction using
    the same declared operation, phase checks, execution controls and effect
    buffer as script dispatch. External/diagnostic operations are rejected before
    execution. The owner must call this on its active interpreter context after
    joining native work; it is not an asynchronous session mutation API.
    Effects participate in Task.catch restoration, handler rollback, prospective
    snapshot preparation and commit. Outside active interpretation it fails. *)
val perform_local_effect : session -> eff -> (unit, string) result

(** Handle one event:

    - invokes [on_event],
    - interprets the returned task,
    - commits buffered state/effects on success,
    - or rolls back local transactional buffers on failure.

    [copy_state] optionally makes a defensive copy for handler execution, leaving
    the original state untouched until commit. This also prevents mutation of
    older queued payloads sharing state arrays. Failure (including exceptions/
    cancellation) retains the original state. By default state is used by reference
    for legacy callers. This does not undo mutable globals or external effects.
    [prepare_commit]'s returned installer must remain infallible.
    [validate_suspension] runs before installing a legacy UI continuation. An
    error rejects suspension and rolls back the handler just like other failures;
    no pending request is retained. The default preserves legacy UI behavior. *)
val handle_event
  :  ?prepare_commit:prepare_commit
  -> ?prepare_transaction:prepare_transaction
  -> ?validate_state:(value -> (unit, string) result)
  -> ?validate_suspension:(unit -> (unit, string) result)
  -> ?copy_state:(value -> (value, string) result)
  -> ?limits:execution_limits
  -> session
  -> context:value
  -> event:value
  -> (unit, string) result

(** Same execution and callback contracts, preserving the canonical structured
    failure until the caller chooses rendering. Cancellation/control exceptions
    propagate after the existing cleanup. *)
val handle_event_detailed
  :  ?prepare_commit:prepare_commit
  -> ?prepare_transaction:prepare_transaction
  -> ?validate_state:(value -> (unit, string) result)
  -> ?validate_suspension:(unit -> (unit, string) result)
  -> ?copy_state:(value -> (value, string) result)
  -> ?limits:execution_limits
  -> session
  -> context:value
  -> event:value
  -> (unit, Chatml_diagnostic.t) result

(** Handle the oldest queued event, removing it only in the successful state/
    effects commit. Returns [Ok None] for an empty queue. The prospective
    transaction excludes that head and retains the tail followed by new emits.
    Failure leaves the queue untouched; there is no automatic retry.

    [copy_event] must return a detached execution value without mutating the
    borrowed head. Supply [copy_state] for mutable data state too. UI suspension
    is rejected. The caller must serialize queue/state access for the whole
    operation, including host callbacks, and make persistence cancellation-safe.
    A durable host must claim work before external effects and record failure
    disposition; retaining a queue entry alone is not permission to replay it. *)
val handle_next_queued_event
  :  ?prepare_commit:prepare_commit
  -> ?prepare_transaction:prepare_transaction
  -> ?validate_state:(value -> (unit, string) result)
  -> ?copy_state:(value -> (value, string) result)
  -> ?limits:execution_limits
  -> session
  -> context:value
  -> copy_event:(value -> (value, string) result)
  -> (unit option, string) result

(** Same execution and callback contracts, preserving the canonical structured
    failure until the caller chooses rendering. Cancellation/control exceptions
    propagate after the existing cleanup. *)
val handle_next_queued_event_detailed
  :  ?prepare_commit:prepare_commit
  -> ?prepare_transaction:prepare_transaction
  -> ?validate_state:(value -> (unit, string) result)
  -> ?copy_state:(value -> (value, string) result)
  -> ?limits:execution_limits
  -> session
  -> context:value
  -> copy_event:(value -> (value, string) result)
  -> (unit option, Chatml_diagnostic.t) result

(** Resume a suspended UI approval request with a host-supplied response. *)
val resume_ui_request
  :  ?prepare_commit:prepare_commit
  -> ?prepare_transaction:prepare_transaction
  -> ?validate_state:(value -> (unit, string) result)
  -> ?limits:execution_limits
  -> session
  -> response:string
  -> (unit, string) result

(** Same execution and callback contracts, preserving the canonical structured
    failure until the caller chooses rendering. Cancellation/control exceptions
    propagate after the existing cleanup. *)
val resume_ui_request_detailed
  :  ?prepare_commit:prepare_commit
  -> ?prepare_transaction:prepare_transaction
  -> ?validate_state:(value -> (unit, string) result)
  -> ?limits:execution_limits
  -> session
  -> response:string
  -> (unit, Chatml_diagnostic.t) result

(** Enqueue an internal event for later delivery via the host's internal-event
    replay mechanism. Unlike [Runtime.emit], this is host-driven and does not
    require an active handler execution. *)
val enqueue_internal_event : session -> value -> (unit, string) result

(** Prepare a queue append without changing live state. The owning host must
    serialize state/queue access until it invokes the non-yielding installer,
    exactly once after successful durable preparation. No script executes. *)
val prepare_enqueue_internal_event : session -> value -> (unit -> unit, string) result
