(** One daemon-owned, process-local REPL foundation over the existing developer
    source owner and OCH-401 typing/evaluator. It has no Store writer, Root,
    scheduler, native provider or Task interpreter. Task actions belong to the
    later Host adapter. A restart expires every instance and handle. *)

open! Core
module P = Agent_protocol
module Capture_host = Authoring_context_service.Developer
module Evaluator = Chatml_repl_execution

type t

module Original : sig
  type t

  (** Canonical capture-independent create identity. Route, selected Standard
      ABI, configured Prompt/Workspace selectors and requested execution/output
      ceilings are comparison data, not grants. This is a REPL action; it does
      not manufacture a Check/Live command kind or entrypoint. *)
  val create
    :  route:Developer_command_original.Route.t
    -> target:Chatml.Chatml_compiler_contract.Target.t
    -> prompt:P.Id.Prompt_definition.t
    -> workspace:P.Id.Workspace_definition.t
    -> requested_execution:Chatml_execution.limits option
    -> requested_max_output_bytes:int option
    -> checkpoint:(unit -> unit)
    -> (t, P.Error.t) result

  val fingerprint : t -> Chatml_source.Digest.t
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

module Limits : sig
  type t

  (** All counts/bytes are positive and bounded by the installed source,
      typing and evaluator policies. Retained receipts and exact Task origins
      have independent caps. A bounded key ledger retains either the original
      result or a tombstone after detail eviction; known keys never execute
      again. At least two keys are required: one slot is reserved for an
      authenticated close even after submit/reset admission is exhausted.
      Exhaustion rejects fresh work keys before any effect. *)
  val create
    :  max_instances:int
    -> max_keys_per_instance:int
    -> max_submissions_per_epoch:int
    -> max_source_bytes:int
    -> max_receipt_bytes:int
    -> max_total_receipt_bytes:int
    -> max_task_origins:int
    -> t Or_error.t
end

module Configuration : sig
  type t

  (** Immutable REPL-only policy. This does not mint source, method or Task
      authority. The host binds it to its existing capture owner at install. *)
  val create
    :  scope_limits:Chatml_configured_lookup.Repl_scope.Limits.t
    -> root_limits:Chatml_dependency_session.Roots.Limits.t
    -> typing_limits:Chatml_repl_typing.Limits.t
    -> evaluator_limits:Evaluator.Limits.t
    -> execution_policy:Chatml_execution.policy
    -> limits:Limits.t
    -> t
end

module View : sig
  type t

  val instance : t -> Instance.t
  val epoch : t -> Epoch.t
  val revision : t -> Revision.t
  val run_id : t -> P.Id.Developer_run.t

  (** Source_selection.repl_root_fingerprint: exact Prepared graph plus
      selected Standard contract policy. This same canonical identity is the
      later Store handle and Root.Source.Repl fingerprint. *)
  val capture_fingerprint : t -> Chatml_source.Digest.t
end

module Task_reference : sig
  type t

  (** Canonical process-instance/epoch/ordinal/source identity. A decoded
      reference remains inert; only the exact current physical handle and
      [task_origin] may authenticate it for a fresh action. *)
  val of_string : string -> (t, P.Error.t) result

  val to_string : t -> string
  val equal : t -> t -> bool
  val run_id : t -> P.Id.Developer_run.t
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

  val status : t -> status
  val details : t -> details
  val completed_statements : t -> int
  val revision : t -> Revision.t
  val statements : t -> Evaluator.Receipt.t list
  val task_handles : t -> Evaluator.Task_handle.t list

  (** Bounded comparison references issued alongside successful retained
      handles, in the same order. These can be projected to a later REPL client
      without exposing a Lang.Task or physical handle. Output_limited hides
      detail but does not turn a retained reference into execution authority;
      a later action must still resolve it against this live epoch. *)
  val task_references : t -> Task_reference.t list
end

module Task_origin : sig
  type t

  (** Exact Candidate.source paired with the physical Task handle only after
      successful evaluator finalization; it may be an earlier snippet. Source
      identity and digest do not grant Task execution. *)
  val source : t -> Chatml_source.t

  (** Stable bounded comparison reference minted with this origin, never from
      a caller-provided Task value or a later latest-snippet lookup. *)
  val reference : t -> Task_reference.t

  (** Recheck the exact original/current actor and selected base source and
      contract after waits. A newly captured equal-looking graph is not this
      epoch's authority. *)
  val check_current
    :  t
    -> actor:Operator_authorization.t
    -> checkpoint:(unit -> unit)
    -> (unit, P.Error.t) result
end

(** Private host installation. Borrow the SAME capture host already installed
    by developer commands; no second source resolver or writer. [surface] is
    selected internally from the authentic Standard contract. One_off_v1 and
    Tool_v1 deliberately omit Model; only an actual selected Developer_tool_v1
    contract exposes its bounded Model.spawn interface. All three omit print
    and never touch the process-global print sink/stdout. No arbitrary
    caller-supplied runtime implementation is accepted. No language or native
    effect runs at installation. *)
val install
  :  sw:Eio.Switch.t
  -> capture_host:Capture_host.t
  -> env:Eio_unix.Stdenv.base
  -> clock:Eio.Time.Mono.ty Eio.Time.Mono.t
  -> configuration:Configuration.t
  -> checkpoint:(unit -> unit)
  -> t

(** Serialize this new instance's keyed create and exact physical source
    acquisition. The original actor token is retained; reconnect may provide
    a fresh current token only for the same principal ID and authentication
    kind, with both tokens current and the source policy approving each.
    Match route/Fresh_source before read, then use the shared
    Source_selection kernel; require its selected target to equal Original.
    Create the Repl_scope, Typing and sole evaluator from the same Prepared and
    contract. A duplicate key/payload returns the retained View without
    recapture; changed payload conflicts. No Task/initializer runs here. *)
val create
  :  t
  -> actor:Operator_authorization.t
  -> run_id:P.Id.Developer_run.t
  -> key:P.Idempotency_key.t
  -> original:Original.t
  -> fresh_source:Developer_command_original.Fresh_source.t option
  -> (View.t, P.Error.t) result

(** Expected-revision, keyed statement submission. The actor makes a fresh
    captured snippet ID/bytes under its bounded namespace, calls the existing
    Typing.admit/prepare_next and Evaluator.execute_next in order, and stores
    every truthful partial outcome. A syntax Need_more/Rejected result does not
    run code. Selected configured initializers execute only inside
    [execute_next]'s current execution scope. Before each
    code entry, repeat source/contract and original/current actor admission
    after waits. Selected Standard surfaces have no print or eager native
    callbacks. Even Developer_tool_v1 Model.spawn remains inside an inert
    deferred Task until a distinct durable action starts it. Same key and
    identical action/payload return the exact prior result without re-executing
    effects; changed payload conflicts. If receipt detail was evicted but its
    bounded key tombstone remains, matching retry reports result-expired and
    never runs again. Fresh keys reject when the per-instance key ledger is
    full; reset does not clear old-key tombstones. Cancellation cannot leave a reserved
    typing candidate or detached evaluator callback. An unexpected callback
    exception is contained by the daemon owner: the original waiter receives
    the same exception/backtrace after join. If statement execution began, a
    bounded [Stopped] partial outcome remains; in every case the key is
    consumed and an authorized retry cannot execute it again. *)
val submit
  :  t
  -> actor:Operator_authorization.t
  -> instance:Instance.t
  -> epoch:Epoch.t
  -> expected_revision:Revision.t
  -> key:P.Idempotency_key.t
  -> source:string
  -> (Submission.t, P.Error.t) result

(** Read-only current process-local result, with current actor/source
    disclosure checks. A known evicted key reports result-expired rather than
    looking like a miss. Unknown or expired instances do not restart/replay. *)
val observe
  :  t
  -> actor:Operator_authorization.t
  -> instance:Instance.t
  -> key:P.Idempotency_key.t
  -> (Submission.t option, P.Error.t) result

(** Explicit current-actor control of the exact in-flight submission. This
    signal path does not wait for the serialized statement lock: it marks and
    cancels that physical callback first, then joins its finalization and
    returns the truthful retained outcome. A foreign/historical key cannot
    cancel a different statement. It does not clear idempotency evidence. *)
val cancel_and_wait
  :  t
  -> actor:Operator_authorization.t
  -> instance:Instance.t
  -> key:P.Idempotency_key.t
  -> (Submission.t, P.Error.t) result

type borrowed

(** Lexically lend the exact serialized actor/epoch to the later Task adapter.
    It must create a separate durable action receipt and fresh Root admission;
    this borrow itself never runs a Task. It expires on scope exit. *)
val with_current
  :  t
  -> actor:Operator_authorization.t
  -> instance:Instance.t
  -> epoch:Epoch.t
  -> f:(borrowed -> ('a, P.Error.t) result)
  -> ('a, P.Error.t) result

val evaluator : borrowed -> Evaluator.t

(** Immutable selected Standard surface used by this same borrow's typing and
    evaluator. Comparison data, not a new source or effect grant; call after
    [check_now] and repeat admission after any yielding work. *)
val execution_surface : borrowed -> Chatml.Chatml_builtin_surface.surface

val check_now : borrowed -> (unit, P.Error.t) result
val task_origin : borrowed -> Evaluator.Task_handle.t -> (Task_origin.t, P.Error.t) result

(** The selected source is a comparison-only projection from the same physical
    borrowed instance. Root admission must recheck this exact selection after
    its Store/worker waits; it cannot use a caller-constructed Prepared graph. *)
val source_selection : borrowed -> Capture_host.Source_selection.t

(** Configured selectors retained with the authenticated instance, used only
    to prepare its exact Host Root after the first durable Task Start. *)
val prompt : borrowed -> P.Id.Prompt_definition.t

val workspace : borrowed -> P.Id.Workspace_definition.t
val original_actor : borrowed -> Operator_authorization.t
val current_actor : borrowed -> Operator_authorization.t
val run_id : borrowed -> P.Id.Developer_run.t

module Task_action : sig
  type active

  type unlaunched_disposition =
    | No_durable_row
    | Terminal_row

  (** Register an independently cancellable Task action in the SAME instance
      state as statements and reset/close control, then run the trusted
      reservation/launch callback in a daemon-owned child fiber. Registration
      mutates the control state nonyieldingly after acquiring its gate and
      rejects reserved control, another statement/Task, changed epoch, reused
      key or expired owner. The marker and its cancellation/join handle exist
      BEFORE any yielding Store/actor work. Transport disconnect abandons only
      its waiter. Reset/close can signal this prelaunch fiber as well as the
      later Lifecycle callback. A typed error, exception or cancellation
      before Launch is attempted abandons exactly the unentered marker. Once
      the registered fiber calls Lifecycle.launch, the marker stays owned
      through its launched-but-not-yet-entered child, entered child, and any
      unknown launch/Finish. Returning the initial persisted View does not
      release it; the registered fiber awaits the exact Lifecycle terminal or
      retained uncertainty, and reset/close can cancel and join it meanwhile.
      A conclusive launch refusal with no child settles/abandons the marker
      only after Lifecycle's exact unlaunched-row cleanup; uncertain custody
      never becomes an apparently idle instance. [publish_initial] is an
      owner-supplied nonyielding, nonraising, one-shot resolver. If [f]
      raises or returns before publishing, [start] resolves its waiting caller
      with that typed/raised outcome while retaining any launched custody;
      it never waits for the interpreter merely to return an initial View. *)
  val start
    :  t
    -> actor:Operator_authorization.t
    -> instance:Instance.t
    -> epoch:Epoch.t
    -> key:P.Idempotency_key.t
    -> f:
         (active
          -> publish_initial:(('a, P.Error.t) result -> unit)
          -> (unit, P.Error.t) result)
    -> ('a, P.Error.t) result

  (** Before [Lifecycle.launch], the owner fiber has a registered prelaunch
      marker. It must call [mark_launching] immediately before Launch with no
      intervening yield, then publish exactly one initial View/error through
      the [start] callback's [publish_initial]. The callback continues to
      await exact Lifecycle settlement while the marker remains registered.
      The Lifecycle child may call [enter] before or after publication.
      [mark_launching] does not grant execution or assert Launch succeeded;
      a conclusive no-child refusal uses [finish_unlaunched] only after Store
      cleanup, whereas uncertain Launch is retained for reset/recovery. Its
      closed disposition says whether no row was ever durably installed or a
      terminal row now owns the key; an unknown/partial reservation has no
      disposition and retains marker/key. Historical same-action lookup still
      precedes registration, while statement/reset/close reuse of a retained
      Task key is rejected. [f]
      must await exact Lifecycle terminal or retained uncertainty after
      publication. Only a conclusively settled terminal releases the action;
      callback-body exit alone cannot make reset replace its Root. *)
  val mark_launching : active -> (unit, P.Error.t) result

  val finish_unlaunched
    :  active
    -> disposition:unlaunched_disposition
    -> (unit, P.Error.t) result

  (** Trusted Host acknowledgement AFTER it has awaited/reconciled the exact
      Lifecycle terminal row, compared its physical run/key/owner and checked
      terminal persistence. This is not itself a Store proof. It rejects a
      prelaunch, conclusive no-child or foreign/expired marker. Only this
      state or [finish_unlaunched] lets the owner fiber clear registration on
      normal exit; returning Ok from its callback alone never does. Any
      exception, cancellation or unknown Finish retains retryable custody. *)
  val mark_settled : active -> (unit, P.Error.t) result

  (** On a true key miss, authenticate the reference against this active
      instance's internally retained physical Task handle, exact Task_origin
      and current source before reserving a Store row. This returns no Task
      or effect capability. The returned exact Source_selection is comparison
      data for the Store capture fingerprint; callback entry repeats the proof
      after waits. *)
  val preflight
    :  active
    -> actor:Operator_authorization.t
    -> reference:Task_reference.t
    -> (Capture_host.Source_selection.t, P.Error.t) result

  (** Enter only from the one daemon-owned Lifecycle callback paired with
      this marker. The callback's joined switch and cancellation handle are
      recorded before [f]; an earlier reset signal cancels entry. [f] may
      enter [with_current] repeatedly but never exposes an unregistered Task.
      On exit the execution callback's completion resolves exactly once; this
      is not the durable Lifecycle Finish acknowledgement and does not clear
      the registered action. *)
  val enter
    :  active
    -> sw:Eio.Switch.t
    -> f:(unit -> ('a, P.Error.t) result)
    -> ('a, P.Error.t) result

  (** Serialize the exact active marker with this instance's evaluator gate;
      refresh original/current actor and Source_selection before yielding work,
      then resolve the reference to the exact internally retained physical
      handle/origin and check membership immediately before interpreter entry.
      The caller never supplies a mutable Task or borrowed handle. Escaped
      scopes expire. Reset reserves
      control outside this gate, signals/joins [active], then acquires it. *)
  val with_current
    :  active
    -> actor:Operator_authorization.t
    -> reference:Task_reference.t
    -> f:(borrowed -> Evaluator.Task_handle.t -> Task_origin.t -> ('a, P.Error.t) result)
    -> ('a, P.Error.t) result
end

type owner = t

(** Comparison-only projection for trusted Task cleanup callbacks. The
    instance must be physically installed by this exact REPL owner; a decoded
    or foreign display identity cannot select another run. It is available
    during reserved reset/close and owner shutdown, including after admission
    has stopped, and grants no evaluator or Store mutation. *)
val instance_run_id : owner -> Instance.t -> (P.Id.Developer_run.t, P.Error.t) result

(** Comparison only: the exact installed instance's selected source and
    original actor are physically the ones retained by a Host Root. This
    cannot borrow a Task or authorize teardown on its own. *)
val instance_paired_to
  :  owner
  -> Instance.t
  -> selection:Capture_host.Source_selection.t
  -> original_actor:Operator_authorization.t
  -> bool

(** The same physical source comparison for an admitted Task whose durable
    Start failed before a Root existed. Its Host package still retains the
    selected source; there is no Root actor to compare in that phase. *)
val instance_uses_selection
  :  owner
  -> Instance.t
  -> Capture_host.Source_selection.t
  -> bool

module Task_cleanup : sig
  type t
  type custody

  (** Comparison-only identity of the exact reserved reset/close control.
      Same-key cleanup retries carry the same physical custody token. This
      projection is available only while that instance's control remains
      reserved and its Task/statement has joined; it grants no fresh effect
      or source authority. *)
  val current_custody : owner -> Instance.t -> (custody, P.Error.t) result

  val same_custody : custody -> custody -> bool

  (** Inside the authentic reset/close callback after it has reserved control
      and joined the current action, refresh original/current actor and exact
      selected source under that same still-reserved control. Repeat a final
      nonyielding control/actor/source suffix after the yielding refresh.
      Host calls this immediately before retiring the paired Root; it is not
      an evaluator borrow or a grant to start another Task. Shutdown has a
      separate trusted no-actor path. *)
  val check_retiring_current
    :  owner
    -> actor:Operator_authorization.t
    -> instance:Instance.t
    -> (unit, P.Error.t) result

  (** Trusted Host-only coordination. [authorize_retirement] checks distinct
      Task/Control permission and any retained Root policy before control is
      reserved; it is nonyielding. Reset/close reserve control before any
      later wait, signal and join statement/Task outside the evaluator gate,
      then repeat yielding actor/source/affected-row checks and final
      nonyielding admission before destructive retirement. [retire_instance]
      also joins the exact Lifecycle terminal/uncertain-result custody for
      every affected Task row, then closes its Root/jobs before evaluator/
      epoch replacement. A pending/unknown Finish or failed Root/Job close
      remains retryable under the reserved control; callback exit alone is
      insufficient to replace the epoch.
      Shutdown uses [retire_all] after stopping new actions, with no expired
      request actor. Callbacks retain the exact per-instance Host package,
      never a Root selected by display run ID alone. *)
  val create
    :  authorize_retirement:
         (actor:Operator_authorization.t
          -> instance:Instance.t
          -> (unit, P.Error.t) result)
    -> retire_instance:
         (actor:Operator_authorization.t -> Instance.t -> (unit, P.Error.t) result)
    -> retire_all:(unit -> (unit, P.Error.t) result)
    -> t

  val install_once : owner -> t -> (unit, P.Error.t) result
end

(** Reset/close signal the actor's current statement through the independent
    control path, then join it before revoking
    scope/evaluator/handles. OCH-402 registers any background Task action with
    the same owner and joins it before these operations return. Same-key retry
    observes the original result; no restart recovery or replay exists. *)
val reset_and_wait
  :  t
  -> actor:Operator_authorization.t
  -> instance:Instance.t
  -> expected_epoch:Epoch.t
  -> key:P.Idempotency_key.t
  -> (View.t, P.Error.t) result

val close_and_wait
  :  t
  -> actor:Operator_authorization.t
  -> instance:Instance.t
  -> key:P.Idempotency_key.t
  -> (unit, P.Error.t) result

(** Trusted daemon destructor: stop admission, signal and join every owned
    statement callback, then close the evaluators. It does not rely on a
    request actor whose credential may have expired during shutdown. *)
val shutdown_and_wait : t -> (unit, P.Error.t) result
