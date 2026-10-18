open! Core
module P = Agent_protocol
module Store = Agent_store.Developer_run_store

(** One daemon-owned callable developer host. Client adapters bind local/HTTP/stdio
    clients to this service; no transport parser or second writer lives here.
    The daemon shares its physical Session_store, Blob_store, Job_capacity,
    Authoring_installation, Prompt/workspace catalogs, provider port and ONE
    Job_scheduler with this host. Each Live command borrows one joined
    Developer_live_resources package constructed from those same owners;
    Captured Check/Fixture work is pure and never creates a fake live Root. *)
type t

module Limits : sig
  type t

  (** Validated host command-slot bound and existing Root/disclosure bounds.
      These cap callbacks plus retained uncertain work; durable Store rows,
      whole-document bytes and output use their own configured lower limits. *)
  val create
    :  roots:Developer_root_owner.Limits.t
    -> max_active_commands:int
    -> disclosure:Developer_result_disclosure.Limits.t
    -> t Or_error.t
end

module Repl : sig
  module Original = Developer_repl_epoch.Original
  module Instance = Developer_repl_epoch.Instance
  module Epoch = Developer_repl_epoch.Epoch
  module Revision = Developer_repl_epoch.Revision
  module View = Developer_repl_epoch.View
  module Submission = Developer_repl_epoch.Submission
  module Task_origin = Developer_repl_epoch.Task_origin

  type borrowed = Developer_repl_epoch.borrowed

  (** Every operation borrows the same process-local actor installed with this
      host's exact Developer_capture_host. No command row, Store writer or
      second scheduler is constructed for inert REPL statements. *)
  val create
    :  t
    -> actor:Operator_authorization.t
    -> run_id:P.Id.Developer_run.t
    -> key:P.Idempotency_key.t
    -> original:Original.t
    -> fresh_source:Developer_command_original.Fresh_source.t option
    -> (View.t, P.Error.t) result

  val submit
    :  t
    -> actor:Operator_authorization.t
    -> instance:Instance.t
    -> epoch:Epoch.t
    -> expected_revision:Revision.t
    -> key:P.Idempotency_key.t
    -> source:string
    -> (Submission.t, P.Error.t) result

  val observe
    :  t
    -> actor:Operator_authorization.t
    -> instance:Instance.t
    -> key:P.Idempotency_key.t
    -> (Submission.t option, P.Error.t) result

  val cancel_and_wait
    :  t
    -> actor:Operator_authorization.t
    -> instance:Instance.t
    -> key:P.Idempotency_key.t
    -> (Submission.t, P.Error.t) result

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

  val with_current
    :  t
    -> actor:Operator_authorization.t
    -> instance:Instance.t
    -> epoch:Epoch.t
    -> f:(borrowed -> ('a, P.Error.t) result)
    -> ('a, P.Error.t) result

  val evaluator : borrowed -> Chatml_repl_execution.t
  val check_now : borrowed -> (unit, P.Error.t) result

  val task_origin
    :  borrowed
    -> Chatml_repl_execution.Task_handle.t
    -> (Task_origin.t, P.Error.t) result
end

module Command : sig
  type view

  module Original = Developer_command_original

  module Disclosure_policy : sig
    type t

    (** Trusted nonyielding receipt-read decision for this authenticated
        current actor and exact persisted row; checked after Store waits. *)
    val create : authorize:(Operator_authorization.t -> Store.Command.Row.t -> bool) -> t
  end

  module Control_policy : sig
    type t

    (** Separate trusted nonyielding explicit-cancel permission. Receipt-read
        access never implies control over a physical callback. *)
    val create : authorize:(Operator_authorization.t -> Store.Command.Row.t -> bool) -> t
  end

  (** Authenticated projection of the exact persisted row. These accessors
      carry identity/status for clients; they grant no Start, cancel,
      source access or result disclosure by themselves. [receipt=None] means
      a persisted nonterminal phase, never a missing command. *)
  val row : view -> Store.Command.Row.t

  val id : view -> Store.Command.Receipt.Id.t
  val owner : view -> P.Execution_owner.developer
  val kind : view -> Store.Command.Receipt.Kind.t
  val phase : view -> Store.Command.Row.Phase.t
  val capture_fingerprint : view -> Chatml_source.Digest.t
  val request_fingerprint : view -> Chatml_source.Digest.t
  val receipt : view -> Store.Command.Receipt.t option

  (** Reserve this run's bounded host admission slot without yielding, then
      authenticate actor and do a physical run/key lookup FIRST. The canonical
      original kind/fingerprint is compared before source bind, recapture or
      epoch admission. Identical historical retry can set fresh_source=None,
      even after source removal/reconnect; current receipt disclosure is
      checked before/after I/O. Changed request conflicts and cannot execute.

      Developer_method_authorization.check_now requires the exact command
      action scope on original and current actors before this lookup. After any
      yielding read, recheck the slot, actor, method scope and key before the
      Fresh path or publication; Store's final writer repeats absence/CAS.
      For a true miss, a Cold persisted run first uses
      Result_root.observe_latest plus exact
      admit_epoch(previous=Some) to obtain its former handle; joined
      Run_claim/Recovery.finish interrupts abandoned nonterminal commands
      BEFORE reserving this new key. A Ready run does not repeat startup
      Recovery on every key. Quarantine blocks new reservations and retains
      exact physical custody. On a Cold historical nonterminal retry, finish
      startup recovery then reread its truthful Interrupted receipt; an
      active same-process callback may instead be observed in progress.

      Only after an authoritative miss does the host require a real current
      Fresh_source, bind its stable route to the issuing source/connection and
      prepare one source-owned Candidate. Pre-capture denial is a typed preparation
      failure, not a fabricated captured-run receipt. Admit its durable epoch;
      for Live, durably reserve the captured command row, then its one
      daemon-owned Active callback selects a current configured Prompt/Workspace
      pair and consumes the unique durable Execution_start BEFORE preparing
      native/shell/MCP resources. An uncertain Start cannot construct any
      runtime and cannot be replayed. The started callback then prepares the
      matching Developer_live_resources package, installs one actual Root,
      shared Publisher, lexical Job/native/Model packages and sole-scheduler
      attachment, and binds its one-shot preparing proof to that exact Root
      before user code. Resource/Root failures settle the already started row
      as truthful partial-effect failure, including any completed setup
      effects; no second Start or language callback is attempted.
      The command callback owns a typed provisional Live package before a
      yielding Root admission. Its trusted nonyielding preparation handoff
      retains that exact package in this host before returning to the callback;
      cancellation cannot leave an untracked switch or Workspace lease. Once
      Root admission succeeds, the host replaces the provisional state with
      the exact Root/package pair before further yielding setup. Failed setup
      either joins/cleans this retained owner or leaves it in explicit
      retirement custody, never treats it as a runnable Root. A later command
      cannot reuse a partially installed package as an effect authority.
      Check/Fixture require no Root. A captured compile/admission/runtime
      failure settles a truthful bounded command row through Store.Command.
      A changed capture or original actor first passes current persisted-
      principal, method and source admission, then retires old callbacks under
      this same-run Slot, detaches and joins the old scheduler/Root/live owner,
      and selects the next epoch from one exact latest Observation. The host
      retains that intended owner, command key/request identity, capture and
      original previous Observation
      before Store.admit_epoch; on uncertain admission it retries precisely
      the same tuple, never rereads latest to allocate another epoch or
      treats equal-looking coordinates from a different request as authority.
      A different selection waits for that retained admission to settle.
      The first Live selector is retained as typed Prompt/Workspace IDs in the
      Ready owner with its original actor before Reserve or Start. A later pure
      callback with the same principal cannot replace that actor. A matching
      pair and exact actor may reuse its Root;
      a changed pair requires authorized retirement and a new epoch before
      any effect. Pure Check/Fixture never borrow its Root, replace that
      selection or require its original actor object. After
      startup recovery no prior Root remains to reuse, so a fresh admitted
      Live command establishes the new process-local pair. The historical
      same-key comparison still uses the persisted original request digest.
      This Original's requested execution/output ceilings are resolved
      against the Store-bound host Configuration before any callback; no
      requested value widens the admitted host budget.

      Live consumes only its fresh uninterrupted Execution_start before the
      daemon-owned callback enters any language/native/job/provider effect.
      Installed/reconciled/historical Start is never replayed. The transport
      request owns only this call's waiter; disconnect does not cancel the
      host callback or undo a completed external effect. *)
  val submit
    :  t
    -> actor:Operator_authorization.t
    -> run_id:P.Id.Developer_run.t
    -> key:P.Idempotency_key.t
    -> original:Original.t
    -> fresh_source:Original.Fresh_source.t option
    -> (view, P.Error.t) result

  (** Historical disclosure without same-request comparison. It never
      recaptures source, creates an execution run, retries a callback or
      treats a pending-barrier error as a key miss. Under the same-run Slot, a
      Cold restarted nonterminal row first receives joined startup Recovery
      and an authoritative reread, so a vanished callback is not reported as
      live. This is terminal bookkeeping through the same Store writer, not a
      new effect. Ready same-process active work skips Recovery. Current
      actor/receipt policy is checked after every Store wait. *)
  val observe
    :  t
    -> actor:Operator_authorization.t
    -> run_id:P.Id.Developer_run.t
    -> key:P.Idempotency_key.t
    -> (view option, P.Error.t) result

  (** On a Cold restarted nonterminal row, first complete joined startup
      Recovery and reread its truthful terminal interruption. Otherwise a
      passive wait keeps the command's daemon-owned callback alive when this
      waiter is cancelled/disconnected. *)
  val await_terminal
    :  t
    -> actor:Operator_authorization.t
    -> run_id:P.Id.Developer_run.t
    -> key:P.Idempotency_key.t
    -> (view, P.Error.t) result

  (** Distinct Control_policy and current actor/run/key checks select only
      the same-process physical callback generation, then cancel/join it and
      persist truthful partial-effect completion. On a Cold restarted row,
      perform recovery and report its interruption: there is no vanished
      callback to cancel. Historical/foreign keys cannot cancel another
      active command. *)
  val cancel_and_wait
    :  t
    -> actor:Operator_authorization.t
    -> run_id:P.Id.Developer_run.t
    -> key:P.Idempotency_key.t
    -> (view, P.Error.t) result
end

(** Open one Store beneath the daemon's Session_store, verify the actual Blob
    binding, borrow the existing Authoring installation and build one physical
    Root.Registry over the SAME Job_capacity. [prompts], [catalogs] and
    [workspace_leases] are the daemon's genuine publication/lifetime owners;
    no caller supplies a separately chosen native registry, Workspace_instance
    or conflict domain. [provider] is the genuine operator port, optionally
    absent; no ambient provider fallback exists. A configured [model_target]
    is selected for the original actor only on a fresh Live command, never on
    a historical retry. [approval_broker=None] retains typed permission-required
    shell behavior until a real operator response route is installed; no
    Assume_approved or Session-scoped grant is substituted.
    The live Developer facade borrows the exact Authoring_installation owner;
    it does not reconstruct a context/source owner. No command or
    Root effect occurs here. [runtime_policy] is the daemon's immutable initial
    moderator policy for every selected child of this Host, including recursive
    Content and Model children. It comes from Host configuration rather than
    [Prompt_definition.runtime_policy] text. Caller supplies
    explicit trusted policy/budgets;
    Developer_command_budgets.bind checks the Store's actual limits after
    opening it; Configuration itself has no physical Store dependency. *)
val create
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> session_store:Agent_store.Session_store.t
  -> blobs:Agent_store.Blob_store.t
  -> capacity:Job_capacity.t
  -> authoring:Authoring_installation.t
  -> provider:Provider_operator_port.t option
  -> model_target:Inference.Request.Target.t option
  -> prompts:Agent_session.Prompt_catalog.t
  -> catalogs:Session_factory.Developer_catalog.t
  -> workspace_leases:Agent_session.Workspace_lease.t
  -> physical_roots:string list
  -> workspace_cleanup:Agent_session.Workspace_cleanup.Limits.t
  -> script_preparation:Agent_session.Runtime_script_preparation.t option
  -> tool_dir:Eio.Fs.dir_ty Eio.Path.t
  -> home:Eio.Fs.dir_ty Eio.Path.t
  -> cache:Chat_response.Cache.t
  -> approval_broker:Shell_runtime.Approval_broker.t option
  -> max_native_bindings:int
  -> child_sources:Session_factory.Child_source_policy.t option
  -> content_limits:Chatmd_prompt_content.Limits.t option
  -> remote_content:Developer_child_remote_content.Policy.t option
  -> agent_source_limits:Developer_agent_source_runtime.Limits.t
  -> job_policy:Chat_response.One_off_request.policy
  -> runtime_policy:Chat_response.Runtime_semantics.policy
  -> authorize_native:
       (P.Host_invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, P.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t -> (Jsonaf.t, P.Error.t) result)
  -> capture_policy:Authoring_context_service.Developer.Target_policy.t
  -> capture_limits:Authoring_context_service.Developer.Limits.t
  -> command_disclosure:Command.Disclosure_policy.t
  -> command_control:Command.Control_policy.t
  -> result_disclosure:Developer_result_disclosure.Policy.t
  -> publisher_limits:Agent_store.Developer_result_publisher.Limits.t
  -> publisher_storage:Agent_store.Developer_result_publisher.Storage.t
  -> store_limits:Store.Limits.t
  -> limits:Limits.t
  -> budget_configuration:Developer_command_budgets.Configuration.t
  -> repl:Developer_repl_epoch.Configuration.t option
  -> now:(unit -> P.Timestamp.t)
  -> checkpoint:(unit -> unit)
  -> (t, P.Error.t) result

(** The daemon passes this exact registry into its single
    Job_scheduler.start_controlled as developer_registry=Some. *)
val registry : t -> Developer_root_owner.Registry.t

(** One-shot physical comparison with the scheduler's installed registry and
    shared capacity. No developer command is admitted before binding. The
    daemon may instead mechanically construct both from one local argument;
    the scheduler comparison confirms the installed physical registry before
    developer command admission. *)
val install_scheduler : t -> Job_scheduler.t -> (unit, P.Error.t) result

(** Stop new command admission first. Cancel/join daemon-owned command
    callbacks while the shared scheduler remains serviceable, then retire
    this host's physical developer attachments and join their scans/offers.
    Settle/retain exact pending custody, close Root.Registry, then close
    Developer_run_store. The daemon closes its shared conversation/developer
    scheduler afterward. Return Error with unresolved ownership retained
    rather than closing borrowed Blob, authoring, provider or Session_store
    underneath it. *)
val close_and_wait : t -> (unit, P.Error.t) result
