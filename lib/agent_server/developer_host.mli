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

module Change : sig
  type kind =
    | Command_changed
    | Approval_changed
end

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
  module Task_reference = Developer_repl_epoch.Task_reference
  module Type_selector = Developer_repl_epoch.Type_selector
  module Type_inspection = Developer_repl_epoch.Type_inspection
  module Task_reference_page = Developer_repl_epoch.Task_reference_page

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

  (** Same keyed Create owner as [create]. Only a true new-key action calls
      [resolve_fresh]; a historical retry compares its canonical Original
      before any source read. The nonyielding request lease is checked again
      after that possibly yielding resolution and before admission. *)
  val create_resolving
    :  t
    -> actor:Operator_authorization.t
    -> run_id:P.Id.Developer_run.t
    -> key:P.Idempotency_key.t
    -> original:Original.t
    -> resolve_fresh:
         (unit -> (Developer_command_original.Fresh_source.t, P.Error.t) result)
    -> request_current:(unit -> bool)
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

  val submit_with_request
    :  t
    -> actor:Operator_authorization.t
    -> instance:Instance.t
    -> epoch:Epoch.t
    -> expected_revision:Revision.t
    -> key:P.Idempotency_key.t
    -> source:string
    -> request_current:(unit -> bool)
    -> (Submission.t, P.Error.t) result

  val observe
    :  t
    -> actor:Operator_authorization.t
    -> instance:Instance.t
    -> key:P.Idempotency_key.t
    -> (Submission.t option, P.Error.t) result

  val inspect_type
    :  t
    -> actor:Operator_authorization.t
    -> instance:Instance.t
    -> epoch:Epoch.t
    -> key:P.Idempotency_key.t
    -> statement_index:int
    -> selector:Type_selector.t
    -> max_bytes:int
    -> (Type_inspection.t, P.Error.t) result

  val task_references_page
    :  t
    -> actor:Operator_authorization.t
    -> instance:Instance.t
    -> epoch:Epoch.t
    -> key:P.Idempotency_key.t
    -> after:Task_reference.t option
    -> limit:int
    -> (Task_reference_page.t, P.Error.t) result

  val cancel_and_wait
    :  t
    -> actor:Operator_authorization.t
    -> instance:Instance.t
    -> key:P.Idempotency_key.t
    -> (Submission.t, P.Error.t) result

  val cancel_and_wait_with_request
    :  t
    -> actor:Operator_authorization.t
    -> instance:Instance.t
    -> key:P.Idempotency_key.t
    -> request_current:(unit -> bool)
    -> (Submission.t, P.Error.t) result

  val reset_and_wait
    :  t
    -> actor:Operator_authorization.t
    -> instance:Instance.t
    -> expected_epoch:Epoch.t
    -> key:P.Idempotency_key.t
    -> (View.t, P.Error.t) result

  val reset_and_wait_with_request
    :  t
    -> actor:Operator_authorization.t
    -> instance:Instance.t
    -> expected_epoch:Epoch.t
    -> key:P.Idempotency_key.t
    -> request_current:(unit -> bool)
    -> (View.t, P.Error.t) result

  val close_and_wait
    :  t
    -> actor:Operator_authorization.t
    -> instance:Instance.t
    -> key:P.Idempotency_key.t
    -> (unit, P.Error.t) result

  val close_and_wait_with_request
    :  t
    -> actor:Operator_authorization.t
    -> instance:Instance.t
    -> key:P.Idempotency_key.t
    -> request_current:(unit -> bool)
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

  module Task : sig
    module Original = Developer_repl_task_original

    type view

    val row : view -> Store.Command.Row.t
    val receipt : view -> Store.Command.Receipt.t option

    (** Same-key historical lookup/disclosure is first and compares the
        canonical Task Original before any physical Task borrow. [fresh_epoch]
        is absent for a historical read, including after process restart; a
        true miss requires the exact live physical instance/epoch. On a true
        miss, atomically register this instance's Task action against reset/
        close, authenticate its original retained source and exact physical
        handle, reserve a real Repl_task_run row in the shared Store, and use
        one fresh Lifecycle Start. If preparation, Store, admission or launch
        fails before callback entry, protected cleanup abandons only that
        unentered marker on typed error, exception or cancellation; an entered
        or uncertain callback remains owned and must be joined. The first
        action lazily admits the matched
        Root/resources; later rows reuse that same Root only under exact
        Store handle, original/current actor, source/contract and configured
        Prompt/Workspace checks. Its daemon-owned callback drives OCH-413's
        retained interpreter once through the existing runner, native/Job/
        Model policy and single scheduler. Historical/uncertain Start or
        Finish cannot enter the callback again. A Task terminal does not
        retire its Root while children may remain queued. *)
    val submit
      :  t
      -> actor:Operator_authorization.t
      -> key:P.Idempotency_key.t
      -> original:Original.t
      -> fresh_epoch:(Instance.t * Epoch.t) option
      -> (view, P.Error.t) result

    (** Durable run/key/Original history is resolved before this callback.
        Only the authoritative true miss may resolve wire comparison IDs to
        the exact current physical instance/epoch. A historical retry works
        after reset or daemon restart without a process-local token. The
        request lease governs Reserve admission, not a daemon-owned callback
        already durably handed off. *)
    val submit_resolving
      :  t
      -> actor:Operator_authorization.t
      -> key:P.Idempotency_key.t
      -> original:Original.t
      -> resolve_fresh_epoch:(unit -> (Instance.t * Epoch.t, P.Error.t) result)
      -> request_current:(unit -> bool)
      -> (view, P.Error.t) result

    (** Current disclosure of an actual persisted Task row, including after
        process restart. A row is comparison data, never a fresh handle. *)
    val observe
      :  t
      -> actor:Operator_authorization.t
      -> run_id:P.Id.Developer_run.t
      -> key:P.Idempotency_key.t
      -> (view option, P.Error.t) result

    val await_terminal
      :  t
      -> actor:Operator_authorization.t
      -> run_id:P.Id.Developer_run.t
      -> key:P.Idempotency_key.t
      -> (view, P.Error.t) result

    (** Distinct Task/Control permission, current actor/source and exact
        physical callback check before cancellation. A cancelled Task remains
        a truthful durable receipt and never clears its key. *)
    val cancel_and_wait
      :  t
      -> actor:Operator_authorization.t
      -> run_id:P.Id.Developer_run.t
      -> key:P.Idempotency_key.t
      -> (view, P.Error.t) result

    val cancel_and_wait_with_request
      :  t
      -> actor:Operator_authorization.t
      -> run_id:P.Id.Developer_run.t
      -> key:P.Idempotency_key.t
      -> request_current:(unit -> bool)
      -> (view, P.Error.t) result
  end
end

module Trace : sig
  type lookup =
    | Command of
        { run_id : P.Id.Developer_run.t
        ; key : P.Idempotency_key.t
        }
    | Repl_statement of
        { instance : Repl.Instance.t
        ; epoch : Repl.Epoch.t
        ; key : P.Idempotency_key.t
        }
    | Repl_task of
        { run_id : P.Id.Developer_run.t
        ; key : P.Idempotency_key.t
        }

  (** Private bounded host read for the later portable pager. First select the
      exact persisted Command/Task row, or the physical historical REPL
      instance/epoch/key, under current method and receipt policy. Then apply
      the separately configured current historical source-read policy to the
      immutable captured provenance, repeating actor/row/epoch checks after
      yielding Store work and immediately before the snapshot copy. A normal
      edit or closed old execution lease does not recapture/reinterpret a site.
      Revoked principal/source read access fails Permission_denied, not
      Unavailable. IDs, rows and trace cursors confer no authority. This
      returns no source text or authored coordinate without an exact retained
      map; a separate portable adapter owns encoded pages and transports. *)
  val snapshot
    :  t
    -> actor:Operator_authorization.t
    -> lookup:lookup
    -> (Developer_trace_retention.Availability.t, P.Error.t) result
end

module Command : sig
  (** Existing command client/DTO boundary accepts only Check, Fixture_run
      and Live_run rows. A persisted Repl_task_run row is never projected or
      controlled through this surface, even if its run/key is known; use
      [Repl.Task], which requires REPL plus effectful method permission and
      its own disclosure/control checks. *)
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

  (** Resolve a fresh physical source at most once, only after the serialized
      historical-key lookup misses. A same-key retry never invokes the callback.
      Current actor and method policy are rechecked after its possible yield;
      [request_current] is a trusted nonyielding, nonraising lease predicate.
      It is checked through the pre-Reserve admission boundary but not retained
      by the installed callback, so disconnect cannot cancel accepted work.
      The returned source remains subject to ordinary Candidate admission.
      [validate_capture] is trusted, pure, nonyielding compatibility validation
      of the newly captured Prepared source IDs. It runs only after a historical
      miss and before Reserve/effects; it grants no source or execution
      authority. Direct nonportable callers pass an identity validator. *)
  val submit_resolving
    :  t
    -> actor:Operator_authorization.t
    -> run_id:P.Id.Developer_run.t
    -> key:P.Idempotency_key.t
    -> original:Original.t
    -> resolve_fresh:(unit -> (Original.Fresh_source.t, P.Error.t) result)
    -> request_current:(unit -> bool)
    -> validate_capture:(Chatml_developer_capture.t -> (unit, P.Error.t) result)
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

module Approval : sig
  module Pending = Developer_approval_owner.Pending

  type decision = Developer_approval_owner.decision =
    | Approve_once
    | Deny of string

  (** Process-local projection of the exact installed run generation.
      [run_id] is comparison data; actor, Root, command/Job row and the
      distinct read policy are rechecked after any Store wait. *)
  val pending
    :  t
    -> actor:Operator_authorization.t
    -> run_id:P.Id.Developer_run.t
    -> (Pending.t list, P.Error.t) result

  (** Resolve one request once under separate response permission and exact
      command/Claim/native ownership. ID or receipt read is not authority. *)
  val respond
    :  t
    -> actor:Operator_authorization.t
    -> run_id:P.Id.Developer_run.t
    -> id:P.Id.Permission.t
    -> decision
    -> (unit, P.Error.t) result

  val cancel
    :  t
    -> actor:Operator_authorization.t
    -> run_id:P.Id.Developer_run.t
    -> id:P.Id.Permission.t
    -> (unit, P.Error.t) result
end

(** Open one Store beneath the daemon's Session_store, verify the actual Blob
    binding, borrow the existing Authoring installation and build one physical
    Root.Registry over the SAME Job_capacity. [prompts], [catalogs] and
    [workspace_leases] are the daemon's genuine publication/lifetime owners;
    no caller supplies a separately chosen native registry, Workspace_instance
    or conflict domain. [provider] is the genuine operator port, optionally
    absent; no ambient provider fallback exists. A configured [model_target]
    is selected for the original actor only on a fresh Live command, never on
    a historical retry. [approval=None] retains typed permission-required
    shell behavior; an enabled owner belongs to one durably Started run
    generation. No caller-supplied broker, Assume_approved provider or
    Session-scoped grant is substituted.
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
  -> approval:Developer_approval_owner.Configuration.t option
  -> max_native_bindings:int
  -> child_sources:Session_factory.Child_source_policy.t option
  -> content_limits:Chatmd_prompt_content.Limits.t option
  -> remote_content:Developer_child_remote_content.Policy.t option
  -> agent_source_limits:Developer_agent_source_runtime.Limits.t
  -> active_agent_calls:Agent_session.Selected_agent_depth.Limits.t
  -> job_policy:Chat_response.One_off_request.policy
  -> runtime_policy:Chat_response.Runtime_semantics.policy
  -> native_permission:Developer_native_permission.Mode.t
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
  -> trace:Developer_trace_retention.Configuration.t option
  -> ?on_change:
       (run_id:P.Id.Developer_run.t
        -> key:P.Idempotency_key.t
        -> kind:Change.kind
        -> unit)
  -> ?on_repl_change:(Developer_repl_epoch.Change.t -> unit)
  -> ?on_task_change:(run_id:P.Id.Developer_run.t -> key:P.Idempotency_key.t -> unit)
  -> now:(unit -> P.Timestamp.t)
  -> checkpoint:(unit -> unit)
  -> unit
  -> (t, P.Error.t) result

(** The optional change callbacks above are trusted, same-domain,
    nonyielding and nonraising bounded Hub enqueues. They run only after the
    corresponding owner transition has committed, outside lifecycle/REPL
    mutation gates; a notification cannot decide durable execution. *)

(** Comparison-only installation facts; neither accessor grants admission. *)
val approval_installed : t -> bool

(** Comparison-only fact about this exact Host's retention owner. No Entry,
    recorder, provenance or source-read policy escapes. Portable trace method
    installation still requires an independently validated wire policy and
    current connection negotiation. *)
val trace_installed : t -> bool

(** Checked comparison-only capacity from the installed REPL policy. It
    does not expose or construct an Instance, Epoch or Revision. *)
val repl_registry_required_entries : t -> (int, P.Error.t) result

val uses_authoring : t -> Authoring_installation.t -> bool

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
