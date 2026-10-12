open! Core

module Activation : sig
  (** Existing session activation only. No queue/implicit retry. *)
  type t =
    | Require_running
    | Start_stopped
end

module Request : sig
  (** Intent, not authority or a source-kind classifier. The actual host checks
      current source/startup/runtime capabilities at command admission. Never
      change branches after a failed managed-run admission. *)
  type t =
    | Foreground_continue of Activation.t
    (** Genuine ordinary/legacy foreground execution. Has no Workflow mode. *)
    | Authored_start of Agent_protocol.Run.Mode.t
    (** Genuine unconsumed versioned moderator startup in the explicitly selected
        mode. start_run owns preparation+activation itself; never call
        session.start beforehand. Single_turn completes actual inert startup or
        its owned foreground operation; Workflow waits for its genuine terminal. *)
    | User_submission of
        { mode : Agent_protocol.Run.Mode.t
        ; content : Agent_protocol.Session.Message_content.t
        ; activation : Activation.t
        }
    | Observe_run of Agent_protocol.Run_receipt.t
    (** Existing original Admission receipt, no new start/key. *)
end

module Evidence : sig
  type t =
    | Foreground of
        { operation : Agent_protocol.Operation.t
        ; admission : Agent_protocol.Mutation_result.t
        }
    | Managed_run of
        { admission : Agent_protocol.Run_receipt.t
        ; terminal : Agent_protocol.Run_query.View.t
        }
    (** Terminal run lifecycle and its actual terminal receipt, including
            Failed/Cancelled/Limited/Interrupted. A completed foreground operation
            or an idle session never substitutes for Workflow terminal. *)
end

module Completed : sig
  (** Immutable presentation evidence, not replay/execution authority. *)
  type t = private
    { evidence : Evidence.t
    ; export : Agent_client.Canonical_export.t
    ; chatmd : string
    }
end

module Error : sig
  type t =
    | Protocol of Agent_protocol.Error.t
    | Continuation_unavailable of Agent_protocol.History_edit.Continuation.unavailable
    | Rendering of History_chatmd.Rendering.Error.t
end

(** [connection] is initialized and genuinely owned by the caller's host. Main's
    minimal local route uses Embedded.with_local_host + host_connection (no initial
    attachment), then genuine234 seed/current retained selection yields session_id.
    Do not create a host/session, select source, adopt history, infer role/provider,
    acquire grants, or choose frontend defaults here. Requires this connection's
    notification lease to be unclaimed; it is not reusable beside a subscribed UI.

    Own exactly one subscribed Read_write Session_handle in a nested Switch. Its
    reader/reducer and renewal are the sole notification owner. Close/detach/join
    before return, without closing the borrowed connection/host. Existing
    Embedded.start/attach_retained create a DISTINCT bootstrap attachment; they
    are not the proposed main route and this function cannot pretend to adopt or
    clean their privately owned bootstrap attachment. See the caller memo.

    The fresh subscribed Read_write attachment commits exactly one session
    revision change. Before activation/admission, require unchanged generation
    and attached revision exactly the checked successor of the caller's selected
    revision. Any other intervening mutation conflicts; there is no latest-state
    fallback. Commands use the actual returned attached positions. Start_stopped uses actual
    Session_handle.start ~queue_if_limited:false exactly once if Stopped, then uses
    its returned session positions (not an arbitrary latest projection). Running
    must already be Idle. Failed/queued/competing state does not auto-wait/retry.
    Initialization/resume remain the actual Factory/Runtime owner responsibility.
    Authored_start never uses this separate activation path: start_run initializes
    Pending state under run preparation and durably admits the run before making
    it Running, using its genuine pending Session_start. A consumed startup cannot
    be recreated on retained attach or inferred from missing history.

    [prepare], when supplied, borrows this same handle only after the original
    attachment successor guard. It may perform the caller's actual stopped-only
    rebuild and genuine retained source query, returning the request and command's
    actual session positions. It must not escape the handle or install a reader.
    After the callback and yielding checkpoints, wait for the returned command's
    exact ID/generation/revision on this handle's existing reader. A behind
    projection waits; foreign identity, overshoot, closure or reader failure rejects.
    Recheck exact equality before activation/admission; never substitute newer
    positions or install another reader. Original exceptions propagate.

    Foreground_continue uses actual continue_history Started ID+mutation and
    Completion_view.await_terminal; Not_started retains its typed reason.
    Managed branches use actual Session_handle.start_run, preserving the original
    Admission receipt and the borrowed Connection's command reconciliation owner.
    No re-keyed retry/fallback on any error. [on_admission] receives the actual
    start_run Admission receipt before terminal waiting, permitting the caller to
    report its original run ID even for long workflows or later interruption.
    It runs in the caller domain under the same scoped cleanup; original callback
    exceptions/cancellation propagate. Observe_run admits no mutation and invokes
    no new-admission callback.

    Watch exact host-qualified run_id with Run_views.watch/lookup, multiplexing
    its nonyielding observer into the SAME handle.on_update. A run ID is known only
    after admission: install watch then explicitly feed the actual current
    projection, so already-terminal admission cannot be lost. Initial/final lookup
    rechecks current authorization. Correlate session/source/principal/mode and
    exact original admission receipt with the actual View; unavailable/expired
    evidence never authorizes resubmission. Await actual Run.Lifecycle.Terminal
    with actual terminal receipt. Admitted/Active/Waiting keep waiting; operation
    replacement is ordinary Workflow progress, not Workflow completion/conflict.
    Race the handle's real await_closed; stale/reader failure or closure must wake
    the waiter even without update. All watcher/race fibers join within scope.

    on_view runs in caller, never reader/watch callbacks. It receives actual
    bounded whole-session projection/Draft/activity, with honest gaps/replacements;
    it is not a provider-delta or assertion that every activity belongs to the run.
    Reconcile actual state after yielding callbacks and before result delivery.

    Canonical_export.read uses the authorized terminal lookup/projection's exact
    session revision. Managed final evidence must match its session/source
    generation and snapshot positions; use the View's current session positions,
    not an assumed archived terminal-receipt snapshot. No latest-revision fallback
    if export races mutation. Require complete Full canonical history, including
    compaction/rewrite; render with existing bounded Chatmd_export template. Return
    bytes only. No declaration flattening, file/stdout writes, user placeholder,
    automatic session stop, background-job kill or CLI exit/default decision.

    Original checkpoint/on_view failures, cancellation and backtraces propagate.
    Check Eio before/after callbacks and before delivery. Protected close/detach attempts preserve the original primary failure;
    a cleanup exception after otherwise successful completion propagates. The
    borrowed connection/host still owns actual transport-context cleanup, including
    attachments if a detach attempt fails. No new handle retry/lifecycle framework. Original uncertain command custody stays on the borrowed Connection
    for caller reconciliation; no usable handle/watcher/callback escapes. *)
val run
  :  Agent_client.Connection.t
  -> clock:_ Eio.Time.clock
  -> session_id:Agent_protocol.Id.Session.t
  -> expected_generation:int
  -> expected_revision:int64
  -> request:Request.t
  -> prepare:
       (Agent_client.Session_handle.t
        -> Agent_protocol.Session.t
        -> (Request.t * Agent_protocol.Session.t, Error.t) Result.t)
         option
  -> document_limits:Document_schema.Limits.t
  -> rendering_limits:History_chatmd.Rendering.Limits.t
  -> checkpoint:(unit -> unit)
  -> on_view:(Agent_client.Projection.t -> unit)
  -> on_admission:(Agent_protocol.Run_receipt.t -> unit)
  -> (Completed.t, Error.t) Result.t
