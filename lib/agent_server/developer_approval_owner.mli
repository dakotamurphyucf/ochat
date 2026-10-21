open! Core
module P = Agent_protocol
module Store = Agent_store.Developer_run_store
module Root = Developer_root_owner
module Lifecycle = Developer_command_lifecycle

(** Process-local, bounded shell approval ownership for one physical Developer
    run generation. Store Command/Job and Root retain execution custody; this
    module adds no durable grant, Session actor or replay path. *)
type t

module Limits : sig
  type t

  (** Comparison-only validated ceilings for portable composition. *)
  val max_pending : t -> int

  val max_display_bytes : t -> int
  val max_decision_reason_bytes : t -> int

  (** Request capacity is at least 2048 bytes for fixed context framing; the
      other byte bounds and pending count are positive. The finite wait is
      representable on the daemon's monotonic clock and at most one hour.
      Request bytes include the broker's
      retained Shell approval context; display bytes bound only the redacted
      client projection. [wait_seconds] is enforced on the daemon monotonic
      clock, independently of any connected client. *)
  val create
    :  max_pending:int
    -> max_request_bytes:int
    -> max_display_bytes:int
    -> max_decision_reason_bytes:int
    -> wait_seconds:float
    -> (t, P.Error.t) result
end

module Configuration : sig
  type t

  (** Host supplies its exact existing command disclosure and control
        decisions, including method authorization, as distinct callbacks.
        Job decisions receive the immutable row selected from this Root's
        latest-Active Store observation, with exact owner, capture and
        attempt rechecked. All are trusted daemon callbacks; a pending DTO
        never supplies policy or authority. These trusted policy callbacks
        must be nonyielding and nonraising, so the final currentness check and
        broker response stay one local decision. A read decision never grants
        response. *)
  val create
    :  limits:Limits.t
    -> command_read:(Operator_authorization.t -> Store.Command.Row.t -> bool)
    -> command_respond:(Operator_authorization.t -> Store.Command.Row.t -> bool)
    -> job_read:(Operator_authorization.t -> P.Developer_job.t -> bool)
    -> job_respond:(Operator_authorization.t -> P.Developer_job.t -> bool)
    -> t
end

module Pending : sig
  type t

  type owner =
    | Command of Agent_store.Developer_command_receipt.Id.t
    | Claimed_job of
        { job_id : P.Id.Job.t
        ; attempt : int
        }

  type execution =
    | Native_invocation of P.Id.Invocation.t
    | Claimed_model_job of
        { job_id : P.Id.Job.t
        ; attempt : int
        }

  val id : t -> P.Id.Permission.t
  val owner : t -> owner
  val execution : t -> execution
  val runtime_id : t -> string
  val manifest_sha256 : t -> string
  val redacted_display : t -> string
  val expires_at : t -> P.Timestamp.t
end

type decision =
  | Approve_once
  | Deny of string

(** Create only after the exact fresh durable Start, using the one-use
    Preparing token. Until Root pairing it retains that one-use token; after
    [bind_root] it retains only the paired Root and physical Store, never the
    first Active as authority for future commands. The broker may be
    passed into Live resource preparation, but cannot publish an approvable
    request without a later entered Started/Claim/native callback. The current
    Shell registry activation gate prevents a setup Shell approval request. *)
val create
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> store:Store.t
  -> preparing:Lifecycle.Active.Preparing.t
  -> configuration:Configuration.t
  -> (t, P.Error.t) result

(** Trusted nonyielding, nonraising hint after the process-local pending index
    changes, outside its mutex. Only an entered Live Started command supplies
    a row; claimed Jobs and REPL Tasks have no Command watch key here. *)
val create_with_change
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> store:Store.t
  -> preparing:Lifecycle.Active.Preparing.t
  -> configuration:Configuration.t
  -> on_change:(Store.Command.Row.t -> unit)
  -> (t, P.Error.t) result

(** After [Preparing.bind_root] returns [started], pair this owner with that
    exact physical Root once. [Preparing.issued_started] verifies the token
    pair; Root must be issued by [store] and [started] by Root. Failed pairing
    closes the broker; an epoch replacement creates a different owner. This
    transition is one-way: Awaiting_pair becomes Bound_root, with no retained
    Preparing or first-command callback. Host pairs the owner before attaching
    Native services or a scheduler, so no unbound owner can enter a callback. *)
val bind_root
  :  t
  -> root:Root.run
  -> started:Lifecycle.Active.Started.t
  -> (unit, P.Error.t) result

(** Comparison only: true for the exact physical Root paired by [bind_root].
    Native.with_approval_owner requires this before installing callbacks. *)
val issued_by_root : t -> Root.run -> bool

(** Enter an exact current Live or REPL Task callback, or a claimed Worker.
    Every later action supplies its own fresh Started token; the first Active
    is never reused. Lifecycle validates the token's closed Live-original or
    Task-original/current actor pairing against this exact physical Root.
    Task approval read/respond requires REPL plus Live permission and its own
    row policy; the ordinary Command DTO cannot disclose a Task pending row.
    The claimed branch derives Root.job_parent from the actual active Worker.
    Fiber-local scopes expire on return and are rechecked after waits. The
    Root source fingerprint is the exact command capture or retained REPL
    selection, never a fabricated command capture for Task. Neither a saved
    row nor a pending ID can enter these scopes. *)
val with_started_command
  :  t
  -> active:Lifecycle.Active.t
  -> started:Lifecycle.Active.Started.t
  -> actor:Operator_authorization.t
  -> (unit -> ('a, P.Error.t) result)
  -> ('a, P.Error.t) result

val with_claimed_job
  :  t
  -> active:Agent_session.Developer_job_scope.active
  -> (unit -> ('a, P.Error.t) result)
  -> ('a, P.Error.t) result

(** Trusted Developer_native_executor enters this only inside its exact
    Root.with_native_callback after durable Dispatching. The terminal and
    dispatched invocation are comparison data for pending projection. The
    native executor separately installs Shell Call_context whose getter
    re-runs its full effect_entry(binding), including current Root, selected
    child or claimed source, exact implementation and physical parent; it
    also checks the current borrowed invocation against [dispatched]. A
    terminal completion check is insufficient for this new effect. *)
val with_native_invocation
  :  t
  -> terminal:Root.Native_terminal.t
  -> dispatched:P.Host_invocation.t
  -> (unit -> ('a, P.Error.t) result)
  -> ('a, P.Error.t) result

(** Enter only within the same Root.Model_inference.with_claimed callback for
    the actual persisted Model_call Worker. This lends the owner's shell
    services for the strict File recipe/Driver lifetime with a
    [Claimed_model_job] pending origin, never an invented native invocation.
    The service getter calls Root.Model_inference.check_claimed_approval on
    this physical scope after each yielded reviewer wait. The outer Worker
    Driver-owned moderator and inference children join before this loan
    expires; the outer Root Worker Switch joins after it. Client read/respond
    checks use durable observation and owner liveness instead of borrowing
    this execution-fiber proof. *)
val with_claimed_model_inference
  :  t
  -> active:Agent_session.Developer_job_scope.active
  -> scope:Root.Model_inference.t
  -> (unit -> ('a, P.Error.t) result)
  -> ('a, P.Error.t) result

val provider : t -> Shell_runtime.Approval_broker.provider
val approval_store : t -> Shell_access.Approval.store
val shell_owner_label : t -> string

(** Authenticated live projection only. IDs, owners and redacted display are
    comparison data; no raw Shell approval request or responder is exposed.
    Reads recheck original/current actor, Root generation and the matching
    command disclosure or Job read policy after any yielded Store lookup. *)
val pending : t -> actor:Operator_authorization.t -> (Pending.t list, P.Error.t) result

(** Resolve once only after the separate response policy, current original
    actor, exact command/Claim attempt and owner/source checks. A changed,
    expired, answered or foreign pending request fails closed. Shell_access
    rechecks full native admission after its reviewer wait and before effects.
    [Deny] reason must fit [Limits.max_decision_reason_bytes] and is validated
    before resolving the pending request. *)
val respond
  :  t
  -> actor:Operator_authorization.t
  -> id:P.Id.Permission.t
  -> decision
  -> (unit, P.Error.t) result

val cancel
  :  t
  -> actor:Operator_authorization.t
  -> id:P.Id.Permission.t
  -> (unit, P.Error.t) result

(** Deny all pending waits and remove projections on run retirement, joined
    cancellation and daemon shutdown. The existing command/Worker owners
    settle truthful terminal receipts; closing does not replay an effect. *)
val close : t -> unit
