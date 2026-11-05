open! Core
module P = Agent_protocol

module Basis : sig
  type t = private
    { session : P.Session_ref.t
    ; generation : int
    ; state_revision : int64
    ; catalog_revision : int64
    ; lifecycle_revision : P.Session_lifecycle.Revision.t
    ; automatic : bool
    ; source : P.Run_source.t option
    }

  val equal : t -> t -> bool
end

module Definition_view : sig
  (** Safe metadata plus full schedule (including an interval's first_due).
      No captured input, files, private resource pins or target permission. *)
  type t
end

module Status : sig
  (** Bounded controller-owned DATA, not a target observation or retry authority.
      No input, artifacts, private selectors, workspace paths or receipt keys. *)
  type t
end

module History : sig
  (** Retained selected/omitted/withdrawn ranges and attempt audit/outcome rows.
      Lifetime totals are distinct from finite retained detail. Missing historical
      frontier support is an explicit error, never an empty successful history.
      This internal snapshot retains references needed for visibility checks;
      it has no public/raw JSON serializer. *)
  type t
end

type request =
  | List
  | Get of P.Id.Recurrence.t
  | Status of P.Id.Recurrence.t
  | History of P.Id.Recurrence.t

(** Opaque read snapshot carrying only its actual controller observation for
    final denial-only currentness checks. It is not a target or execution proof. *)
type t

(** Same current-original/read/Current contract as existing read_recurrences.
    The Actor adds actual process-local planning availability separately; this
    operation never resolves a target, creates a source loan or loads a runtime. *)
val read
  :  Session_state.t
  -> controller:Recurrence_controller.t
  -> actor:Operator_authorization.t
  -> authorize:(Session_state.t -> (unit, P.Error.t) Result.t)
  -> planning:Recurrence_planning_status.t
  -> request:request
  -> (t, P.Error.t) Result.t

val basis : t -> Basis.t

(** Pure denial-only check on the actual observation issued by [read]. Invoke
    after all yielding host/visibility checks, before disclosure. No refresh,
    permission, mutation, execution or retry authority is issued. *)
val check_current : t -> (unit, P.Error.t) Result.t

(** Trusted read composition, OUTSIDE the Actor mailbox. [read_target] is the
    actual currently authorized immutable registry reader, never an activating
    loader or acquire-target operation. Read one target snapshot per occurrence, shared by its attempts; drop that
    complete session snapshot before the next occurrence. No whole-history cache;
    work is bounded by this single recurrence's retained frontier/attempt limits.

    Controller counts/ranges/audit codes are retained metadata. Run/session IDs,
    imported outcome tags and current Run status are disclosed only after current
    target visibility succeeds. Permission_denied/Session_not_found produce an
    explicit redacted/unavailable target projection, never empty history; other
    errors propagate. A missing exact retained Run/receipt yields explicitly
    unavailable detail, not an absence/no-effects proof. Visible Run snapshots
    validate the exact retained origin/admission association and expose only
    compact lifecycle/outcome tags: no result history or wake token.

    Output labels retained controller evidence separately from a target snapshot
    at its actual session/Run revisions. This issues NO Recurrence_run_observation,
    import, terminal proof, retry capability or mutation. Target states are separately authorized historical snapshots. Neither final
    controller checks nor this renderer establish target-state freshness after
    its own read, cross-target atomicity, or current execution authority.

    Caller must check its current native/original/host admission before and after
    this operation AND re-read the controller basis after target IO. Perform all Actor/
    registration IO before the final SAME-service yielding host check, then only
    pure admission/current-observation/basis checks through disclosure. Changed basis rejects
    with Conflict; an expired caller rejects the entire read even if a per-target
    Permission_denied was locally rendered as redacted. *)
val render
  :  t
  -> read_target:(P.Session_ref.t -> (Session_state.t, P.Error.t) Result.t)
  -> (Jsonaf.t, P.Error.t) Result.t
