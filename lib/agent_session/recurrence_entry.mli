(** Durable metadata control for one recurrence ID. Immutable definition revisions
    and a shape-validated cursor are distinct from control revision. The pure due planner owns unadmitted nominal rows/alarms in a separate frontier.
    Pure imports retain associated target execution data; actual observation
    authentication and activation remain with their effectful owners. *)
type t

module Pause : sig
  type t =
    | Operator_pause
    | Controller_stop
    | Source_review
    | Recovery_review
  [@@deriving equal, sexp_of]
end

module Retirement : sig
  type t =
    | Operator_retire
    | Lifecycle_changed
  [@@deriving equal, sexp_of]
end

type control =
  | Paused of Pause.t
  | Enabled
  | Retired of Retirement.t
[@@deriving equal, sexp_of]

(** New definitions start paused at control revision zero. The immutable revision
    key must start at zero; identity and creator never change afterward. *)
val create : Recurrence_definition.t -> (t, Agent_protocol.Error.t) result

val id : t -> Agent_protocol.Id.Recurrence.t
val controller : t -> Agent_protocol.Session_ref.t
val definitions : t -> Recurrence_definition.t list
val current : t -> Recurrence_definition.t
val control : t -> control
val control_revision : t -> int64
val cursor : t -> Recurrence_cursor.t

(** Historical entries without frontier custody remain readable and may be
    paused/retired, but automatic planning requires a newly created identity.
    No history is inferred from an initial-looking current cursor. *)
val frontier : t -> (Recurrence_frontier.t, Agent_protocol.Error.t) result

(** Move an exact Ready nominal into immutable protected creation custody while
    Enabled. No actor authority is supplied; an existing reservation must match
    the original control frontier. Repeated exact data preserves all IDs. *)
val reserve_child
  :  t
  -> principal_id:Agent_protocol.Id.Principal.t
  -> child_session_id:Agent_protocol.Id.Session.t
  -> transaction_id:Agent_protocol.Id.Transaction.t
  -> occurrence:Recurrence_identity.Occurrence.t
  -> (t * Recurrence_child_reservation.t, Agent_protocol.Error.t) result

(** Immutable parent link candidate while enabled, preserving the current
    definition/control/cursor. Actual child proof and ACK belong to the actor. *)
val link_child : t -> Recurrence_child_association.t -> (t, Agent_protocol.Error.t) result

(** Pure ordinal-zero record candidate. Exact replay preserves original audit
    data; a new request requires the current enabled definition/control. The
    publisher authenticates actual current authority and persistence separately. *)
val record_execution
  :  t
  -> intent:Recurrence_execution_intent.t
  -> attempt:Recurrence_execution_attempt.t
  -> (t * Recurrence_execution.t, Agent_protocol.Error.t) result

(** Resolve the exact retained current predecessor and append one contiguous
    attempt in one pure candidate. Complete intent, audit evidence and compatible
    wrappers remain retained. Exact current-successor/predecessor/resolution DATA
    replay returns the unchanged carrier; later successors are not this replay.
    A new rotation requires the current Enabled definition/control.
    No no-effects, retry or target authority is issued. No pruning or occupancy,
    cursor, control or lifetime-count movement accompanies this operation. *)
val rotate_execution
  :  t
  -> intent:Recurrence_execution_intent.t
  -> previous:Recurrence_execution_attempt.t
  -> resolution:Recurrence_retry_resolution.t
  -> next:Recurrence_execution_attempt.t
  -> (t * Recurrence_execution.t, Agent_protocol.Error.t) result

(** Apply one exact-basis opaque pure plan while Enabled. Cursor/frontier advance
    together; control revision and immutable definitions stay unchanged. This is
    not lifecycle/source authorization or a persistence/dispatch operation. *)
val apply_due : t -> Recurrence_due_plan.t -> (t, Agent_protocol.Error.t) result

(** Pure metadata changes. These functions supply no original operator, source,
    lifecycle or execution authority; the actor validates those separately.
    At most eight immutable revisions are retained; edits reject saturation.
    A retired ID is a permanent tombstone and cannot be edited or enabled.
    A genuine new enable from an ordinary pause can consume an exact definitive
    delay basis once, keeping its receipt-derived deadline. Review-held completed
    or interrupted waits require explicit definition recovery; duplicate enable
    never rearms. *)
val edit : t -> Recurrence_definition.t -> (t, Agent_protocol.Error.t) result

val enable : t -> (t, Agent_protocol.Error.t) result
val pause : t -> reason:Pause.t -> (t, Agent_protocol.Error.t) result
val retire : t -> reason:Retirement.t -> (t, Agent_protocol.Error.t) result

(** Internal safety disposition advances only enabled controls, preserving an
    existing pause and all definition/cursor custody. It never proves cancellation
    of external work. Retirement applies to every nonretired control. *)
val hold_for_review : t -> (t option, Agent_protocol.Error.t) result

val retire_lifecycle : t -> (t option, Agent_protocol.Error.t) result

(** Complete previous immutable revisions and unknowns cannot disappear or be
    replaced. Control changes advance the control revision once; exact-basis due plans
    advance the planning revision while leaving control unchanged. Decoded updates obey the
    same fences; historical snapshot admission is separate. Paused and retired
    entries admit exact evidence-only settlement without resurrection. *)
val validate_transition : t -> incoming:t -> (unit, Agent_protocol.Error.t) result

(** Validate only the exact single execution-origin/terminal import projection by
    reconstructing the existing atomic settlement. Paused/retired entries cannot
    rearm; only Enabled may use current automatic policy. This is DATA validation,
    not target observation or permission, and does not replace [validate_transition]
    and its complete extension-carrier retention checks. *)
val validate_execution_import : t -> incoming:t -> (unit, Agent_protocol.Error.t) result

val of_document : Document_schema.Document.t -> (t, Document_schema.Error.t) result
val to_document : t -> (Document_schema.Document.t, Document_schema.Error.t) result

(** Stateless projection under a genuine current lifecycle observation supplied
    by the owner. A newer birth is inconsistent. Effective retirement changes no
    stored revision and never asserts that reconciliation already persisted. *)
val effective_control
  :  t
  -> current_revision:Agent_protocol.Session_lifecycle.Revision.t
  -> archived:bool
  -> (control, Agent_protocol.Error.t) result

(** Positive known control-disposition growth, excluding enclosing catalog/host
    counters. Shared owners add this reservation to independent obligations. *)
val reserved_growth : t -> (Capacity_growth.t, Agent_protocol.Error.t) result

val required_transition_steps : t -> int

(** Each immutable reservation without a retained association needs one later link. These
    independent steps add to bulk control obligations even after pause/retirement.
    This count conveys no proof that a child exists or is linked. *)
val pending_child_associations : t -> int

(** Private capacity-only future wire witness with bounded known control and
    decimal growth. Never decode it as an entry, install it, or treat it as
    retirement evidence. All compatible child unknowns remain present. *)
val capacity_projection : t -> (Jsonaf.t, Document_schema.Error.t) result

(** Independent row proof commits, added to other owners' reserved transitions. *)
val pending_execution_steps : t -> int

(** Pure atomic terminal/origin data import. Paused/retired controls always force
    evidence-only behavior regardless of the supplied policy data. A new current
    definitive delay completion stores a basis and either rearms or remains held;
    original intent control revision is audit only. The actual publisher must
    authenticate the target observation and all current policy separately. *)
val import_execution
  :  t
  -> origin:Recurrence_run_origin.t
  -> terminal:Recurrence_run_reference.t option
  -> automatic:Recurrence_settlement.Automatic.t
  -> (t * Recurrence_settlement.t, Agent_protocol.Error.t) result
