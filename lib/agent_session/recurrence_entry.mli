(** Durable metadata control for one recurrence ID. Immutable definition revisions
    and a shape-validated cursor are distinct from control revision. This owner
    does not yet produce due alarms, occurrences or target execution evidence. *)
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

(** Pure metadata changes. These functions supply no original operator, source,
    lifecycle or execution authority; the actor validates those separately.
    At most eight immutable revisions are retained; edits reject saturation.
    A retired ID is a permanent tombstone and cannot be edited or enabled. *)
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
    replaced. A changed entry advances control once. Decoded updates must obey the
    same fences; historical snapshot admission is separate. *)
val validate_transition : t -> incoming:t -> (unit, Agent_protocol.Error.t) result

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
