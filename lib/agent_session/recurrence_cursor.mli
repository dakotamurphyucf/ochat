open! Core

(** Durable frontier tied to one immutable definition revision. Constructors
    create initial cursors; checked transitions use the existing timing owner.
    No API accepts an arbitrary next position. Codecs
    admit historical local shape, not proof of consumed slots, a live elapsed
    phase, actual completion, or a calendar candidate under current zone rules. *)
type t [@@deriving equal, sexp_of]

type position =
  | Interval of Recurrence_rules.Cursor.Interval.t
  | Calendar of
      { cursor : Recurrence_rules.Cursor.Calendar.t
      ; next_slot : int64
      }
  | Awaiting_completion of Recurrence_identity.Occurrence.t
  | Exhausted
[@@deriving equal, sexp_of]

val create_interval
  :  Recurrence_identity.Definition.t
  -> first_due:Time_ns.Alternate_sexp.t
  -> t

val create_calendar
  :  Recurrence_identity.Definition.t
  -> rule:Recurrence_rules.Rule.Calendar.t
  -> t

val definition : t -> Recurrence_identity.Definition.t
val position : t -> position

type interval_step =
  { consumed : Recurrence_rules.Cursor.Interval.due_range
  ; next : t
  }

val consume_interval
  :  t
  -> rule:Recurrence_rules.Rule.Interval.t
  -> count:int64
  -> (interval_step, Agent_protocol.Error.t) result

type calendar_search =
  | Found of Recurrence_rules.Cursor.Calendar.Occurrence.t * t
  | Continue of t
  | End of t

(** Advance only the calendar search. Found reserves this cursor's next slot;
    consuming that selected nominal remains a distinct checked operation.
    The enclosing frontier retains any unconsumed lookahead and provenance. *)
val search_calendar
  :  t
  -> rule:Recurrence_rules.Rule.Calendar.t
  -> zone:Recurrence_rules.Zone.t
  -> max_steps:int
  -> (calendar_search, Recurrence_rules.Timing_error.t) result

val consume_calendar
  :  t
  -> occurrence:Recurrence_identity.Occurrence.t
  -> nominal_at:Time_ns.Alternate_sexp.t
  -> (t, Agent_protocol.Error.t) result

(** Initial fixed-delay selection waits for the actual execution owner. No
    transition here re-arms Awaiting_completion from a callback or copied tag. *)
val consume_initial_delay
  :  t
  -> rule:Recurrence_rules.Rule.Interval.t
  -> (t, Agent_protocol.Error.t) result

(** Current fixed-delay interval slot to its exact Awaiting_completion identity.
    This consumes one selection, never rearms it. The enclosing frontier verifies
    later-slot deadline association against its exact completion basis. *)
val consume_delay
  :  t
  -> rule:Recurrence_rules.Rule.Interval.t
  -> (t, Agent_protocol.Error.t) result

(** Specific completion transition, not an arbitrary position setter: requires
    the exact Awaiting_completion occurrence and next.slot = prior.slot + 1.
    The enclosing owner computes [next] through the timing owner from its exact
    validated terminal basis; this local shape transition grants no proof. *)
val rearm_delay
  :  t
  -> occurrence:Recurrence_identity.Occurrence.t
  -> next:Recurrence_rules.Cursor.Interval.t
  -> (t, Agent_protocol.Error.t) result

(** Validate rule kind and same-definition fixed-delay completion identity.
    Calendar candidate correctness is rechecked by Cursor.Calendar.next using
    the actual admitted Zone; decoding alone grants no scheduling authority. *)
val validate : t -> rule:Recurrence_rules.Rule.t -> (unit, Agent_protocol.Error.t) result

val to_json : t -> Jsonaf.t

val of_json
  :  Jsonaf.t
  -> rule:Recurrence_rules.Rule.t
  -> (t, Agent_protocol.Error.t) result

val shape : Document_schema.Shape.t

(** Supplied rule/Zone association belongs to the owning due planner and frontier.
    These checked operations provide timing arithmetic/search, never source proof. *)
