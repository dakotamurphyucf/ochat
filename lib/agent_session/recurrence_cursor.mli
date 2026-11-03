open! Core

(** Durable shape-only frontier tied to one immutable definition revision.
    Constructors create initial cursors only. No API accepts an arbitrary next
    position; the due owner adds evidence-checked progression separately. Codecs
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
