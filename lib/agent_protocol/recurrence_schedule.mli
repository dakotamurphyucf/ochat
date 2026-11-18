open! Core

(** Interval origin is explicit wall-time metadata, independent of the live
    elapsed phase. Calendar begins at its actual rule's start date. *)
type t =
  | Interval of
      { rule : Recurrence_rules.Rule.Interval.t
      ; first_due : Time_ns.Alternate_sexp.t
      }
  | Calendar of Recurrence_rules.Rule.Calendar.t
[@@deriving sexp_of]

val rule : t -> Recurrence_rules.Rule.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Protocol_error.t) result
val of_public_json : Jsonaf.t -> (t, Protocol_error.t) result
val shape : Document_schema.Shape.t
