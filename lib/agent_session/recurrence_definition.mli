open! Core

(** One immutable captured recurrence revision. Its complete universal document
    owns its compatible unknown metadata and embeds the complete target carrier
    as an opaque value. It stores no live clock phase, control flags or writable
    execution state. An enclosing owner authenticates creation and currentness. *)
type t

module Schedule : sig
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
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
end

val create
  :  key:Recurrence_identity.Definition.t
  -> creator:Agent_protocol.Id.Principal.t
  -> source:Agent_protocol.Run_source.t
  -> birth_lifecycle_revision:Agent_protocol.Session_lifecycle.Revision.t
  -> schedule:Schedule.t
  -> target:Recurrence_target.t
  -> policy:Recurrence_policy.t
  -> created_at:Agent_protocol.Timestamp.t
  -> (t, Agent_protocol.Error.t) result

val key : t -> Recurrence_identity.Definition.t
val creator : t -> Agent_protocol.Id.Principal.t
val source : t -> Agent_protocol.Run_source.t
val birth_lifecycle_revision : t -> Agent_protocol.Session_lifecycle.Revision.t
val schedule : t -> Schedule.t
val target : t -> Recurrence_target.t
val policy : t -> Recurrence_policy.t
val created_at : t -> Agent_protocol.Timestamp.t
val initial_cursor : t -> Recurrence_cursor.t
val digest : t -> string
val of_document : Document_schema.Document.t -> (t, Document_schema.Error.t) result
val to_document : t -> Document_schema.Document.t
