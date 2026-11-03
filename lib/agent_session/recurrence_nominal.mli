open! Core

(** Immutable selected nominal occurrence metadata, never an admission or terminal
    receipt. Stores the exact definition digest and UTC selection; calendar labels
    retain their admitted data provenance. Historical decode validates shape, not
    the authenticity of a source, a Zone or an execution. *)
type t [@@deriving equal, sexp_of]

module Calendar_label : sig
  type t = private
    { date : Date.t
    ; ofday : Time_ns.Ofday.t
    ; fold : int
    ; shifted_gap : bool
    ; zone : string
    ; provenance : Recurrence_rules.Zone.Provenance.t
    }
  [@@deriving equal, sexp_of]
end

type label =
  | Interval
  | Calendar of Calendar_label.t
[@@deriving equal, sexp_of]

(** Compute the selected slot with the timing owner's checked arithmetic.
    Fixed-delay construction in this owner is limited to the initial occurrence;
    subsequent slots require actual terminal evidence from its execution owner. *)
val create_interval
  :  Recurrence_definition.t
  -> slot:int64
  -> (t, Agent_protocol.Error.t) result

(** Copy a genuine pure calendar emission and assign its next nominal slot.
    The caller/plan validates its actual cursor/Zone association separately. *)
val create_calendar
  :  Recurrence_definition.t
  -> slot:int64
  -> occurrence:Recurrence_rules.Cursor.Calendar.Occurrence.t
  -> (t, Agent_protocol.Error.t) result

val key : t -> Recurrence_identity.Occurrence.t
val definition_sha256 : t -> string
val at : t -> Time_ns.Alternate_sexp.t
val label : t -> label

val validate
  :  t
  -> definition:Recurrence_definition.t
  -> (unit, Agent_protocol.Error.t) result

(** Bounded plain projection. Compatible unknowns belong to the enclosing frontier
    carrier, including row identities. Unsupported label variants fail closed. *)
val to_json : t -> Jsonaf.t

val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t

(** Shared complete plain-projection admission bound for immutable stored rows. *)
val limits : Document_schema.Limits.t
