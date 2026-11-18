open! Core

(** Compact immutable cursor metadata, extracted from a definitive fixed-delay
    execution. Full input and result history remain with their existing owners.
    Native and decoded values are data, never terminal observations or rearm
    authority. Only an authenticated terminal import may persist a new basis. *)
type t

val create
  :  Recurrence_execution.t
  -> definition:Recurrence_definition.t
  -> (t, Agent_protocol.Error.t) result

val occurrence : t -> Recurrence_identity.Occurrence.t
val definition_sha256 : t -> string
val nominal_at : t -> Time_ns.Alternate_sexp.t
val origin : t -> Recurrence_run_origin.t
val terminal : t -> Recurrence_run_reference.t
val completed_at : t -> Agent_protocol.Timestamp.t

(** Exact definition identity/digest, fixed-delay mode and definitive terminal
    association. A restored cached outcome is historical data, not proof of the
    actual target payload; live import separately authenticates the target. *)
val validate
  :  t
  -> definition:Recurrence_definition.t
  -> (unit, Agent_protocol.Error.t) result

(** Checked successor through the existing timing owner, from the exact prior
    selected slot/nominal and terminal receipt UTC. Slot/timestamp overflow is an
    error, never an exhausted or successful rearm claim. No clock is read. *)
val next_interval
  :  t
  -> definition:Recurrence_definition.t
  -> (Recurrence_rules.Cursor.Interval.t, Agent_protocol.Error.t) result

(** Complete original JSON, including child custody. Unknown basis metadata must
    move with the old complete value to bounded control history on replacement
    or definition adoption; it must not be retargeted onto another occurrence. *)
val to_json : t -> Jsonaf.t

val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val equal : t -> t -> bool

(** Full Origin and Reference profiles plus the measured wrapper and a finite
    extension allowance. Enclosing frontier/control-history and actual whole
    persistence profiles remain independent and may reject earlier. *)
val limits : Document_schema.Limits.t
