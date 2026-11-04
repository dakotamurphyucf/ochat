(** Private execution table in the existing frontier. Ordered complete raw
    wrappers preserve compatible fields; typed occurrence lookup avoids repeated
    row scans. Missing and null wire fields remain distinct. No authority lives
    in this table. *)
type t

val empty : t
val rows : t -> Recurrence_execution.t list
val find : t -> Recurrence_identity.Occurrence.t -> Recurrence_execution.t option

(** Only initial ordinal-zero recording is constructed here. Duplicate compares
    the complete original intent/nominal and exact attempt zero and returns the
    original row, without extending history or granting retry permission. *)
val record
  :  t
  -> intent:Recurrence_execution_intent.t
  -> attempt:Recurrence_execution_attempt.t
  -> (t * Recurrence_execution.t, Agent_protocol.Error.t) result

type change =
  | Associated
  | Settled of Recurrence_identity.Occurrence.t list

type imported =
  | Absent
  | Retained of Recurrence_execution.t
  | Changed of
      { execution : Recurrence_execution.t
      ; change : change
      }

(** Associate the exact current attempt and optionally settle in one pure table
    candidate. No actor observation is issued. A first terminal import retires
    whole oldest known-terminal rows to max_history (1 through 256), independent
    of unresolved slot ordering, and returns their exact occurrence keys so the
    enclosing owner retires child reservations/associations in the same candidate.
    Unresolved/uncertain rows never retire. Complete compatible wrappers move
    intact while retained; whole detail retirement follows explicit retention.
    Absent is only an absence result, not a completion/no-effects claim. *)
val import
  :  t
  -> origin:Recurrence_run_origin.t
  -> terminal:Recurrence_run_reference.t option
  -> max_history:int
  -> (t * imported, Agent_protocol.Error.t) result

(** Complete raw field custody. The table's defensive ceiling is 512 rows:
    at most 256 claimed unresolved occurrences and 256 settled bundles across
    all retained definitions. The enclosing owner must enforce the complete claimed union
    and current retention before installation; this is not a new authoring pending quota. Existing
    protected inventory and lowered policies remain inspectable. Native recording
    and decoding validate the same complete array profile: 2 MiB, depth 48,
    32768 fields and 65536 nodes, before returning a table. The enclosing carrier
    separately charges wrapper/envelope and mandatory future growth. *)
val fields : t -> (string * Jsonaf.t) list

val of_field : Jsonaf.t option -> (t, Agent_protocol.Error.t) result

(** Retained rows preserve immutable evidence and compatible wrappers. A single
    evidence change may move a just-terminal row to the end and retire other
    whole known-terminal details; unresolved/uncertain removal always rejects.
    The enclosing frontier validates exact lifetime increments, current retention
    and joint child-bundle removal. A new pristine request cannot also import or
    retire evidence in the same table transition. *)
val validate_transition : t -> incoming:t -> (unit, Agent_protocol.Error.t) result

val reserved_growth : t -> (Capacity_growth.t, Agent_protocol.Error.t) result
val required_transition_steps : t -> int

(** Complete raw wrappers with each current attempt's mandatory proof projection.
    Storage accounting only; includes future child depth at its real path. *)
val capacity_fields : t -> (string * Jsonaf.t) list

val requires_semantics : t -> bool
