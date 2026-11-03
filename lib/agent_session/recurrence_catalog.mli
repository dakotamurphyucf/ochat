(** One bounded subcarrier of the sole session document. At most 16 retained
    recurrence IDs including retired tombstones, eight immutable revisions per ID
    and 256 immutable command receipts. Complete preserved storage is at most 32 MiB
    including known safe-control growth reserves. Saturation rejects new commands;
    no ID, source revision, receipt or execution evidence is evicted. *)
type t [@@deriving sexp]

val empty : t
val revision : t -> int64
val entries : t -> Recurrence_entry.t list
val find : t -> Agent_protocol.Id.Recurrence.t -> Recurrence_entry.t option
val receipts : t -> Recurrence_command_receipt.t list

val receipt
  :  t
  -> principal_id:Agent_protocol.Id.Principal.t
  -> key:Agent_protocol.Idempotency_key.t
  -> request_sha256:string
  -> (Recurrence_command_receipt.t option, Agent_protocol.Error.t) result

(** Pure checked metadata candidate, never authentication. New entries must have
    paused revision-zero provenance and matching new creation receipt. Existing
    child documents retain their own complete universal custody as opaque values. *)
val commit
  :  t
  -> entry:Recurrence_entry.t
  -> receipt:Recurrence_command_receipt.t
  -> (t, Agent_protocol.Error.t) result

(** Pure exact-basis one-entry due candidate. Advances catalog/planning state
    together without consuming a keyed user command receipt or control revision.
    The publisher must check actual lifecycle/source/control and preflight the
    complete shared state/journal before saving or installing its local phase. *)
val apply_due
  :  t
  -> recurrence_id:Agent_protocol.Id.Recurrence.t
  -> Recurrence_due_plan.t
  -> (t, Agent_protocol.Error.t) result

(** Internal safe dispositions consume no user receipt slot. They preserve all
    definitions and the sole source cursor, withdraw known unadmitted rows and
    retire owned alarms while advancing changed control/catalog frontiers once.
    They fence future launches only, never establish external cancellation. *)
val pause_for_stop : t -> (t option, Agent_protocol.Error.t) result

val hold_for_source : t -> (t option, Agent_protocol.Error.t) result

val retire_stale_births
  :  t
  -> current_revision:Agent_protocol.Session_lifecycle.Revision.t
  -> archived:bool
  -> (t option, Agent_protocol.Error.t) result

val validate
  :  t
  -> session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> (unit, Agent_protocol.Error.t) result

val validate_transition : t -> incoming:t -> (unit, Agent_protocol.Error.t) result
val to_jsonaf : t -> (Jsonaf.t, Agent_protocol.Error.t) result
val of_jsonaf : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t
val limits : Document_schema.Limits.t

module Reservation : sig
  type t

  val growth : t -> Capacity_growth.t
  val required_transition_steps : t -> int
end

(** Known durable control growth only. Sum these independent obligations at the
    shared actual state/document/journal admission seam; never independently
    spend the same host headroom promised to other owners. Exact journal and
    persistence preflight remains required before effects. *)
val reservation : t -> (Reservation.t, Agent_protocol.Error.t) result

val validate_encoded_capacity : t -> Jsonaf.t -> (unit, Agent_protocol.Error.t) result

(** Private capacity-only complete control projection. Never decode/install it;
    it conveys no lifecycle authority or execution evidence. The enclosing
    universal carrier preserves its own compatible wrapper/envelope fields. *)
val capacity_projection : t -> (Jsonaf.t, Agent_protocol.Error.t) result
