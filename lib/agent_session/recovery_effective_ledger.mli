(** Bounded, immutable local preparation records and their current overlay
    incarnation. The ledger cannot attest to provider submission. *)
open! Core

module P = Agent_protocol
module Record = Recovery_effective_record

type t

val records : t -> Record.t list
val current_incarnation : t -> int64 option
val rotate_incarnation : t -> t

(** Record-count precheck before deriving a request witness. *)
val has_record_capacity : t option -> bool

(** On the live path, an evidence-only capacity refusal preserves the existing
    ledger and ordinary model operation. Other admission failures are errors. *)
val add_if_room : t option -> Record.t -> (t option, P.Error.t) result

(** Replay an actually committed addition. Exceeding any bound is invalid. *)
val add : t option -> Record.t -> (t, P.Error.t) result

val has_live_sources : t -> bool

(** Persist one source transfer while preserving all pre-existing facts. *)
val relocate
  :  t
  -> transfer:(Record.t -> (Record.t, P.Error.t) result)
  -> (t, P.Error.t) result

val validate_state
  :  t
  -> windows:Recovery_window_ledger.t option
  -> publication_revision:int64
  -> has_archive:(Record.Archive.t -> bool)
  -> (unit, P.Error.t) result

val to_jsonaf : t -> Jsonaf.t
val of_jsonaf : Jsonaf.t -> (t, P.Error.t) result
val validate_encoded : Jsonaf.t -> (unit, Document_schema.Error.t) result
val shape : Document_schema.Shape.t
