open! Core
module P = Agent_protocol

type origin =
  | Returned
  | Interrupted
[@@deriving equal]

type settlement =
  { origin : origin
  ; delivery : Inference.Event.Terminal.delivery
  }
[@@deriving equal]

type phase =
  | Allocated
  | Running
  | Settled of settlement
[@@deriving equal]

(** Durable identity and evidence for one admitted root attempt. Scope and the
    host ordinal identify the actual allocation even if detailed accounting is
    untracked or retired. No runtime object, prepared request or grant is stored. *)
type t [@@deriving equal]

val allocate
  :  Inference_ledger.Handle.t
  -> tracking:Inference_ledger.tracking
  -> (t, P.Error.t) result

val ordinal : t -> int64
val generation : t -> int
val tracked : t -> bool
val phase : t -> phase
val with_phase : t -> phase -> t
val matches_handle : t -> Inference_ledger.Handle.t -> bool
val belongs_to : t -> P.Recovery_window.Scope.t -> bool
val matches_accounting : t -> Inference.Observation.Attempt_record.t -> bool
val check_transition : t -> incoming:t -> (unit, P.Error.t) result

(** At startup an acknowledged running attempt could have crossed submission.
    An allocation that never reached acknowledgement could not have run. *)
val recover : t -> t

val to_jsonaf : t -> Jsonaf.t
val of_jsonaf : Jsonaf.t -> (t, P.Error.t) result
val shape : Document_schema.Shape.t
