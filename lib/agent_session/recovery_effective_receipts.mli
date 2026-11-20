open! Core
module P = Agent_protocol
module Attempt = Recovery_effective_attempt

(** One host-authorized change associated with the immutable preparation
    revision. Its physical adapter identity stays with Configuration_capture. *)
type t

(** Plan a new receipt from the real actor admission and previously admitted
    prepared configuration. Capacity can omit optional recovery evidence. *)
val allocate
  :  Session_state.t
  -> source_revision:int64
  -> Inference_ledger.Handle.t
  -> tracking:Inference_ledger.tracking
  -> Inference.Observation.Configuration.t
  -> t option

(** Match the exact ordinal and source of the already bound host handle. These
    functions produce no durable marker for ordinary or unbound inference. *)
val acknowledge : Session_state.t -> Inference_ledger.Handle.t -> t option

val complete
  :  Session_state.t
  -> Inference_ledger.Handle.t
  -> Inference_client.Completion.t
  -> t option

val release
  :  Session_state.t
  -> Inference_ledger.Handle.t
  -> delivery:Inference.Event.Terminal.delivery option
  -> t option

val recover : Session_state.t -> t list

(** Apply the receipt after the corresponding accounting change. *)
val apply : Session_state.t -> t -> (Recovery_effective_ledger.t, P.Error.t) result

(** A new binding must advance the exact transaction's accounting high-water
    mark, including when its detailed row is untracked. *)
val check_transaction
  :  previous:Session_state.t
  -> next:Session_state.t
  -> (unit, P.Error.t) result

val to_jsonaf : t -> Jsonaf.t
val of_jsonaf : Jsonaf.t -> (t, P.Error.t) result
val shape : Document_schema.Shape.t
