(** Body-free durable evidence from one final prepared root request. *)
open! Core

module P = Agent_protocol
module Archive = Recovery_effective_archive

module Member : sig
  type t

  val citation : t -> P.Recovery_window.Citation.t
  val request_ordinal : t -> int
  val effective_ordinal : t -> int
  val canonical_ordinal : t -> int option
end

type custody =
  | Live
  | Lost
  | Archived of Archive.t

type t

val capture
  :  Inference_runtime.Recovery_input.t
  -> projection:Recovery_effective_projection.t
  -> scope:P.Recovery_window.Scope.t
  -> operation:P.Id.Operation.t
  -> source_revision:int64
  -> overlay_incarnation:int64
  -> (t, P.Error.t) result

val preparation_id : t -> string
val scope : t -> P.Recovery_window.Scope.t
val operation : t -> P.Id.Operation.t
val source_revision : t -> int64
val overlay_incarnation : t -> int64
val input_count : t -> int
val members : t -> Member.t list
val custody : t -> custody
val attempt : t -> Recovery_effective_attempt.t option
val update_attempt : t -> Recovery_effective_attempt.t -> (t, P.Error.t) result
val matches_configuration : t -> Inference.Observation.Configuration.t -> bool
val validate_attempt : t -> accounting:Inference_ledger.t -> bool
val same_storage_reservation : t -> t -> bool
val find_member : t -> P.Recovery_window.Citation.t -> Member.t option

(** A transfer is performed at most once. Existing physical evidence wins over
    anything inferred from a later archive. *)
val archive : t -> Archive.t -> (t, P.Error.t) result

val lose_live_source : t -> t
val equal_preparation : t -> t -> bool

(** Structural identity and exact source proof. No historical/preparation
    capability is recovered by decoding. *)
val validate_source : t -> Recovery_effective_projection.t -> (unit, P.Error.t) result

(** Verify the original source ordinal and version. The caller first establishes
    current visibility and a matching live or retained source owner. *)
val resolve
  :  t
  -> Recovery_effective_projection.t
  -> P.Recovery_window.Citation.t
  -> P.History.entry option

val resolve_archived
  :  t
  -> Recovery_effective_projection.t
  -> P.Recovery_window.Citation.t
  -> P.History.entry option

val to_preparation_jsonaf : t -> Jsonaf.t
val to_jsonaf : t -> Jsonaf.t
val of_jsonaf : Jsonaf.t -> (t, P.Error.t) result
val shape : Document_schema.Shape.t
