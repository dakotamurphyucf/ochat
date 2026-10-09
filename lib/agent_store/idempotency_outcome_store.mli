(** Filesystem adapter for immutable terminal outcomes. The caller retains the
    index-directory ownership lock. This module grants no receipt authority. *)
val publish
  :  Idempotency_outcome.t
  -> directory:_ Eio.Path.t
  -> (Idempotency_outcome.Reference.t, Store_error.t) result

val load
  :  Idempotency_outcome.Reference.t
  -> directory:_ Eio.Path.t
  -> (Idempotency_outcome.t, Store_error.t) result

(** Charges one no-follow read to the existing collector's shared budget. *)
val read_retained
  :  Idempotency_outcome.Reference.t
  -> reader:Retention_reader.t
  -> (Idempotency_outcome.t, Store_error.t) result

val basename : Idempotency_outcome.Reference.t -> string
