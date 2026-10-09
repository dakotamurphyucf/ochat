(** Bounded orphan retirement, called only under the receipt owner mutex after
    validating both disk and memory metadata plus all referenced outcome files.
    No expiration policy or receipt deletion authority is introduced. *)
val collect
  :  directory:_ Eio.Path.t
  -> reader:Retention_reader.t
  -> retained:Idempotency_outcome.Reference.t list
  -> (int, Store_error.t) result
