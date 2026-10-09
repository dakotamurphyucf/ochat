open! Core

(** Projection/completion in the existing retained host idempotency owner. No
    separate receipt file or current-caller identity is created. *)
type t

val create
  :  store:Agent_store.Idempotency_store.t
  -> server_id:Agent_protocol.Id.Server.t
  -> t

val result
  :  t
  -> key:Agent_store.Idempotency_store.Key.t
  -> Agent_store.Session_archive_record.Outcome.t
  -> (Agent_protocol.Method_result.t, Agent_store.Store_error.t) Result.t

val complete
  :  t
  -> key:Agent_store.Idempotency_store.Key.t
  -> request_digest:string
  -> Agent_store.Session_archive_record.Outcome.t
  -> (unit, Agent_store.Store_error.t) Result.t

val complete_receipt
  :  t
  -> Agent_store.Session_archive_record.Receipt.t
  -> (unit, Agent_store.Store_error.t) Result.t

(** Called only after a checked absent lifecycle outcome under the session
    reservation, before any publication attempt. Completes the exact original
    protected Pending with the primary rejection in the existing host owner.
    The caller must retain that reservation through completion. *)
val reject
  :  t
  -> key:Agent_store.Idempotency_store.Key.t
  -> request_digest:string
  -> Agent_protocol.Error.t
  -> (unit, Agent_store.Store_error.t) Result.t
