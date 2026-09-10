open! Core

(** Periodic Eio maintenance for daemon-global durable stores. *)

type stats =
  { expired_idempotency_records : int
  ; expired_temporary_blobs : int
  ; expired_response_artifacts : int
  ; discarded_job_results : int
  ; retired_job_preparations : int
  ; deferred_result_collections : int
  }
[@@deriving sexp]

type t

type status =
  { running : bool
  ; last_stats : stats option
  ; last_success_at : Agent_protocol.Timestamp.t option
  ; last_error : Agent_store.Store_error.t option
  }
[@@deriving sexp]

val run_once
  :  env:Eio_unix.Stdenv.base
  -> idempotency_store:Agent_store.Idempotency_store.t
  -> blob_store:Agent_store.Blob_store.t
  -> session_store:Agent_store.Session_store.t
  -> registry:Session_registry.t option
  -> protected_response_sessions:Agent_protocol.Id.Session.t list
  -> response_retention:Time_ns.Span.t
  -> now:Agent_protocol.Timestamp.t
  -> (stats, Agent_store.Store_error.t) Result.t

val start
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> clock:_ Eio.Time.clock
  -> every:float
  -> idempotency_store:Agent_store.Idempotency_store.t
  -> blob_store:Agent_store.Blob_store.t
  -> response_retention:Time_ns.Span.t
  -> registry:Session_registry.t
  -> session_store:Agent_store.Session_store.t
  -> on_error:(Agent_store.Store_error.t -> unit)
  -> t

val close : t -> unit

(** [status t] returns the latest maintenance-cycle outcome without exposing
    native paths through protocol projections. *)
val status : t -> status
