(** Named store.session_archive v2. Pure bounded admission/conversion; no I/O,
    authorization or execution. Original owner guard precedes conversion. *)
open! Core

module R = Session_archive_record

type t

val limits : Document_schema.Limits.t
val value : t -> R.t
val of_document : Document_schema.Document.t -> (t, Document_schema.Error.t) Result.t
val authored : R.t -> (t, Document_schema.Error.t) Result.t
val to_document : t -> (Document_schema.Document.t, Document_schema.Error.t) Result.t

val stored_session_id
  :  Document_schema.Document.t
  -> (Agent_protocol.Id.Session.t, Document_schema.Error.t) Result.t

(** Exact admitted value must match the prepared basis. Only expired acknowledged
    receipt templates retire; all retained keyed nested unknown fields survive. *)
val prepare
  :  t
  -> R.Prepared.t
  -> now:Agent_protocol.Timestamp.t
  -> (t, Document_schema.Error.t) Result.t

(** Caller proves exact generic durable completion before acknowledging. *)
val acknowledge
  :  t
  -> key:Idempotency_store.Key.t
  -> request_digest:string
  -> (t, Document_schema.Error.t) Result.t
