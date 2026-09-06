open! Core

(** Durable append-only server audit log with framed records, monotonic
    sequence numbers, a hash chain, and integrity-protected page cursors. *)

type t

val open_or_create
  :  env:Eio_unix.Stdenv.base
  -> directory:string
  -> max_payload_length:int
  -> (t, Store_error.t) result

(** [append] allocates the next durable sequence and flushes the record before
    returning it. *)
val append
  :  t
  -> timestamp:Agent_protocol.Timestamp.t
  -> level:Agent_protocol.Audit.level
  -> name:string
  -> session_id:Agent_protocol.Id.Session.t option
  -> principal_id:Agent_protocol.Id.Principal.t option
  -> payload:Jsonaf.t
  -> redacted:bool
  -> (Agent_protocol.Audit.t, Store_error.t) result

(** [read] verifies the signed cursor and returns records after its sequence,
    filtered in durable order. *)
val read
  :  t
  -> Agent_protocol.Audit.Read_request.t
  -> (Agent_protocol.Audit.t Agent_protocol.Page.t, Store_error.t) result
