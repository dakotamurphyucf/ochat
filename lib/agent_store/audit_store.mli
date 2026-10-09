open! Core

(** Durable append-only server audit log with named current event/evidence
    documents inside verified frames, monotonic sequences, a hash chain and
    integrity-protected page cursors. Beta binary audit payloads are unsupported.
    Startup admits every complete semantic record before repairing a short tail.
    The frame byte limit covers the complete escaped evidence envelope; aggregate
    segment loading retains the existing journal scanner contract. *)

type t

val open_or_create
  :  env:Eio_unix.Stdenv.base
  -> directory:string
  -> max_payload_length:int
  -> (t, Store_error.t) result

(** [append] validates the complete named event/evidence frame before effects,
    allocates a positive durable sequence and flushes before returning. Hashes
    cover exact embedded event bytes. An uncertain publication reconciles the
    complete canonical journal; if that cannot be proven valid, both append and
    read return the stored unavailable failure. Original publication errors and
    exception backtraces survive secondary recovery failure. Cancellation and
    unexpected exceptions propagate outside the unpoisoned owner mutex. *)
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

(** [read] requires a verified current snapshot, verifies the signed cursor
    and returns current protocol projections filtered in durable order. Unknown
    private document members are retained on disk, never disclosed by this page. *)
val read
  :  t
  -> Agent_protocol.Audit.Read_request.t
  -> (Agent_protocol.Audit.t Agent_protocol.Page.t, Store_error.t) result
