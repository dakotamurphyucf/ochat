(** Complete named-field session transactions. Frame version is independent.
    Restored records retain their exact original payload and immutable digest. *)
open! Core

module Value : sig
  type t =
    { schema_version : int
    ; session_id : Agent_protocol.Id.Session.t
    ; generation : int
    ; transaction_sequence : int64
    ; previous_transaction_hash : string option
    ; session_revision : int64
    ; first_event_sequence : int64 option
    ; last_event_sequence : int64 option
    ; accepted_at_ns : int64
    ; command_audit : (Document_schema.Document.t[@sexp.opaque]) option
    ; delta : (Document_schema.Document.t[@sexp.opaque])
    ; durable_events : (Document_schema.Document.t[@sexp.opaque]) list
    }
  [@@deriving sexp_of]
end

module Stored : sig
  type metadata = private
    { session_id : string
    ; generation : int64
    ; transaction_sequence : int64
    ; previous_transaction_hash : string option
    ; session_revision : int64
    ; first_event_sequence : int64 option
    ; last_event_sequence : int64 option
    ; accepted_at_ns : int64
    ; durable_event_count : int
    }

  type t

  (** Generic stored-version integrity projection; constructs no current IDs or
      embedded domain values. Unsupported metadata versions fail closed. *)
  val of_record : Document_record.t -> (t, Store_error.t) Result.t

  val metadata : t -> metadata
  val record : t -> Document_record.t
  val digest : t -> string
end

type provenance

type t = private
  { schema_version : int
  ; session_id : Agent_protocol.Id.Session.t
  ; generation : int
  ; transaction_sequence : int64
  ; previous_transaction_hash : string option
  ; session_revision : int64
  ; first_event_sequence : int64 option
  ; last_event_sequence : int64 option
  ; accepted_at_ns : int64
  ; command_audit : (Document_schema.Document.t[@sexp.opaque]) option
  ; delta : (Document_schema.Document.t[@sexp.opaque])
  ; durable_events : (Document_schema.Document.t[@sexp.opaque]) list
  ; provenance : (provenance[@sexp.opaque])
  }
[@@deriving sexp_of]

val kind : string
val current_schema_version : int
val value : t -> Value.t
val carrier : t -> Value.t Document_schema.Extension_carrier.t
val stored : t -> Stored.t
val validate : t -> (unit, Store_error.t) Result.t

(** New authored records validate, encode once, and establish their exact digest.
    Embedded document contracts are independently validated by their owners. *)
val create
  :  limits:Document_schema.Limits.t
  -> session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> transaction_sequence:int64
  -> previous_transaction_hash:string option
  -> session_revision:int64
  -> first_event_sequence:int64 option
  -> last_event_sequence:int64 option
  -> accepted_at_ns:int64
  -> command_audit:Document_schema.Document.t option
  -> delta:Document_schema.Document.t
  -> durable_events:Document_schema.Document.t list
  -> (t, Store_error.t) Result.t

(** Deliberate rewrite retains extensions, validates replacements, and creates
    a new record/digest. It never changes the original record. *)
val with_value
  :  t
  -> limits:Document_schema.Limits.t
  -> Value.t
  -> (t, Store_error.t) Result.t

val restore : Stored.t -> limits:Document_schema.Limits.t -> (t, Store_error.t) Result.t

val decode_record
  :  Document_record.t
  -> limits:Document_schema.Limits.t
  -> (t, Store_error.t) Result.t

(** Payload-only convenience reader under the shared durable structural profile
    and the existing 16 MiB byte budget. Files/journals must verify their frame
    before calling; configured recovery uses Stored projections and
    [decode_record] with its owner's explicit byte budget. *)
val decode : string -> (t, Store_error.t) Result.t

(** Exact original stored payload; restored values are never re-encoded here. *)
val encode : t -> string

(** SHA-256 of exact original payload, unchanged by logical conversions. *)
val hash : t -> string
