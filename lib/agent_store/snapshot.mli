(** Atomic Frame v1 snapshots whose complete payload is a universal envelope.
    Unsupported/malformed complete documents fail closed without fallback. *)
open! Core

module Value : sig
  type t =
    { schema_version : int
    ; session_id : Agent_protocol.Id.Session.t
    ; transaction_sequence : int64
    ; transaction_hash : string option
    ; event_sequence : int64
    ; created_at : Agent_protocol.Timestamp.t
    ; prompt_artifact : string
    ; workspace_identity : string
    ; payload : Document_schema.Document.t
    }
end

module Stored : sig
  type metadata = private
    { session_id : string
    ; transaction_sequence : int64
    ; transaction_hash : string option
    ; event_sequence : int64
    ; prompt_artifact : string
    ; workspace_identity : string
    ; generation : int64
    ; session_revision : int64
    }

  type t

  val of_record : Document_record.t -> (t, Store_error.t) Result.t
  val metadata : t -> metadata
  val record : t -> Document_record.t
end

type provenance

type t = private
  { schema_version : int
  ; session_id : Agent_protocol.Id.Session.t
  ; transaction_sequence : int64
  ; transaction_hash : string option
  ; event_sequence : int64
  ; created_at : Agent_protocol.Timestamp.t
  ; prompt_artifact : string
  ; workspace_identity : string
  ; payload : Document_schema.Document.t
  ; provenance : provenance
  }

type installed =
  { filename : string
  ; snapshot : t
  }

type installed_stored =
  { filename : string
  ; stored : Stored.t
  }

val kind : string
val current_schema_version : int
val value : t -> Value.t
val carrier : t -> Value.t Document_schema.Extension_carrier.t
val stored : t -> Stored.t

val create
  :  limits:Document_schema.Limits.t
  -> session_id:Agent_protocol.Id.Session.t
  -> transaction_sequence:int64
  -> transaction_hash:string option
  -> event_sequence:int64
  -> created_at:Agent_protocol.Timestamp.t
  -> prompt_artifact:string
  -> workspace_identity:string
  -> payload:Document_schema.Document.t
  -> (t, Store_error.t) Result.t

(** Functional replacements preserve restored outer extensions. *)
val with_value
  :  t
  -> limits:Document_schema.Limits.t
  -> Value.t
  -> (t, Store_error.t) Result.t

val update
  :  t
  -> limits:Document_schema.Limits.t
  -> transaction_sequence:int64
  -> transaction_hash:string option
  -> event_sequence:int64
  -> created_at:Agent_protocol.Timestamp.t
  -> prompt_artifact:string
  -> workspace_identity:string
  -> payload:Document_schema.Document.t
  -> (t, Store_error.t) Result.t

(** Validate/encode before writing, then reread before atomic CURRENT install. *)
val install
  :  env:Eio_unix.Stdenv.base
  -> directory:string
  -> max_payload_length:int
  -> t
  -> (installed, Store_error.t) Result.t

val restore : Stored.t -> limits:Document_schema.Limits.t -> (t, Store_error.t) Result.t

val decode_stored_file
  :  max_payload_length:int
  -> string
  -> (Stored.t, Store_error.t) Result.t

val decode_file : max_payload_length:int -> string -> (t, Store_error.t) Result.t

val read_stored_file
  :  env:Eio_unix.Stdenv.base
  -> directory:string
  -> max_payload_length:int
  -> filename:string
  -> (installed_stored, Store_error.t) Result.t

val read_file
  :  env:Eio_unix.Stdenv.base
  -> directory:string
  -> max_payload_length:int
  -> filename:string
  -> (installed, Store_error.t) Result.t

(** Only missing/physically incomplete CURRENT candidates select fallback;
    complete unsupported or malformed documents never do. Does not rewrite CURRENT. *)
val load_current_stored
  :  env:Eio_unix.Stdenv.base
  -> directory:string
  -> max_payload_length:int
  -> (installed_stored option, Store_error.t) Result.t

val load_current
  :  env:Eio_unix.Stdenv.base
  -> directory:string
  -> max_payload_length:int
  -> (installed option, Store_error.t) Result.t

(** All complete retained checkpoints, before conversion. Complete errors stop
    the attempt. Recovery may skip physical incomplete candidates; retention may not. *)
val retained_stored
  :  env:Eio_unix.Stdenv.base
  -> directory:string
  -> max_payload_length:int
  -> allow_incomplete:bool
  -> (installed_stored list, Store_error.t) Result.t

(** Preflight every retained document and CURRENT before any deletion. Caller
    also validates journal anchors under its existing mutation serialization. *)
val prune_older
  :  max_payload_length:int
  -> env:Eio_unix.Stdenv.base
  -> directory:string
  -> keep:int
  -> (int, Store_error.t) Result.t

val retention_floor
  :  env:Eio_unix.Stdenv.base
  -> directory:string
  -> max_payload_length:int
  -> (int64, Store_error.t) Result.t
