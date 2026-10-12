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

(** Validate/encode before Exclusive writing, then reread before atomic CURRENT
    install. Existing same-sequence files may be reused only when exact full frame
    bytes match, validated and fsynced on one bounded descriptor plus parent sync.
    Mismatches remain untouched and do not change CURRENT. This supports unchanged
    checkpoints after reopen and retry after uncertain snapshot publication. *)
val install
  :  env:Eio_unix.Stdenv.base
  -> directory:string
  -> max_payload_length:int
  -> t
  -> (installed, Store_error.t) Result.t

(** Recover only an authenticated first checkpoint (transaction/event sequence
    one) while the caller retains the same journal/lifecycle/Handle exclusion.
    This supplies no authority to bootstrap state: the caller has already
    validated its genuine first Created transaction and original receipt.

    [reader] is rooted at that actual snapshot directory and borrows the caller's
    complete finite entry/byte allowance. Inventory, contents and comparisons
    consume that allowance/checkpoint before mutation. Byte debits conservatively
    cover both retained encoded frames, observed bytes, any prefix recheck and
    the ordinary install owner's one (missing target) or two (existing target)
    full read buffers. Exact compact frame measurement precedes frame allocation;
    all remaining buffer admission precedes mutation. This is a bounded buffer
    allowance, not exact heap or cumulative IO accounting. Only the first checkpoint
    file and optional matching CURRENT are accepted, along with abandoned
    CURRENT temporary names recognized by [Durable_file.temporary_target]. Those
    names consume the entry allowance and supply no checkpoint authority; this
    operation neither reads nor deletes their contents. An absent file or a shorter
    exact prefix of the requested encoded frame (including empty) can be completed;
    mismatching/extra/unsupported complete data remains untouched. Complete
    existing checkpoints must agree on the entire known value and retain their
    original outer unknown-field carrier. Both known JSON values are admitted
    with cooperative traversal before existing [Document_schema.Json.equal];
    that comparison is a bounded nonpreemptible stage bracketed by checkpoints.
    Ordinary [install] is unchanged.

    Recheck the observed exact prefix on the retained descriptor before extending,
    sync the file/directory, then use the same reread/CURRENT publication owner as
    [install]. Cancellation or uncertain writes may leave a later valid phase;
    retry re-admits actual evidence instead of creating another identity. Original
    checkpoint/unexpected exceptions propagate; no pruning or metadata writes. *)
val install_first_checkpoint
  :  t
  -> env:Eio_unix.Stdenv.base
  -> reader:Retention_reader.t
  -> max_payload_length:int
  -> checkpoint:(unit -> unit)
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

module Pruned : sig
  (** Evidence from one completed, serialized pruning operation. It must not be
      reused after another snapshot mutation. No persistent filesystem cache. *)
  type t

  val removed_count : t -> int
  val retention_floor : t -> int64
end

(** Validate every retained document and CURRENT, prune, and return the oldest
    remaining checkpoint from that same validated inventory. The caller holds
    snapshot/journal mutation serialization through use of the returned floor.
    No result is returned if unlinking or directory synchronization fails. *)
val prune_older_with_floor
  :  max_payload_length:int
  -> env:Eio_unix.Stdenv.base
  -> directory:string
  -> keep:int
  -> (Pruned.t, Store_error.t) Result.t

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
