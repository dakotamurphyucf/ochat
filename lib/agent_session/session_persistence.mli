open! Core

(** Durable adapter. One owner holds both state and outer snapshot preservation
    context through recovery, commit and checkpoint installation. Complete admitted
    documents become the next preservation basis only after the journal
    acknowledgement or snapshot installation succeeds. *)
module Restored : sig
  type t

  val authored : Session_state.t -> t
  val state : t -> Session_state.t
  val state_document : t -> Session_state_document.t
  val with_state : t -> Session_state.t -> t
end

type t

(** [retention_preflight], when supplied, must belong to the same journal/session
    and fixed policy as [writer] for this owner's lifetime. Creation/reopen must
    supply a fresh scope; staging and direct callers may pass [None]. *)
val create
  :  archive:
       (Session_state.Compaction_archive.t
        -> Document_schema.Document.t
        -> (unit, Agent_protocol.Error.t) result)
  -> command_accepted:(Document_schema.Document.t -> int64 -> unit)
  -> writer:Agent_store.Commit_writer.t
  -> durability:Agent_store.Journal_segment.durability
  -> limits:Document_schema.Limits.t
  -> archive_limits:Document_schema.Limits.t
  -> retention_preflight:Agent_store.Recovery.Retention_preflight.t option
  -> restored:Restored.t
  -> previous_transaction_hash:string option
  -> t

val commit
  :  t
  -> command_audit:Document_schema.Document.t option
  -> previous:Session_state.t
  -> Session_transition.t
  -> (unit, Agent_protocol.Error.t) result

val actor_persistence : t -> Session_actor.persistence
val transaction_hash : t -> string option
val restored : t -> Restored.t

(** Borrow this owner's fixed-policy retention preflight scope. The scope remains
    owned by [t]; checks must run serially within the same journal/handle lifetime.
    [None] retains the factory's complete preflight fallback. *)
val retention_preflight : t -> Agent_store.Recovery.Retention_preflight.t option

val install_snapshot_at
  :  t
  -> env:Eio_unix.Stdenv.base
  -> directory:string
  -> max_payload_length:int
  -> transaction_hash:string option
  -> Session_state.t
  -> (Agent_store.Snapshot.installed, Agent_store.Store_error.t) result

val install_snapshot
  :  t
  -> env:Eio_unix.Stdenv.base
  -> handle:Agent_store.Session_store.Handle.t
  -> max_payload_length:int
  -> transaction_hash:string option
  -> Session_state.t
  -> (Agent_store.Snapshot.installed, Agent_store.Store_error.t) result

val restore_snapshot
  :  limits:Document_schema.Limits.t
  -> Agent_store.Snapshot.t
  -> (Restored.t, Agent_store.Store_error.t) result

(** Pure document replay preserving the complete state carrier. Performs the
    same transaction child/owner checks, original state admission and checked
    metadata stamping as [apply_transaction], without an outer Snapshot owner. *)
val apply_document
  :  Session_state_document.t
  -> limits:Document_schema.Limits.t
  -> Agent_store.Transaction.t
  -> (Session_state_document.t, Agent_store.Store_error.t) result

val apply_transaction
  :  limits:Document_schema.Limits.t
  -> Restored.t
  -> Agent_store.Transaction.t
  -> (Restored.t, Agent_store.Store_error.t) result

val durable_events
  :  limits:Document_schema.Limits.t
  -> Agent_store.Transaction.t
  -> (Agent_protocol.Event.Durable.t list, Agent_store.Store_error.t) result

val validate_transaction
  :  limits:Document_schema.Limits.t
  -> Agent_store.Transaction.t
  -> (unit, Agent_store.Store_error.t) result

val validate : Restored.t -> (unit, Agent_store.Store_error.t) result

(** Move validated captured event documents to the bounded replay owner.
    Restore is used before publication; take empties this staging queue. *)
val restore_replay_documents : t -> Durable_event_document.t list -> unit

val take_replay_documents : t -> Durable_event_document.t list

val event_documents
  :  limits:Document_schema.Limits.t
  -> Agent_store.Transaction.t
  -> (Durable_event_document.t list, Agent_store.Store_error.t) result
