open! Core

(** Durable adapter between session transitions and the agent store journal. *)

type t

(** [create ~archive ...] requires [archive] to durably write the supplied
    previous state before returning success. A new reference is never journaled
    if that callback fails. Referenced archives must outlive journal pruning. *)
val create
  :  archive:
       (Session_state.Compaction_archive.t
        -> Session_state.t
        -> (unit, Agent_protocol.Error.t) result)
  -> command_accepted:(string -> int64 -> unit)
  -> writer:Agent_store.Commit_writer.t
  -> durability:Agent_store.Journal_segment.durability
  -> previous_transaction_hash:string option
  -> t

val commit
  :  t
  -> command_audit:string option
  -> previous:Session_state.t
  -> Session_transition.t
  -> (unit, Agent_protocol.Error.t) result

val actor_persistence : t -> Session_actor.persistence
val transaction_hash : t -> string option

val install_snapshot
  :  env:Eio_unix.Stdenv.base
  -> handle:Agent_store.Session_store.Handle.t
  -> max_payload_length:int
  -> transaction_hash:string option
  -> Session_state.t
  -> (Agent_store.Snapshot.installed, Agent_store.Store_error.t) result

val restore_snapshot : string -> (Session_state.t, Agent_store.Store_error.t) result

val apply_transaction
  :  Session_state.t
  -> Agent_store.Transaction.t
  -> (Session_state.t, Agent_store.Store_error.t) result

(** [durable_events transaction] decodes and validates the transaction's
    durable event payloads. *)
val durable_events
  :  Agent_store.Transaction.t
  -> (Agent_protocol.Event.Durable.t list, Agent_store.Store_error.t) result
