open! Core

(** Factual progress of ONE existing canonical persistence invocation. No
    admission, callback permission, journal retry or decoded proof is issued.
    All operations belong to the creating Actor domain; foreign-domain mutation
    returns Conflict and foreign-domain factual reads are invariant violations. *)
type t

module Owner : sig
  type observation = t
  type t

  (** Retain SAME physical previous State and complete candidate before commit. *)
  val create : previous:Session_state.t -> candidate:Session_transition.t -> t

  (** Invoke the actual persistence callback once, preserving its original Error
      or exception/backtrace. Unobserved failure is unknown. Unobserved success
      preserves the existing trusted host contract, without inventing a journal
      hash/sequence. An observed host cannot downgrade to that legacy success. *)
  val commit
    :  t
    -> commit:(observation:observation -> (unit, Agent_protocol.Error.t) result)
    -> (unit, Agent_protocol.Error.t) result
end

module Transaction : sig
  (** Neutral DATA from SAME validated native Transaction and fixed writer
      profile. Payload is its exact original bytes, including unknown fields. *)
  type t =
    { session_id : Agent_protocol.Id.Session.t
    ; generation : int
    ; sequence : int64
    ; revision : int64
    ; previous_hash : string option
    ; payload : string
    ; hash : string
    ; max_payload_length : int
    }
end

module Returned : sig
  (** Actual native commit reply projection. DATA alone grants no ACK authority. *)
  type t =
    { sequence : int64
    ; hash : string
    }
end

module Acknowledgement : sig
  type t

  val transaction : t -> Transaction.t
  val returned : t -> Returned.t
end

module Host : sig
  type observation = t
  type t

  (** First canonical persistence operation, before validation/preflight/archive
      effects. Validates SAME physical previous/candidate and creating domain.
      Entry is a no-journal-attempt fact, not rollback of earlier host effects. *)
  val observe_entry
    :  observation
    -> previous:Session_state.t
    -> candidate:Session_transition.t
    -> (t, Agent_protocol.Error.t) result

  (** Wrap ONLY the same canonical Commit_writer.commit. Validate neutral
      candidate metadata, actual payload bound/digest; record entered before
      invocation, matching native ACK before returning to preservation work.
      No public phase setter or standalone ACK constructor. A trusted host must
      supply its actual writer, just as its existing commit-success contract.
      Error/cancellation after entry never proves absence and is not retried. *)
  val with_writer
    :  t
    -> transaction:Transaction.t
    -> commit:(unit -> (Returned.t, Agent_protocol.Error.t) result)
    -> (Returned.t, Agent_protocol.Error.t) result
end

module Progress : sig
  type t =
    | Unobserved
    | Before_writer
    | Writer_entered of Transaction.t
    | Journal_acknowledged of Acknowledgement.t
end

module Outcome : sig
  type t =
    | Not_called
    | In_progress
    | Returned of (unit, Agent_protocol.Error.t) result
    | Raised of exn * Stdlib.Printexc.raw_backtrace
end

(** Nonyielding reads of actual retained facts. Journal ACK and a later host
    Error/exception coexist. No callback retry or State install is implied. *)
val progress : Owner.t -> Progress.t

val outcome : Owner.t -> Outcome.t
