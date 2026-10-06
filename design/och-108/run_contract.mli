(** Design boundary only: not installed, compiled or an implementation ABI.
    Exact protocol method/native operation names belong to the shared host and
    CAP-R4 lifecycle integration. These types keep run/session/operation identity
    and authoritative terminal state separate without proposing an executor.
    Agent_protocol.Session.t, Session.Message_content.t, Id.Session.t,
    Id.Operation.t, Idempotency_key.t and Error.t are current repository types;
    modules below are proposed types, not aliases for existing services.
    Parsing/formatting does not establish an ABI. *)
open! Core

module Run_id : sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val of_string : string -> (t, Agent_protocol.Error.t) result
  val to_string : t -> string
end

module Mode : sig
  type t =
    | Single_turn
    | Workflow
  [@@deriving equal, sexp_of]
end

module Input : sig
  (** Explicit orchestration start submits no empty user message. Host validation
      admits it only in Workflow mode with authorized captured startup policy. *)
  type t =
    | User_submission of Agent_protocol.Session.Message_content.t
    | Orchestration_start
end

module Ownership : sig
  type t =
    | Process_bound
    | Detached_host
    | Owner_bound
  [@@deriving equal, sexp_of]

  (** Scope validation reuses Session.Spec host/liveness combinations. *)
  val validate
    :  t
    -> session:Agent_protocol.Session.t
    -> (unit, Agent_protocol.Error.t) result
end

module Terminal : sig
  type result_reference

  type t =
    | Completed of result_reference option
    | Failed of Agent_protocol.Error.t
    | Cancelled of { reason : string }
    | Limited of { reason : string }
    | Interrupted of
        { reason : string
        ; retryable_after_reconciliation : bool
        }
end

module Status : sig
  type waiting_reason

  type t =
    | Admitted
    | Running of Agent_protocol.Id.Operation.t
    | Waiting of waiting_reason
    | Terminal of Terminal.t
end

module Run : sig
  type t

  val id : t -> Run_id.t
  val session_id : t -> Agent_protocol.Id.Session.t
  val session_generation : t -> int
  val mode : t -> Mode.t
  val ownership : t -> Ownership.t
  val revision : t -> int64
  val status : t -> Status.t
end

module Host : sig
  type t
  type validated_start
  type validated_terminal_intent
  type receipt

  (** Validation captures expected source/generation/profile references and current
      authority; it does not execute a tool, resolve credentials or grant access. *)
  val validate_start
    :  t
    -> session_id:Agent_protocol.Id.Session.t
    -> expected_generation:int
    -> mode:Mode.t
    -> input:Input.t
    -> ownership:Ownership.t
    -> (validated_start, Agent_protocol.Error.t) result

  (** The actor allocates/commits run identity and request receipt before effects.
      Same-scope retry returns the original receipt; conflicting reuse rejects.
      Record/schema conversions use the shared universal document engine. *)
  val admit
    :  t
    -> validated_start
    -> request_id:Agent_protocol.Idempotency_key.t
    -> (Run.t * receipt, Agent_protocol.Error.t) result

  (** CAP-R4 authored policy requests continuation/wait/finish through existing
      scoped host operations. This commit neither stops the session nor runs a
      second scheduler. Revalidate expected revision/authority; settle or explicitly
      relinquish owned work before terminal commit. Commit precedes publication.
      Identical terminal retry is a no-op; conflicting terminal state rejects. *)
  val commit_terminal
    :  t
    -> run_id:Run_id.t
    -> expected_revision:int64
    -> validated_terminal_intent
    -> (Run.t * receipt, Agent_protocol.Error.t) result

  (** Read is authorization checked; possession of a Run_id grants nothing.
      Restore/reconnect rebuilds from local committed state before further effects. *)
  val read : t -> Run_id.t -> (Run.t, Agent_protocol.Error.t) result
end

module Client_outcome : sig
  (** A connection/timeout may lose the receipt without changing host run state.
      The CLI must report this honestly and query before resubmitting. *)
  type t =
    | Observed_terminal of Run.t
    | Detached of
        { run_id : Run_id.t
        ; last_sequence : int64 option
        }
    | Unconfirmed of
        { run_id : Run_id.t option
        ; request_id : Agent_protocol.Idempotency_key.t
        ; error : Agent_protocol.Error.t
        }
end
