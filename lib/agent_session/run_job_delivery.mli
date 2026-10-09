(** Immutable actual terminal job occurrence required by an exact authored Wait.
    Retained before retry replaces the latest job. This is private durable host
    data; it grants neither callback nor public result/disclosure authority. *)
module Retirement : sig
  type t =
    | Source_change
    | Run_terminal
    | Recovery
    | Authorization_lost
  [@@deriving equal, sexp]
end

module Disposition : sig
  type t =
    | Pending
    | Enqueued of Agent_protocol.Timestamp.t
    | Claimed of
        { enqueued_at : Agent_protocol.Timestamp.t
        ; execution_id : Agent_protocol.Id.Moderator_execution.t
        }
    | Retired of
        { enqueued_at : Agent_protocol.Timestamp.t option
        ; execution_id : Agent_protocol.Id.Moderator_execution.t option
        ; reason : Retirement.t
        }
  [@@deriving equal, sexp]
end

module Key : sig
  type t = private
    { run_id : Agent_protocol.Id.Run.t
    ; job_id : Agent_protocol.Id.Job.t
    ; generation : int
    ; attempt : int
    }
  [@@deriving compare, equal, sexp_of]

  include Core.Comparator.S with type t := t

  val of_wake : Agent_protocol.Run_wake.t -> t option
end

type t [@@deriving equal, sexp]

(** Bytes in the complete encoded carrier, including the escaped frame string.
    The admission reservation also covers same-transaction terminal evidence and
    next-attempt custody. Production captures the existing durable artifact
    descriptor; a bounded inline producer must prove this ceiling before Wait. *)
val max_encoded_bytes : int

val reservation_bytes : int

(** Remaining bytes reserved inside the per-carrier ceiling for enqueue, claim
    and retirement. This reserve also applies to decoding, not only capture. *)
val disposition_reserve_bytes : t -> int

val key : t -> Key.t
val run_id : t -> Agent_protocol.Id.Run.t
val source : t -> Agent_protocol.Run_source.t
val frame : t -> Chat_response.Background_delivery.t
val disposition : t -> Disposition.t

(** Capture only an exact run-owned Waiting job attempt from an actual terminal
    frame. The actor separately proves actual completion/source/creator before
    committing this with immutable terminal evidence and any retry successor.
    Bounds the entire encoded frame under Run_limits, before mutation. *)
val capture
  :  Agent_protocol.Run.t
  -> frame:Chat_response.Background_delivery.t
  -> (t, Agent_protocol.Error.t) result

(** Only Pending may enqueue. The actor rechecks current installation/principal
    authority and saves the queue checkpoint with this transition atomically. *)
val enqueue : t -> at:Agent_protocol.Timestamp.t -> (t, Agent_protocol.Error.t) result

(** Only Enqueued may claim. The actor proves exact retained frame, expiry and
    absence of a prior claim, then saves the same actual execution receipt. *)
val claim
  :  t
  -> execution_id:Agent_protocol.Id.Moderator_execution.t
  -> (t, Agent_protocol.Error.t) result

(** Preserves prior enqueue/claim provenance; idempotent for an already-retired
    occurrence. Recovery never reenqueues from queue absence or replays a claim. *)
val retire : t -> reason:Retirement.t -> t

val validate_transition : previous:t -> t -> (unit, Agent_protocol.Error.t) result
val to_jsonaf : t -> Jsonaf.t
val of_jsonaf : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t

(** Index linkage validation with already-validated exact owner facts. Pending or
    Enqueued requires the current installation and its unresolved exact Wait;
    Claimed retains a live run, Retired retains a terminal run. The actual terminal
    proof must agree with this immutable frame; this does not confer authority. *)
val validate_owner
  :  t
  -> run:Agent_protocol.Run.t
  -> proof:Agent_protocol.Run_work.Terminal.t option
  -> pending_wait:Agent_protocol.Run_wake.t option
  -> current_source:bool
  -> (unit, Agent_protocol.Error.t) result
