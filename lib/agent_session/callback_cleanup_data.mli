open! Core

(** Factual observations of actual Actor-issued callback producers. These are
    neither execution authority nor cleanup executors. No observation uses a
    mailbox or searches the Actor's current operation. *)
module Failure : sig
  type t =
    | Protocol of Agent_protocol.Error.t
    | Raised of exn * Stdlib.Printexc.raw_backtrace
end

module Outcome : sig
  type t = (unit, Failure.t) Result.t

  val capture : (unit -> (unit, Agent_protocol.Error.t) Result.t) -> t
  val resume : t -> (unit, Agent_protocol.Error.t) Result.t
end

module Attempt : sig
  type t

  val create : unit -> t
  val equal : t -> t -> bool
end

module Completion_snapshot : sig
  (** Fixed diagnostic positions on the SAME Completion. A kind is factual
      provenance, not execution or cleanup authority. Different Completions or
      different kinds remain distinct even when failures share a payload. *)
  module Failure_slot : sig
    type kind =
      | Release
      | Lexical_handler
      | Reply_boundary
      | Capture
    [@@deriving equal]

    type t

    val kind : t -> kind
    val failure : t -> Failure.t
  end

  type t

  type producer_exit =
    | Not_returned
    | Returned of Outcome.t

  type finish_call =
    | Not_attempted
    | Awaiting_reply
    | Completed of Outcome.t

  type release =
    | Not_released
    | Finishing
    | Completed of Outcome.t

  (** Actual pre-finish producer-phase outcome: claim/setup/refresh/native-goal
      work included; user callback entry is not implied. It excludes the finish
      RPC and later legacy committed/Stop checks. Only this outside producer
      records exit; Stop or bookkeeping release cannot manufacture it. *)
  val producer_exit : t -> producer_exit

  val finish_call : t -> finish_call
  val release : t -> release

  (** Original full releasing-invocation failure first, then a distinct lexical
      handler failure, then an independent rejection/await failure, and finally
      [capture_failure] when present. The separate capture accessor exposes that
      same failure; do not append it again. A handler's own projected reply counts
      once using invocation identity, never exception identity. Producer primary
      is available separately through [producer_exit]. *)
  val failures : t -> Failure.t list

  (** SAME ordered failure fold with its original positions. [failures t] is
      exactly [List.map (failure_slots t) ~f:Failure_slot.failure]. Entries and
      old snapshots are immutable; an absent slot is not proof of successful
      cleanup. The releasing invocation's lexical projection is omitted only
      when both belong to the SAME actual invocation. *)
  val failure_slots : t -> Failure_slot.t list

  (** Independent exception from the one outside installed-claim capture. This
      is not the producer primary, even when the same exception object occurs. *)
  val capture_failure : t -> Failure.t option

  (** This immutable observation already contains the actual original lexical
      handler result, or a genuine pre-handler rejection. An await failure without
      its handler result is incomplete even if a later observation settles. This
      fact alone proves neither producer exit nor bookkeeping release. *)
  val original_finish_settled : t -> bool
end

module Completion : sig
  type t

  (** Checks creating domain before reading mutable data. Immutable snapshot;
      available after expiry, replacement or Actor death. *)
  val snapshot : t -> (Completion_snapshot.t, Agent_protocol.Error.t) Result.t

  val create : issuer:unit ref -> domain:int -> t
  val belongs_to : t -> issuer:unit ref -> domain:int -> bool
  val producer_exit : t -> Completion_snapshot.producer_exit
  val latest_finish : t -> Outcome.t option
  val record_capture_failure : t -> Failure.t -> unit
  val record_producer_exit : t -> Outcome.t -> unit
  val record_release : t -> attempt:Attempt.t -> unit
  val release : t -> Completion_snapshot.release
  val record_finish : t -> attempt:Attempt.t -> Outcome.t -> unit

  (** Exactly one actual lexical finish invocation. Its token is observation
      provenance for the existing RPC, not scheduling or ownership. *)
  val begin_finish_call : t -> Attempt.t

  module Reply_origin : sig
    type t =
      | Handler
      | Rejected
      | Await_failed
  end

  val reply_origin : t -> attempt:Attempt.t -> Reply_origin.t
  val record_reply : t -> attempt:Attempt.t -> origin:Reply_origin.t -> Outcome.t -> unit
end

module Completion_observation : sig
  type t

  (** SAME original Completion and its immutable observation. This is factual
      identity, not a receipt issuer or execution authority. *)
  val completion : t -> Completion.t

  val snapshot : t -> Completion_snapshot.t

  (** Creating-domain checked observation of the SAME actual Completion. No
      mailbox, execution or new receipt is involved. *)
  val observe : Completion.t -> (t, Agent_protocol.Error.t) Result.t
end

module Cleanup_snapshot : sig
  type t

  val registration_closed : t -> bool
  val pending : t -> bool
  val failures : t -> Failure.t list

  (** Retained original pending/cleanup-negative facts. Any observed producer
      exit, including a handled primary failure, may compact once finish/release
      and independent cleanup facts are positive. Individual Completion handles
      retain their original primary. This is not successful callback history. *)
  val completions : t -> Completion_snapshot.t list

  (** Identity-preserving view of the same retained children. Duplicate ownership
      views can coalesce one original invocation without merging distinct claims
      which happen to raise the same exception. *)
  val claims : t -> Completion_observation.t list
end

module Initialization : sig
  type t

  val snapshot : t -> (Cleanup_snapshot.t, Agent_protocol.Error.t) Result.t
end

module Operation : sig
  type t

  type terminal_outcome =
    ( (unit, Agent_protocol.Error.t) Result.t
      , exn * Stdlib.Printexc.raw_backtrace )
      Result.t

  val operation : t -> (Agent_protocol.Operation.t, Agent_protocol.Error.t) Result.t

  (** Pending also includes absence of this SAME actual terminal-handler result;
      a terminal failure remains separate in [terminal], not callback failures. *)
  val snapshot : t -> (Cleanup_snapshot.t, Agent_protocol.Error.t) Result.t

  (** Filter retained children by their actual ACK-installed Manager association.
      Registration and raw-terminal pending remain whole original-parent facts. *)
  val snapshot_for_manager
    :  t
    -> manager:Chat_response.Moderator_manager.t
    -> (Cleanup_snapshot.t, Agent_protocol.Error.t) Result.t

  (** Raw actual serialized terminal handler result, separate from normalized
      worker outcome and callback cleanup. Creating-domain checked. *)
  val terminal : t -> (terminal_outcome option, Agent_protocol.Error.t) Result.t
end

module Host : sig
  val initialization : issuer:unit ref -> domain:int -> scope:unit ref -> Initialization.t

  val operation
    :  issuer:unit ref
    -> domain:int
    -> operation:Agent_protocol.Operation.t
    -> Operation.t

  val check_initialization_registration
    :  Initialization.t
    -> issuer:unit ref
    -> scope:unit ref
    -> (unit, Agent_protocol.Error.t) Result.t

  val check_operation_registration
    :  Operation.t
    -> issuer:unit ref
    -> operation:Agent_protocol.Operation.t
    -> (unit, Agent_protocol.Error.t) Result.t

  (** Invoke only at the validated committed ACK installation, before publish.
      Same original issuer/parent/domain and actual Manager association; external
      callbacks never run here. These mutate factual DATA only. *)
  val install_initialization_callback
    :  Initialization.t
    -> issuer:unit ref
    -> scope:unit ref
    -> manager:Chat_response.Moderator_manager.t
    -> Completion.t
    -> unit

  val install_operation_callback
    :  Operation.t
    -> issuer:unit ref
    -> operation:Agent_protocol.Operation.t
    -> manager:Chat_response.Moderator_manager.t
    -> Completion.t
    -> unit

  val end_initialization_registration : Initialization.t -> unit
  val end_operation_registration : Operation.t -> unit
  val record_operation_terminal : Operation.t -> Operation.terminal_outcome -> unit
end
