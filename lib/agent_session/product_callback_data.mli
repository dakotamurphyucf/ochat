open! Core

(** Private factual owner allocated by the actual Product_attachment. It retains
    original Operation receipts, never execution authority or writer closures. *)
type t

val create : unit -> t

(** One actual create_moderator result, including genuine absence. Wrong domain
    or a second binding is an invariant violation. *)
val bind_manager : t -> manager:Chat_response.Moderator_manager.t option -> unit

(** Actual worker input and its original Actor-issued receipt. Validate domain
    and original id/generation/kind before enrollment. Positive parents compact
    only with registration closed, positive callback cleanup and raw terminal Ok.
    A handled producer failure alone does not keep successful cleanup history.
    The trusted actual worker callsite supplies Actor affinity; matching IDs and
    domain alone do not prove an arbitrary receipt came from that Actor. *)
val enroll
  :  t
  -> operation:Agent_protocol.Operation.t
  -> Callback_cleanup.Operation.t
  -> (unit, Agent_protocol.Error.t) Result.t

module Observation : sig
  type nonrec t = t

  module Operation_snapshot : sig
    type t

    val receipt : t -> Callback_cleanup.Operation.t
    val registration_closed : t -> bool
    val callbacks : t -> Callback_cleanup.Cleanup_snapshot.t option
    val terminal : t -> Callback_cleanup.Operation.terminal_outcome option
    val terminal_diagnostic : t -> Cleanup_diagnostic.t option

    (** Frozen lookup. SAME original Completion and write-once slot returns SAME
        carrier; unknown or Manager-filtered claims return None. *)
    val completion_diagnostic
      :  t
      -> completion:Callback_cleanup.Completion.t
      -> slot:Callback_cleanup.Completion_snapshot.Failure_slot.kind
      -> Cleanup_diagnostic.t option

    val diagnostics : t -> Cleanup_diagnostic.t list

    (** Domain-checked fresh view of THIS captured original Operation only. It
        cannot enroll a later Product operation or mutate the old snapshot. *)
    val refresh : t -> (t, Agent_protocol.Error.t) Result.t
  end

  module Snapshot : sig
    type t

    val operations : t -> Operation_snapshot.t list
  end

  (** Domain-checked live read, including after Product physical close. No Actor
      RPC or cleanup runs. Frozen facts/carriers are immutable. Only genuinely
      positive parents compact; missing/failed raw terminal remains retained. *)
  val snapshot : t -> (Snapshot.t, Agent_protocol.Error.t) Result.t
end
