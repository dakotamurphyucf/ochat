open! Core

(** Bounded original callback facts for actual Owner lifetime receipts. This
    private module runs no cleanup, selects no Actor authority and performs no IO.
    All values remain in their creating domain. Read refusals retain one stable
    diagnostic; a later positive read of the SAME receipt can prove custody
    independently without erasing that diagnostic. *)
module Product_selection : sig
  module P = Agent_session.Runtime_builder.Product_callbacks

  type operation

  type t =
    | Selected of operation list
    | Read_failed of Agent_session.Cleanup_diagnostic.t

  (** Called once after the real borrower/factual gate, before physical Close.
      Subsequent refreshes cannot enroll a later Product Operation. *)
  val capture : P.t -> t

  val refresh
    :  operation
    -> (P.Operation_snapshot.t, Agent_session.Cleanup_diagnostic.t) result

  (** Last successful immutable view of this SAME captured receipt. *)
  val latest : operation -> P.Operation_snapshot.t

  val read_failure : operation -> Agent_session.Cleanup_diagnostic.t option

  (** Refresh only selected receipts. Custody requires registration closed,
      actual raw terminal present and actual callback producer/finish/release
      settlement. Diagnostic negativity alone does not imply live custody. *)
  val custody_joined : t -> bool

  (** Original lower carriers plus bounded read refusal. No delivery is consumed;
      no new carrier is minted for an already known original failure. *)
  val diagnostics : t -> Agent_session.Cleanup_diagnostic.t list
end

module Lease_callback : sig
  type t

  (** The trusted Owner caller supplies this invocation's initial same-domain
      observation after checking original-open phase. The actual Completion is
      derived from that observation, so a mismatched pair is not representable. *)
  val create : Agent_session.Callback_cleanup.Completion_observation.t -> t

  val completion : t -> Agent_session.Callback_cleanup.Completion.t

  val observe
    :  t
    -> ( Agent_session.Callback_cleanup.Completion_observation.t
         , Agent_session.Cleanup_diagnostic.t )
         result

  (** One write-once carrier per SAME Completion + typed failure slot. *)
  val diagnostic
    :  t
    -> slot:Agent_session.Callback_cleanup.Completion_snapshot.Failure_slot.kind
    -> failure:Agent_session.Callback_cleanup.Failure.t
    -> Agent_session.Cleanup_diagnostic.t

  val read_failure : t -> Agent_session.Cleanup_diagnostic.t option
  val diagnostics : t -> Agent_session.Cleanup_diagnostic.t list
end
