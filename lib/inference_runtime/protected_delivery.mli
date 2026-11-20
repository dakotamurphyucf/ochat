open! Core

type error =
  | Unavailable
  | Invalid_output
  | Limit_exceeded
  | Expired
[@@deriving equal, sexp_of]

exception Rejected of error

type t

(** Trusted host handoff after the exact safe call and output occurrences commit. Both
    callbacks belong to this operation, recipient, selected invocation and
    immutable source revision. Materialization checks current visibility and
    emits only bounded tool data for the supplied output. Refresh may yield to
    recheck the same authority using the actual inference scope. Its final check
    is nonyielding and must verify that the materialized revision still has
    authorized custody. The state returned here cannot be reconstructed from
    durable or provider output. *)
val create
  :  call:History_entry.t
  -> output:History_entry.t
  -> materialize:(unit -> (History_entry.Payload.Output.t, error) result)
  -> refresh:(scope:Transcript.Scope.t -> unit -> bool)
  -> t

(** Retire on cancellation, operation exit or loss of the immediate successor.
    Retiring is safe to repeat and cannot revoke an already authorized send. *)
val retire : t -> unit

(** Reject an unclaimed handoff without disturbing an already claimed attempt. *)
val expire_pending : t -> unit

val max_outputs_per_submission : int

module Materialized : sig
  type pending = t
  type t

  (** Consumes the live input for this one preparation. All output occurrences
      must match the final effective history before any body is materialized.
      Applies aggregate byte and count limits. Failed preparation cannot leave
      an input armed for a later unrelated request. *)
  val prepare : pending list -> history:History_entry.t list -> (t, error) result

  (** Trusted adapter only. By exact committed host occurrence. *)
  val find : t -> History_entry.Id.t -> History_entry.Payload.Output.t option

  (** Drop the projection inputs when adapter preparation returns. The
      submitted wire preparation and authority checks retain their own state. *)
  val release_contents : t -> unit

  (** Bind once to the actual attempt. Live results require a root attempt.
      Argument isolation with no live results can bind to a nested attempt.
      Refresh may yield. The final check admits at most one application submission. *)
  val bind : t -> scope:Transcript.Scope.t -> (unit -> unit -> unit, error) result
end
