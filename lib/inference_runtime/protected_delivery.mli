open! Core

type error =
  | Unavailable
  | Invalid_output
  | Limit_exceeded
  | Expired
[@@deriving equal, sexp_of]

exception Rejected of error

(** Bounded custody elapsed. The backend resolves delivery uncertainty from
    whether an application write started. *)
exception Lease_timeout

module Lease : sig
  module Source : sig
    type t

    (** Canonical host and session components. The pair is compared without
        concatenation and never emitted in inference diagnostics. *)
    val create : host:string -> session:string -> t
  end

  module Authority : sig
    type t

    (** One private identity per host custody implementation. A duplicate source
        key with distinct authority identities is rejected. *)
    val create : unit -> t
  end

  (** Acquire the actual source's exclusion and release it on return, exception
      or cancellation. Pass a synchronous current-ownership check to the scoped
      callback. Bound acquisition and the cancellable write lifetime. *)
  type scope = { run : 'a. ((unit -> bool) -> 'a) -> 'a }

  type t

  val create : Authority.t -> source:Source.t -> scope:scope -> t
end

type custody =
  | Local
  | Source of Lease.t

type t

(** Trusted host handoff after the exact safe call and output occurrences commit. Both
    callbacks belong to this operation, recipient, selected invocation and
    immutable source revision. Materialization checks current visibility and
    emits only bounded tool data for the supplied output. Refresh may yield to
    recheck the same authority using the actual inference scope. Its final check
    is nonyielding and must verify that the materialized revision still has
    authorized custody. Shared notes and retained sources use [Source], even
    when the source and recipient are on the same host. Durable or provider
    output cannot reconstruct this process-local state. *)
val create
  :  call:History_entry.t
  -> output:History_entry.t
  -> materialize:(unit -> (History_entry.Payload.Output.t, error) result)
  -> custody:custody
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

  val has_live_results : t -> bool

  (** Drop the projection inputs when adapter preparation returns. The
      submitted wire preparation and authority checks retain their own state. *)
  val release_contents : t -> unit

  type submission =
    { refresh : unit -> unit -> unit
    ; custody : Lease.scope option
    }

  (** Bind once to the actual attempt. Live results require a root attempt.
      Argument isolation with no live results can bind to a nested attempt.
      Refresh may yield. The final check admits at most one application submission. *)
  val bind : t -> scope:Transcript.Scope.t -> (submission, error) result
end
