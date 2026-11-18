open! Core

(** ONE actual canonical invocation. Retains actual host progress and
    its first failure, including failures before the persistence callback itself.
    No admission, execution authority, retry writer, or journal ACK is issued. *)
module Context : sig
  type t =
    | Constructor_model
    | Delegated_policy_claim
    | Causal_canonical of
        { slot : unit ref
        ; preparation : unit ref
        }
    | Causal_reserve of
        { attempt : unit ref
        ; set : unit ref
        }
    | Causal_terminal of
        { operation : unit ref
        ; reservation : unit ref
        }
end

type t
type invocation

val create
  :  context:Context.t
  -> previous:Session_state.t
  -> candidate:Session_transition.t
  -> t

val observation : t -> Session_commit_observation.Owner.t

(** Only an uncalled callback with an actual failure, or its observed pre-writer
    rejection, proves no writer. An unobserved callback Error remains unknown. *)
val no_writer : t -> bool

(** Return/reraise the first actual failure; otherwise typed unresolved Conflict. *)
val unresolved : t -> ('a, Agent_protocol.Error.t) result

(** Preserve the first actual enclosing guard/write/install outcome unchanged.
    The canonical persistence callback still runs solely through its existing
    Session_commit_observation.Owner. *)
val perform
  :  t
  -> (unit -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Elect the first persistence invocation before any persistence-related
    mutation. A finished/entered/unknown attempt cannot elect another invocation,
    even with the same physical candidate. Factual retry prepares a new attempt
    only after [no_writer]. *)
val begin_invocation : t -> (invocation, Agent_protocol.Error.t) result

val check_invocation : t -> invocation -> (unit, Agent_protocol.Error.t) result

(** Finalize only the SAME elected invocation once. If factual no-writer sealing
    happened first, this consumes its retained finalization obligation without
    reopening admission or changing the retained failure. Foreign/duplicate
    finalization remains an invariant violation. *)
val finish_invocation : t -> invocation -> unit

(** Factual no-writer evidence for only the closed causal writer contexts.
    Issued from the same retained original attempt/outcome on its creating domain.
    Sealing permanently forbids any further invocation of that attempt. No
    current-State, fresh permission, retry, admission or ACK is produced. *)
module No_writer : sig
  type attempt = t
  type t

  val seal : attempt -> (t, Agent_protocol.Error.t) result

  (** Affinity is physical identity of the same attempt, context owners and its
      create-time previous/candidate; equal replacement values do not match.
      Foreign-domain checks return false before reading mutable attempt state. *)
  val matches_canonical
    :  t
    -> attempt:attempt
    -> slot:unit ref
    -> preparation:unit ref
    -> previous:Session_state.t
    -> candidate:Session_transition.t
    -> bool

  val matches_reserve
    :  t
    -> attempt:attempt
    -> reservation_attempt:unit ref
    -> set:unit ref
    -> previous:Session_state.t
    -> candidate:Session_transition.t
    -> bool

  val matches_terminal
    :  t
    -> attempt:attempt
    -> operation:unit ref
    -> reservation:unit ref
    -> previous:Session_state.t
    -> candidate:Session_transition.t
    -> bool
end
