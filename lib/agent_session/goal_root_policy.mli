(** Private helper beneath Session_actor.
    A process-local stable Goal policy cell, never ancestry, reservation, original
    authority or a second counter. Only the actual authenticated root handler
    can place this cell into opaque Reservation/Root_* runtime owners. *)
type t

(** Actual root State must contain this local GD root, same full Session_ref,
    current catalog Goal and resolvable pre-episode account. Creates a fresh live
    cell for current source/budget/objective, excluding creator episode/Run.
    Creation requires Enabled; selected token policy is checked by [fresh].
    Caller has already authenticated actual Factory/Store/route, not Goal DTOs. *)
val create
  :  Session_state.t
  -> goal_id:Agent_protocol.Id.Goal.t
  -> (t, Agent_protocol.Error.t) result

val goal_id : t -> Agent_protocol.Id.Goal.t

(** Permanent, nonyielding retirement. A later current cell never revives this
    one. Reservation captures its SAME cell before real reserve ACK and keeps it
    through child ACK/confirmation; refresh of that Reservation never replaces it. *)
val retire : t -> unit

(** Prewriter DATA compatibility only: no clock, callback, RPC, Store observation
    or fresh token/count admission. Retire on absent Goal/root/account, owner or
    generation change, closed Goal, source/budget/objective change, or a new Held
    state unless the actual owning writer supplies matching [admission_hold].
    Enabled/episode/progress/Run changes alone do not retire this stable cell.
    A retired cell remains retired, including when a writer fails.

    Explicit controls additionally call [retire] even if Goal state is equal.
    This is the existing Actor retire_goal_activation seam, not inferred intent. *)
val retain_candidate
  :  t
  -> previous:Session_state.t
  -> candidate:Session_state.t
  -> admission_hold:Agent_protocol.Id.Goal.t option
  -> unit

(** Current root scoped fresh control/deadline/selected observed-token policy,
    using SAME Goal_charge_admission.check_activation and actual root State.
    No dimension count check: real root reserve owns the capacity decision and
    refresh preserves its exact pending reservation. Returns Hold DATA without
    a write, rather than granting execution or minting a reservation.
    Account integrity is checked even without token policy. *)
val fresh
  :  t
  -> state:Session_state.t
  -> now:Agent_protocol.Timestamp.t
  -> (Goal_admission_policy.decision, Agent_protocol.Error.t) result

(** SAME live cell, current source/budget/objective/account, fresh deadline and
    nonclosed Goal. An automatic admission Hold may grandfather this existing
    cell; explicit same-Hold already retired it before writer. No count/token
    eligibility or creator episode/Run/liveness check. Child original authority
    is a separate mandatory composed check at actual child effect/submission. *)
val admitted
  :  t
  -> state:Session_state.t
  -> now:Agent_protocol.Timestamp.t
  -> (unit, Agent_protocol.Error.t) result
