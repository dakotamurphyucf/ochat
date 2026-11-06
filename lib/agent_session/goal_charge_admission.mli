(** Pure fresh-admission planning against an actual owner's immutable attribution.
    No attribution or permission is minted here. Missing historical accounts are
    errors, even for unlimited policies. *)
type ready

val goal_id : ready -> Agent_protocol.Id.Goal.t
val episode : ready -> Agent_protocol.Goal_episode.t

type deferred =
  { goal_id : Agent_protocol.Id.Goal.t
  ; reason : Goal_admission_policy.Deferred.t
  }

type decision =
  | Unbound
  | Ready of ready
  | Deferred of deferred
  | Held of
      { goal_id : Agent_protocol.Id.Goal.t
      ; reason : Agent_protocol.Goal.Hold.t
      ; delta : Session_delta.t option
      }

val prepare
  :  Session_state.t
  -> attribution:Goal_work_attribution.t option
  -> now:Agent_protocol.Timestamp.t
  -> dimension:Goal_admission_policy.dimension
  -> (decision, Agent_protocol.Error.t) result

(** [Deferred] is a no-write decision for Reserved/Unknown count capacity, not
    an Exhausted Hold. The actual admission owner chooses its defer/error path. *)

(** Plan only a safe hold on an existing current Enabled goal. Existing explicit
    holds remain unchanged; absent/never-committed candidate goals stay absent.
    The caller authenticates the actual denied admission independently. *)
val hold_delta
  :  Session_state.t
  -> goal_id:Agent_protocol.Id.Goal.t
  -> reason:Agent_protocol.Goal.Hold.t
  -> (Session_delta.t option, Agent_protocol.Error.t) result

(** NEW activation policy using actual accepted root-scoped descendants. No count
    check/charge, authority, epoch or root is created. An absent historical child
    component contributes labelled Unknown coverage, never exact historical zero.
    Existing account/ledger evidence is checked even without a token ceiling. *)
val check_activation
  :  Session_state.t
  -> goal:Agent_protocol.Goal.t
  -> accounting:Goal_accounting.t
  -> now:Agent_protocol.Timestamp.t
  -> (Goal_admission_policy.decision, Agent_protocol.Error.t) result
