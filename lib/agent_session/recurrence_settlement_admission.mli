(** Current controller evidence admission, separate from target authentication.
    The actual actor must authenticate an opaque target observation, same Store
    and any saved raw reference before using this helper. No constructor here
    issues a target observation, retry scope or execution permission. *)
type t

(** Classify exact retained data before new-work gates. Both changed and unchanged
    results require current Send_messages, nonyielding host evidence visibility
    for the exact historical definition, and genuine controller Store/Handle
    observation. The caller may differ from the original request principal.

    Only a new current definitive fixed-delay settlement may use automatic_policy;
    it is additionally intersected with actual enabled/running/source/birth and
    automatic lifecycle eligibility. This is rearm metadata policy, never target
    execution authority. Original intent control revision remains audit data.
    Callbacks must not yield; Store observation may yield. Complete saved proof
    carriers are preserved strictly, not normalized or reconstructed. *)
val prepare
  :  Session_state.t
  -> controller:Recurrence_controller.t
  -> actor:Operator_authorization.t
  -> authorize:
       (Session_state.t
        -> Recurrence_definition.t
        -> (unit, Agent_protocol.Error.t) result)
  -> automatic_policy:
       (Session_state.t
        -> Recurrence_definition.t
        -> (bool, Agent_protocol.Error.t) result)
  -> origin:Recurrence_run_origin.t
  -> terminal:Recurrence_run_reference.t option
  -> (t, Agent_protocol.Error.t) result

val definition : t -> Recurrence_definition.t
val catalog : t -> Recurrence_catalog.t
val settlement : t -> Recurrence_settlement.t

(** Recompute historical-only evidence against the SAME original catalog and
    exact evidence, never apply a second mutation after a proposed successor.
    Does not perform IO or grant a timing/terminal proof. *)
val hold : t -> (t, Agent_protocol.Error.t) result

(** Synchronous final original proof/policy, genuine Current and exact captured
    entry/catalog/session basis check. A proposed automatic successor also needs
    current automatic policy and eligibility. Historical data never upgrades
    itself into current execution authority. Own ACK rotates the Store epoch;
    this precommit witness must not be reused as a postACK target read proof. *)
val check_current : t -> Session_state.t -> (unit, Agent_protocol.Error.t) result

(** Final original Send_messages proof/scope, exact captured
    controller/session/catalog/entry/Current and automatic-eligibility check.
    Original currentness executes trusted nonyielding host code; invokes neither
    evidence nor automatic policy. A joint importer runs all policies first,
    repeats both originals and rechecks the target's pure basis after this
    original callback immediately before its effect. Does not replace policy
    checks or establish a callback-free activation boundary. *)
val check_basis : t -> Session_state.t -> (unit, Agent_protocol.Error.t) result
