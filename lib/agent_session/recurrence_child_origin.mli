open! Core

(** Validated upper interpretation of immutable captured delegation custody.
    Neither decoding nor reading selectors grants permission to create, restore
    an execution runtime, initialize a script, or admit a Run. Artifact bytes
    remain owned by the existing artifact store. *)
type t

(** Capture the exact retained definition JSON and selected reservation. The
    initial Spec digest normalizes only its delegation locator to absent; the
    actual locator is independently checked against the admitted record. *)
val create
  :  Recurrence_definition.t
  -> reservation:Recurrence_child_reservation.t
  -> initial_spec:Session_state.Spec.t
  -> (Agent_store.Delegation_captured_origin.t, Agent_protocol.Error.t) result

(** Interpret an actual record's captured source kind, validating its immutable
    child, transaction, source, request and artifact associations. This checks
    data only: the factory must separately resolve the genuine record, verify
    the installed exact artifact publication and authorize each effect. *)
val of_record : Agent_store.Delegation_store.record -> (t, Agent_protocol.Error.t) result

(** Same immutable source selector validated with the lower captured origin.
    The actual source owner/private bindings must still be resolved and checked. *)
val source_kind : t -> Agent_store.Captured_source_kind.t

(** Validate against an actual full controller definition supplied by its owner:
    exact reservation/target, source kind and original Prompt_definition. Neither
    this comparison nor of_record authenticates that definition or grants effects.
    of_record alone cannot compare to a full definition it does not retain. *)
val validate_definition
  :  t
  -> Recurrence_definition.t
  -> (unit, Agent_protocol.Error.t) result

val reservation : t -> Recurrence_child_reservation.t
val definition : t -> Prompt_definition.t

(** Source associations include a present parent-stop acknowledgement at least
    as large as the immutable admitted floor. Existing stop/recovery ownership
    may advance this acknowledgement; exact equality and own-stop zero are not
    required. These counters grant no current permission. *)
val validate_source : t -> Session_state.t -> (unit, Agent_protocol.Error.t) result

(** Narrow intentionally-never-initialized recovery check. Requires the original
    Spec/reference and stopped Pending/fresh-history state with empty canonical,
    deferred, pending and archived history, zero allocators, and no Run, retained
    or retired inference/host-turn, pre-tracking uncertainty, or other execution
    evidence. Full captured content remains in the artifact until genuine
    initialization; no literal-only initial-history conversion is applied here.
    It does not relax ordinary interrupted initialization. *)
val validate_initial_state : t -> Session_state.t -> (unit, Agent_protocol.Error.t) result
