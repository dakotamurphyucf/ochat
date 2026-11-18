open! Core

(** Private, nonactivating native-resource identity and validation DATA.
    No function reads an Actor/Store, acquires resources, executes a callback,
    yields, retries or creates permission. Existing private/caller authorization
    remains in Factory, outside Actor/Owner locks. *)

(** Structural proof choice, before validation or any Error. A checkpoint
    without a lifecycle pointer requires an actual installed Cell Source, including
    when its Run source is present. Unmoderated endpoints and present lifecycle
    pointers retain the original State proof. A malformed/stale present pointer
    never triggers Source fallback. *)
val requires_installed_source : Agent_session.Session_state.t -> bool

(** Exact extraction of the existing Factory Native_moderation_identity.of_state.
    Preserves absent checkpoint, modern provenance, nonhalted observer and
    Legacy/Unknown behavior. Does not require Completed policy restoration.
    [installed_source], when provided, must be the SAME immutable Source from an
    actual current Owner/Cell observation, rechecked around waits by Factory.
    A present lifecycle pointer still requires strict State validation. With an
    absent pointer the genuine Extension selection must match the nonhalted
    checkpoint and any full present Run-source installation; a Source never
    replaces invalid present provenance. No provenance or custody is issued by
    this pure function. *)
val moderation_source
  :  ?installed_source:Agent_session.Runtime_builder.Source.t
  -> Agent_session.Session_state.t
  -> (Agent_protocol.Invocation.observer option, Agent_protocol.Error.t) result

(** Current-use fingerprint with the SAME installed observation rules as
    [moderation_source]. This is separate from construction [fingerprint]. *)
val current_fingerprint
  :  ?installed_source:Agent_session.Runtime_builder.Source.t
  -> Agent_session.Session_state.t
  -> (string, Agent_protocol.Error.t) result

(** Permission-walk fingerprint from independently verified inert moderator
    DATA. This uses the same current checkpoint, observer and present lifecycle
    rules. It supplies no installed Source evidence or resource authority. *)
val eligibility_fingerprint
  :  moderator:Agent_session.Runtime_builder.Source.Moderator.t
  -> Agent_session.Session_state.t
  -> (string, Agent_protocol.Error.t) result

(** Exact existing nonactivating construction fingerprint route. Its optional
    actual selected source remains available ONLY to genuine native construction
    callers, including Pending Init. It does not acquire the NEW current-use
    installed-Cell proof and its existing lifecycle/observer rules are unchanged.
    Hot checks use [current_fingerprint] with separately observed provenance. *)
val fingerprint
  :  ?source:Agent_session.Runtime_builder.Source.t
  -> Agent_session.Session_state.t
  -> (string, Agent_protocol.Error.t) result

(** Exact existing lifetime-specific active-state predicate: Independent keeps
    its resource-only stopped law; Owned/Invocation_owned require Running and
    eligible observed state, nonhalted and no failure. Does not perform the
    separate specific invocation check or grant/private-record authorization. *)
val check_active
  :  Agent_store.Delegation_store.record
  -> Agent_session.Session_state.t
  -> (unit, Agent_protocol.Error.t) result

(** Comparison DATA for the NEW installed-current-Source wait window only.
    Reuses Basis's complete common identity read-set and exact physical Source;
    also binds exact stop/parent-stop epochs and stable eligible Running or
    Stopped liveness. Halted, failed or transitional states conservatively refuse.
    Does not compare factual revision/event/transaction/ACK progress. Does not
    replace lifetime/Invocation eligibility or issue admission/custody. Existing
    construction proof and Basis resource-only Stop semantics are unchanged. *)
module Source_window : sig
  type t

  val capture
    :  source:Agent_session.Runtime_builder.Source.t
    -> Agent_session.Session_state.t
    -> (t, Agent_protocol.Error.t) result

  val check
    :  t
    -> expected:Agent_session.Session_state.t
    -> (unit, Agent_protocol.Error.t) result
end

module Basis : sig
  type t

  (** Short-lived comparison DATA for hot native pin/check only. [record] is the
      SAME actual Linked/unrevoked record returned by the existing twice-resolved
      admission check. [state] is its ORIGINAL immutable Actor snapshot, never a
      newer State substituted after authorization. Pure, creating-domain only.
      Uses current-use identity/fingerprint and exact installed-source extraction.
      Captures proof kind (State or installed Source) and, for the latter, the
      SAME physical immutable selected Source, separately from the full actual
      Run_source option. Factory must first validate current Cell provenance.
      Capture validates the record/initial target association and existing
      invocation/active predicates; it grants no admission by itself. *)
  val capture
    :  ?installed_source:Agent_session.Runtime_builder.Source.t
    -> record:Agent_store.Delegation_store.record
    -> Agent_session.Session_state.t
    -> (t, Agent_protocol.Error.t) result

  (** Consume an opaque observation from the SAME validated native metadata
      prefix. Retains its exact terminal record and original endpoint State;
      only the actual walker-derived Stop boundary can permit resource-only
      liveness above a lower Independent edge. No caller Boolean or terminal
      membership can select that law. Other identity/source/invocation checks
      are identical to [capture]; this DATA grants no admission or execution. *)
  val capture_resource_path
    :  ?installed_source:Agent_session.Runtime_builder.Source.t
    -> Agent_session.Delegation_authority.Resource_path.Observation.t
    -> (t, Agent_protocol.Error.t) result

  (** Pure final read-set validation against the SAME bound record. Preserves
      session/generation/principal, complete Resource_identity, exact installed
      source option, actual modern fingerprint proof, Init/pending-start and
      owner-lease generation. Rechecks lifetime-appropriate liveness and the original specific
      Invocation_owned dispatch predicate. Owned/Invocation_owned stop and parent
      stop epochs remain exact under an Ordinary boundary. An opaque validated
      Independent_resource prefix preserves the resource-only Stop/liveness law
      even when its terminal record is Owned. Invocation_owned is always checked.

      Check rejects State/Source proof switching, omission of a captured Source,
      or another physical Source even when observer/hash payloads are equal.
      It never refreshes the captured proof after a wait. A mismatch returns the existing nonretryable authority-changed Conflict;
      failed source proof preserves its typed error. Must immediately precede the
      existing pure Cell pin/check without a yield. Never replaces original
      authorize/Store/grant/Owner/profile checks or later policy/Goal/Caller checks. *)
  val check
    :  ?installed_source:Agent_session.Runtime_builder.Source.t
    -> t
    -> expected:Agent_session.Session_state.t
    -> (unit, Agent_protocol.Error.t) result
end
