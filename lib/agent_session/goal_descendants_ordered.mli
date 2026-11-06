(** Projection-only ordered DATA validator. No Session_state/Session_delta,
    IO, Handles, persistent ledger, authorization, or serialization. *)
module Projection : sig
  type t =
    { ledger : Inference_ledger.t
    ; accounting : Goal_descendants.t option
    ; children : Child_contributions.t option
    ; catalog : Goal_catalog.t option
    ; accounts : Goal_accounts.t option
    ; delegation : Agent_store.Delegation_store.Reference.t option
    ; active_operation : Agent_protocol.Operation.t option
    ; invocations : Agent_protocol.Invocation.t list
    ; moderator_executions : Agent_protocol.Moderator_execution.t list
    }
end

type t

(** Immutable original revision/ledger and actual current projections. Uses the
    actual local Session ID; no host-qualified Session_ref is fabricated. Does
    not reinterpret historical absence as birth. Maximum revision remains valid
    for unchanged reads/checkpoints; local inclusion requires increment room. *)
val begin_
  :  session_id:Agent_protocol.Id.Session.t
  -> session_revision:int64
  -> Projection.t
  -> (t, Agent_protocol.Error.t) result

val current : t -> Projection.t
val expected_session_revision : t -> (int64, Agent_protocol.Error.t) result

(** Check the actual final publication revision after any local Record_included.
    Its obligation survives later correction/reset and other ordered leaves.
    Unstamped prefix/archive validation uses [check] without publishing. *)
val validate_publication : t -> revision:int64 -> (unit, Agent_protocol.Error.t) result

type step =
  | Catalog of Goal_catalog.t
  | Accounts of Goal_accounts.t
  | Canonical of Inference_ledger.t
  | Reset_generation of int
  | Ordinary_children of Child_contributions.t
  | Active_operation of Agent_protocol.Operation.t option
  | Invocation of Agent_protocol.Invocation.t
  | Moderator_execution of Agent_protocol.Moderator_execution.t
  | Replacement of Projection.t
  | Update of
      { update : Goal_descendants.Update.t
      ; creation : (Agent_protocol.Goal.t * Goal_accounting_document.t) option
      }

(** Observe exactly the accepted ordered leaf projection. Canonical records the
    actual before/after ledger pair, never a global original-to-final shortcut.
    A positive own inherited origin with a fresh canonical admission creates one
    unresolved inclusion obligation; another changed canonical edge cannot erase
    it. Only matching Record_included consumes that edge once. Corrections and
    reset create no admission obligation and cannot justify Record_included.

    Update invokes Goal_descendants.validate_transition against current fields;
    Register_new_root uses ORIGINAL ledger plus actual captured creation, while
    Record_included uses the saved SAME local canonical edge and original +1
    session revision. Other reasons use current ledger as before=after.

    Untracked Attempt operation labels require a same-generation Running active
    operation or a retained same-session/generation moderator execution context
    with that operation and Running or Completed with pending intent status.
    Any invocation label independently requires its retained same-session and
    generation record. Tracked canonical Handles supply actual row association.
    These factual checks do not authenticate private execution owners.

    creation is supplied only from Ordered_bootstrap.creation, retaining its
    actual captured pre-episode Goal and original-ledger bootstrap account. Replacement cannot change known accounting or
    ordinary scoped-child projection, reset the original basis, or manufacture
    sequential catalog creation. Other changed scalar projections remain the
    existing native/owner validators' responsibility. *)
val advance : t -> step -> (t, Agent_protocol.Error.t) result

(** Reject unresolved local inclusion; compare all owned current projections to
    the actual final native candidate without hashes; validate final domain
    context. Owner stamping of time/commit counters is separate and must use the
    captured expected revision for any newly included local occurrence. *)
val check : t -> incoming:Projection.t -> (unit, Agent_protocol.Error.t) result

(** Original-basis guard for encode/adopt without issued ordered evidence. Known
    descendant accounting stays equal with compatible raw preservation. Unknown
    child slots may receive ordinary factual changes; known-origin slots cannot
    move through this route. Positive inherited origin permits no fresh canonical
    count growth. Corrections/reset and max-revision unchanged snapshots remain
    valid. Actual authored new-birth boundary remains the State owner's check. *)
val validate_checkpoint
  :  before:Projection.t
  -> incoming:Projection.t
  -> (unit, Agent_protocol.Error.t) result
