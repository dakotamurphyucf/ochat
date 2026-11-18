open! Core

(** Pure current-installation eligibility DATA. It grants no construction,
    source, original, writer, native or callback authority. The actual Factory
    and Product must additionally select and check the extension definition. *)
type t

(** Requires an ALREADY installed Run index source, its current lifecycle pointer's
    existing Completed receipt, Ready initialization and a validated complete
    checkpoint. Missing source/index/pointer is unavailable, never projected
    into existence from checkpoint DATA. No historical-success search. *)
val of_state : Session_state.t -> (t, Agent_protocol.Error.t) result

val lifecycle : t -> Moderator_lifecycle.t

(** Checks the same session/generation/resource spec/Stop/source/selected pointer
    and complete raw checkpoint envelope, including unknown fields and optional
    queue metadata. Does not require unchanged unrelated metadata/counters. *)
val check_construction
  :  t
  -> state:Session_state.t
  -> (unit, Agent_protocol.Error.t) result

(** [state] must be the actual already-validated immutable Actor State, never an
    unchecked input record. Only this module's bounded local eligibility projection
    is repeated: no full State/history/ledger validation or checkpoint decode.
    Resource identity comes from that validated Spec.

    Callers must repeat this check after every wait and at each
    policy admission/effect/commit/disclosure boundary. Own admitted policy
    checkpoint ACKs may advance the full checkpoint; actual serialized Manager
    capture separately checks its current affinity. A newer Running/Failed/
    Interrupted lifecycle cannot reuse an earlier successful pointer. *)
val check_policy : t -> state:Session_state.t -> (unit, Agent_protocol.Error.t) result
