(** Pure claim-prefix DATA preparation. The Actor supplies the SAME authenticated
    Bound selection only after its existing capture/access/currentness checks.
    This module constructs neither custody nor permission. Input State is the
    actual source-installation prefix before the new Running receipt. *)
module P = Agent_protocol

type selection =
  | Standalone of Policy_standalone_source.t
  | Registered of
      { data : Delegated_policy_governance_data.Registered.t
      ; callee : P.Session_ref.t
      ; path : Agent_store.Delegation_store.Reference.t list
      }

type t

val prepare
  :  Session_state.t
  -> selection:selection
  -> source:P.Run_source.t
  -> receipt:P.Moderator_execution.t
  -> max_inherited_scopes:int
  -> (t, P.Error.t) result

(** Receipt first, followed by its exact optional Cause registration. Source
    installation belongs to the preceding actual Actor-prepared prefix. *)
val changes : t -> Session_delta.t list

(** Prebuilt new-subject association. It becomes owned only with the existing
    claim State ACK; Unknown here accompanies a positive Registered Cause, never
    an inferred unrelated or missing-governance fallback. *)
val classification : t -> Policy_standalone_source.Classification.t
