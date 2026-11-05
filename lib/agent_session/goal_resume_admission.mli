(** Pure existing-episode command planning. No Run admission, source capture,
    binding, original authority, account bootstrap or execution owner is issued.
    The actual Actor authenticates both originals and the same live binding. *)
type t

(** Exact principal/key/digest lookup before resource acquisition. Historical
    receipt DATA does not require the Goal or episode to remain current. Actual
    target association and current command/read authority are Actor checks. *)
val find_receipt
  :  Session_state.t
  -> request:Goal_resume_request.t
  -> principal_id:Agent_protocol.Id.Principal.t
  -> (Goal_command_receipt.t option, Agent_protocol.Error.t) result

module Decision : sig
  type plan = t

  type t =
    | Retained of Goal_command_receipt.t
    | Commit of plan
end

(** On a fresh key, validate the original Goal/catalog/Run CAS, full session and
    source identity, current nonterminal Workflow/episode/link/admission and
    canonical account. Select its existing physical binding separately in Actor.
    Resume the same held Goal exactly once, then check global activation policy
    on that Enabled candidate (no count check/charge). Preserve Run index, link,
    episode, account, usage and pending Wait exactly; commit only the preserved
    Goal document and command receipt. A new episode is never planned here. *)
val prepare
  :  Session_state.t
  -> request:Goal_resume_request.t
  -> principal_id:Agent_protocol.Id.Principal.t
  -> session:Agent_protocol.Session_ref.t
  -> now:Agent_protocol.Timestamp.t
  -> (Decision.t, Agent_protocol.Error.t) result

val catalog : t -> Goal_catalog.t
val receipt : t -> Goal_command_receipt.t
val goal : t -> Agent_protocol.Goal.t
val run : t -> Agent_protocol.Run.t
val admission : t -> Agent_protocol.Run_receipt.t
val link : t -> Run_goal_link.t

(** Callback-free check of this exact semantic session/revision and selected
    immutable facts immediately before its writer. This is not a fresh policy or
    original check, nor permission to reuse a plan after another acknowledged
    write. Actor replans from its actual current state after owned source ACKs;
    it never rewrites the original request fences. *)
val check_basis : t -> state:Session_state.t -> (unit, Agent_protocol.Error.t) result
