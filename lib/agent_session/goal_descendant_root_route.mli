(** PRIVATE pure association DATA beneath the actual Actor route issuer.
    The caller already holds SAME physical Preparation and authenticates actual
    Factory issuer, Checked_ancestry and validated Store-resolved records. No
    host/schema limits are invented or repeated here; record validity is the
    genuine Store owner's precondition. This helper
    grants no route, currentness, reservation, ACK or execution authority. *)
type t

(** Select ALL scopes in the complete preparation vector whose full owner is
    [session]. Match each frozen Anchor to its exact first-child private record,
    including transaction, full Reference and positive frozen origin; require
    exact retained local GD root/branch and C slot/origin. Source/creator episode
    are compared within the frozen association, never against today's episode.
    Missing/conflicting association is an error, not historical zero or a newly
    registered frontier. An unrelated owner legitimately selects no routes.
    The existing Checked_ancestry owner bounds/validates the record vector. *)
val select
  :  session:Agent_protocol.Session_ref.t
  -> scopes:Agent_store.Delegation_goal_origin.Scope.t list
  -> records:Agent_store.Delegation_store.record list
  -> accounting:Goal_descendants.t
  -> children:Child_contributions.t
  -> (t list, Agent_protocol.Error.t) result

val scope : t -> Agent_store.Delegation_goal_origin.Scope.t
val reference : t -> Agent_store.Delegation_store.Reference.t

(** Pure retained association recheck against actual current root DATA. Never
    creates a root/branch/child slot or rewrites its captured origin. *)
val validate
  :  t
  -> accounting:Goal_descendants.t
  -> children:Child_contributions.t
  -> (unit, Agent_protocol.Error.t) result
