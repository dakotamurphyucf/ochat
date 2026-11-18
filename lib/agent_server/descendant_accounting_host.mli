(** The Factory adapter for descendant accounting. Both ports are driven from
    the existing child outer scope, outside Actor mailboxes and Owner/Registry
    locks. The supplied checks retain their distinct fresh and factual
    authority contracts. *)

type host =
  { registry : Session_registry.t
  ; store : Agent_store.Session_store.t
  ; runtime_source_owner : Agent_session.Runtime_source_owner.t
  ; check_registered_owner :
      Session_registry.entry
      -> session:Agent_protocol.Session_ref.t
      -> (unit, Agent_protocol.Error.t) result
  ; check_registered_factual_owner :
      Session_registry.entry
      -> session:Agent_protocol.Session_ref.t
      -> (unit, Agent_protocol.Error.t) result
  }

(** Both ports use the supplied Handle and live Actor reference for the source
    session. The causal port also captures the actual Actor at creation for
    factual completion on that exact source. The original ancestry check is
    repeated at the existing fresh admission boundaries. *)
val create
  :  host
  -> handle:Agent_store.Session_store.Handle.t
  -> actor_ref:Agent_session.Session_actor.t option ref
  -> check_ancestry:
       (unit
        -> ( Agent_session.Delegation_authority.Checked_ancestry.t
             , Agent_protocol.Error.t )
             result)
         option
  -> Agent_session.Session_actor.Descendant_host.t
     * Agent_session.Session_actor.Descendant_host.Causal_driver.t
