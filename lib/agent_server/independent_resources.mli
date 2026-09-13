type ancestor =
  { state : Agent_session.Session_state.t
  ; owner : Runtime_owner.t
  }

type host =
  { find : Agent_protocol.Id.Session.t -> (ancestor, Agent_protocol.Error.t) result
  ; resolve :
      Agent_store.Delegation_store.Reference.t
      -> (Agent_store.Delegation_store.record, Agent_protocol.Error.t) result
  ; authorize :
      Agent_store.Delegation_store.record -> (unit, Agent_protocol.Error.t) result
  ; build_root :
      sw:Eio.Switch.t
      -> ancestor
      -> (Agent_session.Runtime_builder.resources, Agent_protocol.Error.t) result
  ; build_generated :
      sw:Eio.Switch.t
      -> parent:Agent_session.Runtime_builder.resources
      -> ancestor
      -> (Agent_session.Runtime_builder.resources, Agent_protocol.Error.t) result
  }

(** Qualified private ancestry, without native resource setup or execution.
    Stateful parent moderation requires a separate original-owner service and is
    currently rejected. Current host policy must validate Independent grants. *)
val check_parent
  :  max_depth:int
  -> host:host
  -> parent_id:Agent_protocol.Id.Session.t
  -> (unit, Agent_protocol.Error.t) result

type t

val find
  :  t
  -> Agent_protocol.Id.Session.t
  -> (Agent_session.Runtime_builder.resources, Agent_protocol.Error.t) result

(** Retain all ancestor owners without loading execution runtimes; construct
    root-to-leaf resources under a dedicated switch, recheck current private
    ancestry and keep everything alive through [f]. Permanent owner closure or
    caller cancellation joins the whole resource scope. This only accepts an
    explicitly authorized Linked Independent admission. Child execution must
    still install Delegation_authority for each effect/disclosure boundary. *)
val with_chain
  :  max_depth:int
  -> host:host
  -> reference:Agent_store.Delegation_store.Reference.t
  -> f:(t -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result
