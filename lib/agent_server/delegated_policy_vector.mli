(** An ordered delegated policy traversal. The Factory supplies the original
    authorization and the current metadata lookup. Membership observes exact
    native targets, and resource scopes hold the pins used for evaluation. *)

type host =
  { registry : Session_registry.t
  ; runtime_source_owner : Agent_session.Runtime_source_owner.t
  ; native_metadata_target :
      Agent_protocol.Id.Session.t
      -> ( Session_registry.entry * Agent_session.Session_state.t
           , Agent_protocol.Error.t )
           result
  }

type validation =
  { authorize : unit -> (unit, Agent_protocol.Error.t) result
  ; read_ancestry :
      unit
      -> ( Agent_session.Delegation_authority.Checked_ancestry.t
           , Agent_protocol.Error.t )
           result
  }

type membership
type prepared
type bound

(** Open the actual ancestor Sources in private ancestry order. Use the
    membership within [f]. A failed or raised callback keeps its primary
    outcome. A successful callback is subject to current target checks. *)
val with_members
  :  host
  -> ancestry:Agent_session.Delegation_authority.Checked_ancestry.t
  -> sources:Agent_session.Runtime_builder.ancestor_sources
  -> validation:validation
  -> f:(membership -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Revalidate the exact ancestry and member Sources. A restoration-specific
    validation still checks the original membership's issuer and references. *)
val check_members
  :  ?validation:validation
  -> membership
  -> ( Agent_session.Delegation_authority.Checked_ancestry.t
       , Agent_protocol.Error.t )
       result

(** Enter every required resource before [f] and preserve the actual reports
    on scope exit. Use prepared and bound values within [f]. A resource's
    restoration check receives the exact Session ID of that member. *)
val with_resources
  :  membership
  -> actor:Agent_session.Session_actor.t
  -> request:Agent_session.Script_tool_calls.preparation
  -> governance:Agent_session.Session_actor.Causal_governance.t
  -> authorize:(unit -> (unit, Agent_protocol.Error.t) result)
  -> authorize_restoration:
       (Agent_protocol.Id.Session.t -> (unit, Agent_protocol.Error.t) result)
  -> f:(prepared -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Capture and bind the same request after resource preparation. *)
val bind_current : prepared -> (bound, Agent_protocol.Error.t) result

(** Evaluate the pinned vector in policy order with current original and final
    governance and the exact linked delegation reference. *)
val evaluate
  :  bound
  -> reference:Agent_store.Delegation_store.Reference.t
  -> (Chat_response.Moderation.Tool_moderation.t option, Agent_protocol.Error.t) result
