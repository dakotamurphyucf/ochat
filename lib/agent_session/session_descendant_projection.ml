open! Core

let of_state (state : Session_state.t) : Goal_descendants_ordered.Projection.t =
  { ledger = state.inference_ledger
  ; accounting = state.goal_descendants
  ; children = state.child_contributions
  ; catalog = state.goal_catalog
  ; accounts = state.goal_accounts
  ; delegation = state.spec.delegation
  ; active_operation = state.active_operation
  ; invocations = state.invocations
  ; moderator_executions = state.moderator_executions
  }
;;

let begin_ (state : Session_state.t) =
  Goal_descendants_ordered.begin_
    ~session_id:state.identity.session_id
    ~session_revision:state.counters.revision
    (of_state state)
;;
