open! Core
module S = Session_state
module O = Goal_policy_ordered

let of_state (t : S.t) =
  O.Projection.create
    ~session_id:t.identity.session_id
    ~session_revision:t.counters.revision
    ~catalog:t.goal_catalog
    ~accounts:t.goal_accounts
    ~ledger:t.inference_ledger
    ~descendants:t.goal_descendants
    ~children:t.child_contributions
    ~causal:t.goal_policy_contributions
    ~canonical:t.policy_canonical_usage
;;

let begin_ state =
  let%bind.Result projection = of_state state in
  O.begin_ projection
;;
