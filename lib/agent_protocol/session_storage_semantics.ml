let delegated_usage = "delegation.usage.v1"
let goal_descendants = "session.goal-descendants.v1"
let recurrence_run_origin = "recurrence.run-origin.v1"

let supported_children =
  [ Schedule_provenance.persistence_semantics
  ; recurrence_run_origin
  ; Goal_native_resume_intent.persistence_semantics
  ; Goal_native_start_intent.persistence_semantics
  ; delegated_usage
  ; goal_descendants
  ]
;;
