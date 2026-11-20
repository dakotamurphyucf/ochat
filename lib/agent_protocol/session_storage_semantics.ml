let delegated_usage = "delegation.usage.v1"
let goal_descendants = "session.goal-descendants.v1"
let recurrence_run_origin = "recurrence.run-origin.v1"
let policy_canonical_usage = "session.policy-canonical-usage.v1"
let goal_policy_contributions = "session.goal-policy-contributions.v1"
let ingress_provenance = "runtime.ingress-provenance.v1"
let queue_occurrence_producer = "runtime.queue-occurrence-producer.v1"
let moderator_lifecycle = "session.moderator-lifecycle.v1"
let recovery_windows = "session.recovery-windows.v1"
let recovery_effective = "session.recovery-effective.v1"
let recovery_effective_archive = "session.recovery-effective-archive.v1"
let recovery_effective_attempt = "session.recovery-effective-attempt.v1"
let recovery_withdrawals = "session.recovery-withdrawals.v1"
let recurrence_controller_goal = "recurrence.controller-goal.v1"
let recovery_notes = "session.recovery-notes.v1"
let recovery_note_shares = "session.recovery-note-shares.v1"
let recovery_rollover = "session.recovery-rollover.v1"
let recovery_rollover_commit = "session.recovery-rollover-commit.v1"

let supported_children =
  [ Schedule_provenance.persistence_semantics
  ; recurrence_run_origin
  ; ingress_provenance
  ; queue_occurrence_producer
  ; Goal_native_resume_intent.persistence_semantics
  ; Goal_native_start_intent.persistence_semantics
  ; delegated_usage
  ; goal_descendants
  ; policy_canonical_usage
  ; goal_policy_contributions
  ; moderator_lifecycle
  ; recovery_windows
  ; recovery_effective
  ; recovery_effective_archive
  ; recovery_effective_attempt
  ; recovery_withdrawals
  ; recurrence_controller_goal
  ; recovery_notes
  ; recovery_note_shares
  ; recovery_rollover
  ; recovery_rollover_commit
  ]
;;
