let delegated_usage = "delegation.usage.v1"
let goal_descendants = "session.goal-descendants.v1"
let recurrence_run_origin = "recurrence.run-origin.v1"
let policy_canonical_usage = "session.policy-canonical-usage.v1"
let goal_policy_contributions = "session.goal-policy-contributions.v1"
let ingress_provenance = "runtime.ingress-provenance.v1"
let queue_occurrence_producer = "runtime.queue-occurrence-producer.v1"
let moderator_lifecycle = "session.moderator-lifecycle.v1"
let recovery_windows = "session.recovery-windows.v1"
let recovery_installed_originals = "session.recovery-installed-originals.v1"
let recovery_effective = "session.recovery-effective.v1"
let recovery_effective_archive = "session.recovery-effective-archive.v1"
let recovery_effective_attempt = "session.recovery-effective-attempt.v1"
let recovery_withdrawals = "session.recovery-withdrawals.v1"
let recurrence_controller_goal = "recurrence.controller-goal.v1"
let recovery_notes = "session.recovery-notes.v1"
let recovery_note_shares = "session.recovery-note-shares.v1"
let recovery_reminders = "session.recovery-reminders.v1"
let recovery_rollover = "session.recovery-rollover.v1"
let recovery_rollover_commit = "session.recovery-rollover-commit.v1"
let recovery_authored_checkpoint = "session.recovery-authored-checkpoint.v1"
let recovery_rollover_batch = "session.recovery-rollover-batch.v1"
let recovery_rollover_continuation = "session.recovery-rollover-continuation.v1"
let recovery_capacity_rollover = "session.recovery-capacity-rollover.v1"
let recovery_rollover_turn_retirement = "session.recovery-rollover-turn-retirement.v1"
let recovery_rollover_hooks = "session.recovery-rollover-hooks.v1"
let recovery_checkpoint_opportunity = "session.recovery-checkpoint-opportunity.v1"

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
  ; recovery_installed_originals
  ; recovery_effective
  ; recovery_effective_archive
  ; recovery_effective_attempt
  ; recovery_withdrawals
  ; recurrence_controller_goal
  ; recovery_notes
  ; recovery_note_shares
  ; recovery_rollover
  ; recovery_reminders
  ; recovery_rollover_commit
  ; recovery_rollover_batch
  ; recovery_rollover_continuation
  ; recovery_rollover_hooks
  ; recovery_capacity_rollover
  ; recovery_rollover_turn_retirement
  ; recovery_checkpoint_opportunity
  ; recovery_authored_checkpoint
  ]
;;
