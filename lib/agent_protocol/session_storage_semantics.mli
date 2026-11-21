(** Explicit feature names understood by stored session state/delta children.
    Store transaction and snapshot envelopes do not support these semantics on
    themselves. These constants convey no runtime or admission authority. *)
val delegated_usage : string

val goal_descendants : string
val policy_canonical_usage : string
val goal_policy_contributions : string
val recurrence_run_origin : string
val ingress_provenance : string
val queue_occurrence_producer : string
val supported_children : string list

(** Captured current-installation lifecycle pointer; no execution authority. *)
val moderator_lifecycle : string

(** Typed source-bound local recovery ledger, independent of ordinary history. *)
val recovery_windows : string

val recovery_installed_originals : string
val recovery_effective : string
val recovery_effective_archive : string
val recovery_effective_attempt : string

(** Exact membership survives withdrawal. Older reducers must reject even a journal-only edit or deletion. *)
val recovery_withdrawals : string

(** Durable immutable Goal association on a captured recurrence definition. *)
val recurrence_controller_goal : string

val recovery_notes : string
val recovery_note_shares : string
val recovery_reminders : string
val recovery_rollover : string
val recovery_rollover_commit : string
val recovery_rollover_batch : string
val recovery_rollover_continuation : string
val recovery_rollover_hooks : string
val recovery_checkpoint_opportunity : string
val recovery_authored_checkpoint : string
val recovery_capacity_rollover : string
val recovery_rollover_turn_retirement : string
