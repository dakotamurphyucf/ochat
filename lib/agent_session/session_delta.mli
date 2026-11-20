open! Core

(** Closed durable changes applied by the session actor and recovery. *)

type t =
  | Batch of t list
  | Created of Session_state.t
  | Lifecycle_changed of Session_state.Lifecycle.t
  | Metadata_changed of Agent_protocol.Session_metadata.Values.t * int64
  | Organization_changed of Agent_protocol.Session_organization.Values.t * int64
  | Initial_start_consumed
  | Stop_epoch_changed of int64
  | Parent_stop_epoch_changed of int64
  | Workspace_changed of Workspace_instance.t
  | Canonical_entries_appended of Agent_protocol.History.entry list
  | Canonical_history_replaced of Agent_protocol.History.entry list
  | Recovery_window_expired of (Agent_protocol.Recovery_window.Scope.t[@sexp.opaque])
  | Recovery_window_enabled of
      Agent_protocol.Session_ref.t * Agent_protocol.Id.Context_window.t
  | Recovery_effective_prepared of (Recovery_effective_record.t[@sexp.opaque])
  | Recovery_effective_attempt_changed of (Recovery_effective_receipts.t[@sexp.opaque])
  | Recovery_summary_compacted of
      Session_state.Compaction_archive.t
      * Agent_protocol.History.entry list
      * [ `Canonical | `Canonical_and_effective ]
  | Authoring_references_forgotten of Agent_protocol.History.Id.t list
  | Authoring_publication_changed of Chat_response.Authoring_publication.context
  (** Trusted model-input policy/context binding, retained for result publication
      recovery without constructing a runtime. It grants no execution authority. *)
  | Initial_prompt_count_changed of int
  | Deferred_entries_enqueued of Agent_protocol.History.entry list
  | Deferred_entries_adopted
  | Pending_inputs_changed of
      Pending_mutation.t
      * Session_state.Compaction_archive.t option
      * Pending_archive.Reference.t option
  | Active_operation_changed of Agent_protocol.Operation.t option
  | Automatic_turn_budget_enabled of Chat_response.Runtime_semantics.policy
  (** Enable once, or repeat the same policy without resetting accounting.
      New operation admission updates the retained count in the same delta. *)
  | Automatic_turn_pauses_changed of Chat_response.Runtime_semantics.pause_condition list
  (** Host pause/resume preserves every count and ceiling. *)
  | Attachment_added of Agent_protocol.Session.Attachment.t
  | Attachment_removed of Agent_protocol.Id.Attachment.t
  | Permission_changed of Agent_protocol.Permission.t
  | Grant_changed of Agent_protocol.Grant.t
  | Inference_target_captured of (Inference.Request.Target.t[@sexp.opaque])
  | Configuration_revision_changed of int64
  | Inference_target_changed of (Inference.Request.Target.t[@sexp.opaque])
  | Model_job_target_captured of Model_job_target.t
  | Model_job_recipe_target_captured of Model_job_target.t
  | Model_job_target_restored of Model_job_target.t
  | Inference_ledger_changed of (Inference_ledger.t[@sexp.opaque])
  | Delegated_inference_ledger_changed of
      (Inference_ledger.t[@sexp.opaque]) * (Delegated_usage.t[@sexp.opaque])
  | Policy_cause_changed of (Policy_canonical_usage.Update.t[@sexp.opaque])
  | Goal_policy_contributions_changed of
      (Goal_policy_contributions.Update.t[@sexp.opaque])
  | Policy_inference_ledger_changed of
      (Inference_ledger.t[@sexp.opaque])
      * (Delegated_usage.t option[@sexp.opaque])
      * (Policy_canonical_usage.Update.t[@sexp.opaque])
  | Child_contributions_changed of (Child_contributions.t[@sexp.opaque])
  | Goal_descendants_changed of (Goal_descendants.Update.t[@sexp.opaque])
  | Job_changed of Agent_protocol.Job.t
  | Schedule_changed of Agent_protocol.Schedule.t
  | Invocation_changed of Agent_protocol.Invocation.t
  | Managed_submission_admitted of Managed_submission.t
  | Managed_submission_changed of Managed_submission.t
  | Managed_stop_admitted of Managed_stop.t
  | Invocation_reconciled of Agent_protocol.Invocation.t
  (** Recovery-only terminalization/publication of an existing invocation,
        including older generations. Cannot admit, dispatch or create outcomes. *)
  | Moderator_execution_changed of Agent_protocol.Moderator_execution.t
  | Moderator_lifecycle_claimed of
      { receipt : Agent_protocol.Moderator_execution.t
      ; capture : Moderator_lifecycle.t
      }
  | Moderator_execution_reconciled of Agent_protocol.Moderator_execution.t
  (** Recovery-only interruption of an existing execution or discard of retained
      runtime intent. Cannot create receipts or successful outcomes. *)
  | Subscription_changed of Agent_protocol.Subscription.t
  | Subscription_expired of Agent_protocol.Subscription.t
  (** Host expiry of an existing subscription, including retained older
      generations. Cannot create a record, change its context or record success. *)
  | Subscription_cancelled of Agent_protocol.Subscription.t
  (** Host cancellation of an existing source-owned subscription, including
      historical generations. Cannot admit new work or rewrite terminal results. *)
  | Delivery_changed of Agent_protocol.Delivery.t
  | Ingress_changed of External_ingress.t
  | Delivery_committed of Agent_protocol.Delivery.t * Agent_protocol.History.entry
  | Delivery_wake_changed of Agent_protocol.Delivery.t
  (** Settle an existing committed wake without reinserting history. Acceptance
      requires the matching active Turn, generation and running lifecycle in this checkpoint.
      Repeating the same disposition remains valid after that turn has ended. *)
  | Goal_catalog_changed of Goal_catalog.t
  | Goal_accounts_changed of (Goal_accounts.t[@sexp.opaque])
  | Instruction_selection_changed of Instruction_selection.t
  | Recovery_note_changed of (Recovery_note_change.t[@sexp.opaque])
  | Recovery_note_share_changed of (Recovery_note_share_change.t[@sexp.opaque])
  | Recovery_rollover_changed of (Recovery_rollover.Change.t[@sexp.opaque])
  | Recovery_rollover_committed of (Recovery_rollover.Commit.t[@sexp.opaque])
  | Recurrence_catalog_changed of Recurrence_catalog.t
  | Run_state_changed of Run_state.t
  | Moderator_changed of Jsonaf.t option
  | Shell_changed of Session.Shell_state.t
  | History_block_reserved of int64
  | Compaction_generation_changed of int
  | Compaction_archived of Session_state.Compaction_archive.t
  | History_deleted of Agent_protocol.History.Id.t * Session_state.Compaction_archive.t
  | History_edited of
      (Agent_protocol.History_edit.t[@sexp.opaque]) * Session_state.Compaction_archive.t
  | Owner_lease_generation_changed of int64
  | Failure_changed of Agent_protocol.Error.t option
  | Halt_changed of string option
  | Reset_generation of int
[@@deriving sexp]

(** Shared representation selection for actual inference/host-turn writers.
    Applying an ordinary leaf to an already partitioned child is rejected. *)
val inference_ledger_update
  :  ?policy_update:Policy_canonical_usage.Update.t
  -> Inference_ledger.t
  -> delegated_usage:Delegated_usage.t option
  -> t

(** Apply under finite durable bounds; direct native callers default to the
    shared 64MiB durable profile. Document owners pass their configured limits.
    Target changes require prior host policy approval; captured job bindings
    belong in the same atomic Batch as their actual Model_call Job_changed.
    Restored unresolved bindings carry migration evidence, never authority. *)
val apply
  :  ?limits:Document_schema.Limits.t
  -> Session_state.t
  -> t
  -> (Session_state.t, Agent_protocol.Error.t) result

val validate_rollover_transaction : t -> (unit, Agent_protocol.Error.t) result

val rollover_retained_history
  :  Session_state.t
  -> Recovery_rollover.Commit.t
  -> (Agent_protocol.History.entry list, Agent_protocol.Error.t) result

(** Live host admission only: append an immutable source binding next to every
    genuinely new Model_call Job_changed using the then-selected captured source.
    The resulting delta must be committed atomically. Existing/restored jobs keep
    their own selection; unresolved sources reject. Recovery calls [apply] on the
    exact explicit stored changes and never calls this preparation helper. *)
val capture_new_model_jobs
  :  ?limits:Document_schema.Limits.t
  -> Session_state.t
  -> t
  -> (t, Agent_protocol.Error.t) Result.t

(** Existing bounded default for native transition admission. Durable owners may
    impose smaller limits; the actual whole-state publication validates again. *)
val native_limits : Document_schema.Limits.t

(** Ordered structural evidence only. Traverses
    actual Batch ordering; no execution, authorization, ledger construction or
    second state-application pipeline. Called after ordinary apply validated the
    exact native delta. All non-account/catalog leaves are exhaustive no-ops for
    this narrow evidence collection; final actual next ledger still bounds it. *)
val validate_goal_accounts
  :  t
  -> previous:Session_state.t
  -> next:Session_state.t
  -> (unit, Agent_protocol.Error.t) result

(** Validate descendant updates against each actual ordered canonical/catalog
    edge, then check the complete final projection. Structural [apply] alone is
    not this admission gate. *)
val validate_goal_descendants
  :  t
  -> previous:Session_state.t
  -> next:Session_state.t
  -> (unit, Agent_protocol.Error.t) result

(** At the actual publication boundary, each newly recorded LOCAL Included
    receipt must name the published revision. Remote Reserve/Import revisions
    are independent. This supplements the original+1 ordered-edge check. *)
val validate_descendant_publication
  :  t
  -> revision:int64
  -> (unit, Agent_protocol.Error.t) result

(** Exhaustive lookahead over the actual whole delta, before any ordered leaf.
    Positive previous/Created carriers and all policy leaves select capture;
    absent ordinary work cannot acquire a new causal birth-window limit. *)
val requires_policy_tracking : t -> previous:Policy_canonical_usage.t option -> bool

(** Shared finite per-leaf policy projection. Call only after applying the exact
    native leaf to obtain [next]; Batch is deliberately rejected. No permission
    or birth is inferred from an endpoint. Source rotation carries only the actual
    new current source and cannot change work/native rows in the cursor. *)
val policy_usage_step
  :  t
  -> previous:Session_state.t
  -> next:Session_state.t
  -> (Policy_canonical_usage.Ordered.step option, Agent_protocol.Error.t) result

(** Actual ordered canonical/subset checks supplement existing native validators.
    Replays the finite native leaves; no IO, execution or authority construction. *)
val validate_policy_usage
  :  ?limits:Document_schema.Limits.t
  -> t
  -> previous:Session_state.t
  -> next:Session_state.t
  -> revision:int64
  -> (unit, Agent_protocol.Error.t) result

(** Independent exhaustive whole-delta selection for optional Goal-book policy.
    No positive carrier can be introduced through the absence-only fast path. *)
val requires_goal_policy_tracking
  :  t
  -> previous:Goal_policy_contributions.t option
  -> canonical:Policy_canonical_usage.t option
  -> bool

(** Actual ordered prefix capacity/creation checks for optional Goal books.
    Existing native, descendant and canonical-subset validators remain required.
    Whole-delta absence bypasses only this optional feature's new work. *)
val validate_goal_policy
  :  ?limits:Document_schema.Limits.t
  -> t
  -> previous:Session_state.t
  -> next:Session_state.t
  -> (unit, Agent_protocol.Error.t) result
