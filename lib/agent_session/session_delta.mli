open! Core

(** Closed durable changes applied by the session actor and recovery. *)

type t =
  | Batch of t list
  | Created of Session_state.t
  | Lifecycle_changed of Session_state.Lifecycle.t
  | Initial_start_consumed
  | Stop_epoch_changed of int64
  | Parent_stop_epoch_changed of int64
  | Workspace_changed of Workspace_instance.t
  | Canonical_entries_appended of Agent_protocol.History.entry list
  | Canonical_history_replaced of Agent_protocol.History.entry list
  | Authoring_references_forgotten of Agent_protocol.History.Id.t list
  | Authoring_publication_changed of Chat_response.Authoring_publication.context
  (** Trusted model-input policy/context binding, retained for result publication
      recovery without constructing a runtime. It grants no execution authority. *)
  | Initial_prompt_count_changed of int
  | Deferred_entries_enqueued of Agent_protocol.History.entry list
  | Deferred_entries_adopted
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
  | Inference_target_changed of (Inference.Request.Target.t[@sexp.opaque])
  | Model_job_target_captured of Model_job_target.t
  | Model_job_recipe_target_captured of Model_job_target.t
  | Model_job_target_restored of Model_job_target.t
  | Inference_ledger_changed of (Inference_ledger.t[@sexp.opaque])
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
  | Moderator_changed of Jsonaf.t option
  | Shell_changed of Session.Shell_state.t
  | History_block_reserved of int64
  | Compaction_generation_changed of int
  | Compaction_archived of Session_state.Compaction_archive.t
  | Owner_lease_generation_changed of int64
  | Failure_changed of Agent_protocol.Error.t option
  | Halt_changed of string option
  | Reset_generation of int
[@@deriving sexp]

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
