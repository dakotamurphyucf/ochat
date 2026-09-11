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

val apply : Session_state.t -> t -> (Session_state.t, Agent_protocol.Error.t) result
