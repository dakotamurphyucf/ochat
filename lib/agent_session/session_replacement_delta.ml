open! Core

type t =
  | Direct of Session_state.t
  | Pending of Session_delta.t * Session_state.t

let classify = function
  | Session_delta.Created state -> Some (Direct state)
  | Batch [ (Pending_inputs_changed _ as pending); Created state ] ->
    Some (Pending (pending, state))
  | Batch _
  | Lifecycle_changed _
  | Metadata_changed _
  | Organization_changed _
  | Initial_start_consumed
  | Stop_epoch_changed _
  | Parent_stop_epoch_changed _
  | Workspace_changed _
  | Canonical_entries_appended _
  | Canonical_history_replaced _
  | Authoring_references_forgotten _
  | Authoring_publication_changed _
  | Initial_prompt_count_changed _
  | Deferred_entries_enqueued _
  | Deferred_entries_adopted
  | Pending_inputs_changed _
  | Active_operation_changed _
  | Automatic_turn_budget_enabled _
  | Automatic_turn_pauses_changed _
  | Attachment_added _
  | Attachment_removed _
  | Permission_changed _
  | Grant_changed _
  | Inference_target_captured _
  | Configuration_revision_changed _
  | Inference_target_changed _
  | Model_job_target_captured _
  | Model_job_recipe_target_captured _
  | Model_job_target_restored _
  | Inference_ledger_changed _
  | Job_changed _
  | Schedule_changed _
  | Invocation_changed _
  | Managed_submission_admitted _
  | Managed_submission_changed _
  | Managed_stop_admitted _
  | Invocation_reconciled _
  | Moderator_execution_changed _
  | Moderator_execution_reconciled _
  | Subscription_changed _
  | Subscription_expired _
  | Subscription_cancelled _
  | Delivery_changed _
  | Ingress_changed _
  | Delivery_committed _
  | Delivery_wake_changed _
  | Run_state_changed _
  | Moderator_changed _
  | Shell_changed _
  | History_block_reserved _
  | Compaction_generation_changed _
  | Compaction_archived _
  | History_deleted _
  | History_edited _
  | Owner_lease_generation_changed _
  | Failure_changed _
  | Halt_changed _
  | Reset_generation _ -> None
;;

let with_state t state =
  match t with
  | Direct _ -> Session_delta.Created state
  | Pending (pending, _) -> Session_delta.Batch [ pending; Created state ]
;;

let state = function
  | Direct state | Pending (_, state) -> state
;;
