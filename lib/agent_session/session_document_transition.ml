open! Core
module X = Persistence_codec
module Transaction = Session_state_document.Transaction

let admit_encoded previous ~delta ~(next : Session_state.t) ~limits =
  let rec apply carrier = function
    | Session_delta.Batch changes -> List.fold_result changes ~init:carrier ~f:apply
    | Pending_inputs_changed (mutation, archive, expiry_archive) as delta ->
      let open Result.Let_syntax in
      let current = Transaction.value carrier in
      let%bind plan =
        Pending_mutation.prepare mutation current ~limits |> X.document_result
      in
      let%bind candidate =
        Transaction.transfer_pending carrier ~plan ~archive ~expiry_archive ~limits
      in
      let%map state = Session_delta.apply ~limits current delta |> X.document_result in
      Transaction.with_value candidate state
    | Deferred_entries_adopted ->
      let open Result.Let_syntax in
      let%bind plan =
        Pending_plan.legacy_adoption (Transaction.value carrier) ~limits
        |> X.document_result
      in
      Transaction.transfer_pending
        carrier
        ~plan
        ~archive:None
        ~expiry_archive:None
        ~limits
    | ( Created _
      | Lifecycle_changed _
      | Metadata_changed _
      | Organization_changed _
      | Initial_start_consumed
      | Stop_epoch_changed _
      | Parent_stop_epoch_changed _
      | Workspace_changed _
      | Canonical_entries_appended _
      | Run_state_changed _
      | Canonical_history_replaced _
      | Authoring_references_forgotten _
      | Authoring_publication_changed _
      | Initial_prompt_count_changed _
      | Deferred_entries_enqueued _
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
      | Moderator_changed _
      | Shell_changed _
      | History_block_reserved _
      | Compaction_generation_changed _
      | Compaction_archived _
      | Owner_lease_generation_changed _
      | Failure_changed _
      | Halt_changed _
      | Reset_generation _ ) as delta ->
      let%bind.Result state =
        Session_delta.apply ~limits (Transaction.value carrier) delta |> X.document_result
      in
      Ok (Transaction.with_value carrier state)
    | (History_deleted _ | History_edited _) as delta ->
      let open Result.Let_syntax in
      let%bind previous = Transaction.checkpoint carrier ~limits in
      let%bind state =
        Session_delta.apply ~limits (Transaction.value carrier) delta |> X.document_result
      in
      let%bind admitted = History_retirement.admit previous ~delta ~next:state ~limits in
      Transaction.begin_ admitted ~limits
  in
  let open Result.Let_syntax in
  let%bind previous = Transaction.begin_ previous ~limits in
  let%bind candidate = apply previous delta in
  Transaction.finish_encoded candidate ~next ~limits
;;

let admit previous ~delta ~next ~limits =
  Result.map
    (admit_encoded previous ~delta ~next ~limits)
    ~f:Session_state_document.Admitted.state_document
;;
