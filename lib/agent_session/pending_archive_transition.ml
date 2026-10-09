open! Core
module P = Agent_protocol

let collect state ~delta ~limits =
  let open Result.Let_syntax in
  let rec prepare_pending_archives (state, archives) = function
    | Session_delta.Batch changes ->
      List.fold_result changes ~init:(state, archives) ~f:prepare_pending_archives
    | Session_delta.Pending_inputs_changed (mutation, _, reference) as delta ->
      let%bind plan = Pending_mutation.prepare mutation state ~limits in
      let%bind archive =
        match Pending_plan.expired_dispositions plan, reference with
        | [], None -> Ok None
        | [], Some _ ->
          Error (P.Error.invalid_request "unexpected pending expiry archive")
        | _ :: _, None -> Error (P.Error.invalid_request "missing pending expiry archive")
        | records, Some reference ->
          let%bind archive =
            Pending_archive.create
              state
              ~operation_id:(Pending_archive.Reference.operation_id reference)
              ~pending_revision:(Pending_plan.revision plan)
              ~records
              ~limits
          in
          if Pending_archive.Reference.equal reference (Pending_archive.reference archive)
          then Ok (Some archive)
          else
            Error
              (P.Error.invalid_request
                 "pending expiry archive differs from exact retired custody")
      in
      let%map state = Session_delta.apply ~limits state delta in
      ( state
      , Option.value_map archive ~default:archives ~f:(fun archive -> archive :: archives)
      )
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
      | Deferred_entries_adopted
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
      | Reset_generation _
      | History_deleted _
      | History_edited _ ) as delta ->
      let%map state = Session_delta.apply ~limits state delta in
      state, archives
  in
  let%map _, archives = prepare_pending_archives (state, []) delta in
  List.rev archives
;;
