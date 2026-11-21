open! Core
module X = Persistence_codec
module Transaction = Session_state_document.Transaction

module Policy_path = struct
  type t =
    | Capturing
    | Absent_only
end

let check_administration_order
      (original : Session_state.t)
      (current : Session_state.t)
      delta
  =
  let before = original.recovery_rollover in
  let staged = current.recovery_rollover in
  if Option.is_none before && Option.is_none staged
  then Ok ()
  else
    let open Result.Let_syntax in
    let%bind () =
      Recovery_rollover.check_unchanged before ~incoming:staged |> X.document_result
    in
    let exact_order =
      match delta with
      | Session_delta.Created _ | Batch [ Created _ ] -> true
      | Batch [ Pending_inputs_changed (mutation, Some archive, _); Created _ ] ->
        Pending_mutation.retires_for_administration mutation ~kind:archive.kind
      | _ -> false
    in
    if exact_order
    then Ok ()
    else
      Error
        (Document_schema.Error.Invalid_field
           { path = [ "recovery_rollover" ]
           ; reason = "rollover retirement requires the exact administrative transaction"
           })
;;

let prepare previous_document ~delta:transaction ~limits =
  let%bind.Result () =
    Session_delta.validate_rollover_transaction transaction |> X.document_result
  in
  let previous_value = Session_state_document.value previous_document in
  let%bind.Result () =
    Session_delta.validate_rollover_run_work previous_value transaction
    |> X.document_result
  in
  let%bind.Result () =
    Session_delta.validate_successor_dispatch previous_value transaction
    |> X.document_result
  in
  let policy_path =
    if
      Session_delta.requires_policy_tracking
        transaction
        ~previous:previous_value.policy_canonical_usage
    then Policy_path.Capturing
    else Absent_only
  in
  let goal_policy_path =
    if
      Session_delta.requires_goal_policy_tracking
        transaction
        ~previous:previous_value.goal_policy_contributions
        ~canonical:previous_value.policy_canonical_usage
    then Policy_path.Capturing
    else Absent_only
  in
  let apply_context carrier delta step =
    let open Result.Let_syntax in
    let%bind state =
      Session_delta.apply ~limits (Transaction.value carrier) delta |> X.document_result
    in
    let%map carrier = Transaction.observe_descendants carrier step in
    Transaction.with_value carrier state
  in
  let rec apply carrier delta =
    let open Result.Let_syntax in
    let previous = Transaction.value carrier in
    let%bind candidate =
      match delta with
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
      | Goal_catalog_changed incoming -> Transaction.update_goal_catalog carrier incoming
      | Goal_accounts_changed incoming ->
        Transaction.update_goal_accounts carrier incoming
      | Goal_descendants_changed update ->
        Transaction.update_goal_descendants carrier update
      | (Policy_cause_changed _ | Goal_policy_contributions_changed _) as delta ->
        let%map.Result state =
          Session_delta.apply ~limits (Transaction.value carrier) delta
          |> X.document_result
        in
        Transaction.with_value carrier state
      | Child_contributions_changed children as delta ->
        apply_context carrier delta (Ordinary_children children)
      | Active_operation_changed operation as delta ->
        apply_context carrier delta (Active_operation operation)
      | (Invocation_changed invocation | Invocation_reconciled invocation) as delta ->
        apply_context carrier delta (Invocation invocation)
      | Moderator_lifecycle_claimed { receipt = execution; _ } as delta ->
        apply_context carrier delta (Moderator_execution execution)
      | (Moderator_execution_changed execution | Moderator_execution_reconciled execution)
        as delta -> apply_context carrier delta (Moderator_execution execution)
      | Created _ as replacement ->
        let open Result.Let_syntax in
        let%bind state =
          Session_delta.apply ~limits (Transaction.value carrier) replacement
          |> X.document_result
        in
        let%bind carrier =
          if Int.equal previous.identity.generation state.identity.generation
          then Ok carrier
          else (
            let%bind () =
              check_administration_order previous_value previous transaction
            in
            Transaction.retire_administrative_history
              carrier
              ~original:previous_document
              ~next:state
              ~limits)
        in
        let%bind carrier = Transaction.replace_goal_accounting carrier state in
        let%map carrier =
          Transaction.update_canonical_usage
            carrier
            ~ledger:state.inference_ledger
            ~delegated_usage:state.delegated_usage
        in
        Transaction.with_value carrier state
      | ( Inference_ledger_changed _
        | Delegated_inference_ledger_changed _
        | Policy_inference_ledger_changed _
        | Reset_generation _ ) as delta ->
        let open Result.Let_syntax in
        let%bind state =
          Session_delta.apply ~limits (Transaction.value carrier) delta
          |> X.document_result
        in
        let%map carrier =
          Transaction.update_canonical_usage
            carrier
            ~ledger:state.inference_ledger
            ~delegated_usage:state.delegated_usage
        in
        Transaction.with_value carrier state
      | ( Lifecycle_changed _
        | Metadata_changed _
        | Organization_changed _
        | Initial_start_consumed
        | Stop_epoch_changed _
        | Parent_stop_epoch_changed _
        | Workspace_changed _
        | Canonical_entries_appended _
        | Instruction_selection_changed _
        | Recovery_note_changed _
        | Recovery_note_share_changed _
        | Recovery_rollover_changed _
        | Recurrence_catalog_changed _
        | Run_state_changed _
        | Canonical_history_replaced _
        | Recovery_window_expired _
        | Recovery_window_enabled _
        | Recovery_effective_prepared _
        | Recovery_reminder_changed _
        | Recovery_effective_attempt_changed _
        | Authoring_references_forgotten _
        | Authoring_publication_changed _
        | Initial_prompt_count_changed _
        | Deferred_entries_enqueued _
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
        | Job_changed _
        | Schedule_changed _
        | Managed_submission_admitted _
        | Managed_submission_changed _
        | Managed_stop_admitted _
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
        | Halt_changed _ ) as delta ->
        let%bind.Result state =
          Session_delta.apply ~limits (Transaction.value carrier) delta
          |> X.document_result
        in
        Ok (Transaction.with_value carrier state)
      | Recovery_summary_compacted (archive, _, _) as delta ->
        let%bind previous = Transaction.checkpoint carrier ~limits in
        let%bind state =
          Session_delta.apply ~limits (Transaction.value carrier) delta
          |> X.document_result
        in
        let%bind admitted =
          History_retirement.admit_summary previous ~archive ~next:state ~limits
        in
        Transaction.continue_after_history carrier ~admitted ~next:state ~limits
      | Recovery_rollover_committed change as delta ->
        let%bind basis = Transaction.checkpoint carrier ~limits in
        let%bind state =
          Session_delta.apply ~limits previous delta |> X.document_result
        in
        (* The archive and history carrier remain pre-cut with respect to the Turn.
           Advance canonical accounting only after admitting that exact transfer. *)
        let next =
          { state with
            inference_ledger = previous.inference_ledger
          ; delegated_usage = previous.delegated_usage
          }
        in
        let%bind admitted =
          History_retirement.admit_rollover basis change ~next ~limits
        in
        let%bind candidate =
          Transaction.continue_after_history carrier ~admitted ~next ~limits
        in
        let%bind candidate =
          if phys_equal state.inference_ledger previous.inference_ledger
          then Ok candidate
          else
            Transaction.update_canonical_usage
              candidate
              ~ledger:state.inference_ledger
              ~delegated_usage:state.delegated_usage
        in
        (match Recovery_rollover.Commit.predecessor change with
         | None -> Ok candidate
         | Some _ -> Transaction.observe_descendants candidate (Active_operation None))
      | (History_deleted _ | History_edited _) as delta ->
        let open Result.Let_syntax in
        let%bind previous = Transaction.checkpoint carrier ~limits in
        let%bind state =
          Session_delta.apply ~limits (Transaction.value carrier) delta
          |> X.document_result
        in
        let%bind admitted =
          History_retirement.admit previous ~delta ~next:state ~limits
        in
        Transaction.continue_after_history carrier ~admitted ~next:state ~limits
    in
    let%bind candidate =
      match delta with
      | Batch _ -> Ok candidate
      | leaf ->
        (match policy_path with
         | Absent_only ->
           if Option.is_none (Transaction.value candidate).policy_canonical_usage
           then Ok candidate
           else
             Error
               (Document_schema.Error.Invalid_field
                  { path = [ "policy_canonical_usage" ]
                  ; reason = "absence-only native leaf initialized policy accounting"
                  })
         | Capturing ->
           let%bind step =
             Session_delta.policy_usage_step
               leaf
               ~previous
               ~next:(Transaction.value candidate)
             |> X.document_result
           in
           Option.value_map
             step
             ~default:(Ok candidate)
             ~f:(Transaction.advance_policy_usage candidate))
    in
    match goal_policy_path with
    | Absent_only ->
      if
        Option.is_none (Transaction.value candidate).goal_policy_contributions
        && Option.is_none (Transaction.value candidate).policy_canonical_usage
      then Ok candidate
      else
        Error
          (Document_schema.Error.Invalid_field
             { path = [ "goal_policy_contributions" ]
             ; reason = "absence-only leaf became positive"
             })
    | Capturing ->
      (match delta with
       | Batch _ -> Ok candidate
       | Created _ | Reset_generation _ ->
         Transaction.observe_goal_policy_replacement candidate
       | Goal_catalog_changed _ -> Transaction.observe_goal_policy_catalog candidate
       | Goal_accounts_changed _ -> Transaction.observe_goal_policy_accounts candidate
       | Child_contributions_changed _ ->
         Transaction.observe_goal_policy_children candidate
       | Goal_descendants_changed update ->
         Transaction.observe_goal_policy_descendants candidate update
       | Goal_policy_contributions_changed update ->
         Transaction.observe_goal_policy_update candidate update
       | Policy_cause_changed update ->
         Transaction.observe_goal_policy_metadata candidate update
       | Policy_inference_ledger_changed (_, _, update) ->
         Transaction.observe_goal_policy_canonical
           candidate
           ~before:previous.inference_ledger
           ~policy:(Some update)
       | Inference_ledger_changed _ | Delegated_inference_ledger_changed _
       | Recovery_rollover_committed { retired_at = Some _; _ } ->
         Transaction.observe_goal_policy_canonical
           candidate
           ~before:previous.inference_ledger
           ~policy:None
       | Lifecycle_changed _
       | Metadata_changed _
       | Organization_changed _
       | Initial_start_consumed
       | Stop_epoch_changed _
       | Parent_stop_epoch_changed _
       | Workspace_changed _
       | Canonical_entries_appended _
       | Canonical_history_replaced _
       | Recovery_window_expired _
       | Recovery_window_enabled _
       | Recovery_effective_prepared _
       | Recovery_reminder_changed _
       | Recovery_effective_attempt_changed _
       | Recovery_summary_compacted _
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
       | Job_changed _
       | Schedule_changed _
       | Invocation_changed _
       | Managed_submission_admitted _
       | Managed_submission_changed _
       | Managed_stop_admitted _
       | Invocation_reconciled _
       | Moderator_execution_changed _
       | Moderator_lifecycle_claimed _
       | Moderator_execution_reconciled _
       | Subscription_changed _
       | Subscription_expired _
       | Subscription_cancelled _
       | Delivery_changed _
       | Ingress_changed _
       | Delivery_committed _
       | Delivery_wake_changed _
       | Instruction_selection_changed _
       | Recovery_note_changed _
       | Recovery_note_share_changed _
       | Recovery_rollover_changed _
       | Recovery_rollover_committed _
       | Recurrence_catalog_changed _
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
       | Halt_changed _ -> Ok candidate)
  in
  let open Result.Let_syntax in
  let%bind previous =
    match policy_path, goal_policy_path with
    | Capturing, (Capturing | Absent_only) -> Transaction.begin_ previous_document ~limits
    | Absent_only, Capturing -> Transaction.begin_absent_policy previous_document ~limits
    | Absent_only, Absent_only ->
      Transaction.begin_absent_accounting previous_document ~limits
  in
  let%bind candidate = apply previous transaction in
  let current = Transaction.value candidate in
  let%bind () =
    Queue_provenance_validation.validate_transition
      ~previous:(Session_state.queue_provenance previous_value)
      (Session_state.queue_provenance current)
    |> X.document_result
  in
  let%bind () =
    Queue_provenance_validation.validate (Session_state.queue_provenance current)
    |> X.document_result
  in
  let%bind () =
    Recovery_effective_receipts.check_transaction ~previous:previous_value ~next:current
    |> X.document_result
  in
  let%map () =
    Goal_run_evidence.validate_transition
      ~previous_runs:previous_value.run_state
      ~catalog:current.goal_catalog
      ~runs:current.run_state
    |> X.document_result
  in
  candidate
;;

let admit_encoded previous ~delta ~next ~limits =
  let%bind.Result candidate = prepare previous ~delta ~limits in
  Transaction.finish_encoded candidate ~next ~limits
;;

let admit_prefix_encoded previous ~delta ~next ~limits =
  let%bind.Result candidate = prepare previous ~delta ~limits in
  Transaction.checkpoint_encoded candidate ~next ~limits
;;

let admit previous ~delta ~next ~limits =
  Result.map
    (admit_encoded previous ~delta ~next ~limits)
    ~f:Session_state_document.Admitted.state_document
;;
