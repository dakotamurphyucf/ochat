open! Core

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
  | Recovery_window_enabled of
      Agent_protocol.Session_ref.t * Agent_protocol.Id.Context_window.t
  | Recovery_summary_compacted of
      Session_state.Compaction_archive.t * Agent_protocol.History.entry list
  | Authoring_references_forgotten of Agent_protocol.History.Id.t list
  | Authoring_publication_changed of Chat_response.Authoring_publication.context
  | Initial_prompt_count_changed of int
  | Deferred_entries_enqueued of Agent_protocol.History.entry list
  | Deferred_entries_adopted
  | Pending_inputs_changed of
      Pending_mutation.t
      * Session_state.Compaction_archive.t option
      * Pending_archive.Reference.t option
  | Active_operation_changed of Agent_protocol.Operation.t option
  | Automatic_turn_budget_enabled of Chat_response.Runtime_semantics.policy
  | Automatic_turn_pauses_changed of Chat_response.Runtime_semantics.pause_condition list
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
  | Moderator_execution_changed of Agent_protocol.Moderator_execution.t
  | Moderator_lifecycle_claimed of
      { receipt : Agent_protocol.Moderator_execution.t
      ; capture : Moderator_lifecycle.t
      }
  | Moderator_execution_reconciled of Agent_protocol.Moderator_execution.t
  | Subscription_changed of Agent_protocol.Subscription.t
  | Subscription_expired of Agent_protocol.Subscription.t
  | Subscription_cancelled of Agent_protocol.Subscription.t
  | Delivery_changed of Agent_protocol.Delivery.t
  | Ingress_changed of External_ingress.t
  | Delivery_committed of Agent_protocol.Delivery.t * Agent_protocol.History.entry
  | Delivery_wake_changed of Agent_protocol.Delivery.t
  | Goal_catalog_changed of Goal_catalog.t
  | Goal_accounts_changed of (Goal_accounts.t[@sexp.opaque])
  | Instruction_selection_changed of Instruction_selection.t
  | Recovery_note_changed of (Recovery_note_change.t[@sexp.opaque])
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

let replace_by compare_id id value values ~id_of =
  value :: List.filter values ~f:(fun candidate -> compare_id (id_of candidate) id <> 0)
;;

let nonexecuting_intent_transition
      (previous : Agent_protocol.Invocation.t)
      (next : Agent_protocol.Invocation.t)
  =
  let module I = Agent_protocol.Invocation in
  let observation =
    match previous.observation, next.observation with
    | before, after when Option.equal I.equal_observation before after -> true
    | Some { status = Observing; _ }, Some { status = Observation_failed _; _ } -> true
    | ( Some
          { status = Observed
          ; follow_up = Some (Pending_follow_up _ | Compaction_accepted_follow_up _)
          ; _
          }
      , Some { status = Observed; follow_up = Some (Discarded_follow_up _); _ } ) -> true
    | _ -> false
  in
  let handler =
    match previous.handler_intent, next.handler_intent with
    | before, after when Option.equal I.equal_handler_intent before after -> true
    | ( Some { follow_up = Pending_follow_up _ | Compaction_accepted_follow_up _; _ }
      , Some { follow_up = Discarded_follow_up _; _ } ) -> true
    | _ -> false
  in
  observation && handler
;;

let retained_authoring index =
  let module R = Chat_response.Authoring_reference_index in
  match List.is_empty (R.receipts index) && not (R.truncated index) with
  | true -> None
  | false -> Some (R.to_json index)
;;

let remember_authoring state batches =
  let module R = Chat_response.Authoring_reference_index in
  let open Result.Let_syntax in
  let%bind index = Session_state.authoring_references state in
  let%map index =
    List.fold_result batches ~init:index ~f:(fun index history ->
      R.remember index ~history)
  in
  retained_authoring index
;;

let native_limits =
  Persistence_codec.limits ~max_bytes:(64 * 1024 * 1024)
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum (Document_schema.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let inference_error error =
  Agent_protocol.Error.invalid_request
    (Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))
;;

let retire_pending_dispositions state ~history_ids ~limits =
  let retired = Hash_set.of_list (module Agent_protocol.History.Id) history_ids in
  let%map.Result dispositions =
    List.map state.Session_state.conversation.pending_dispositions ~f:(fun document ->
      let id =
        Pending_disposition.history_id (Pending_disposition_document.value document)
      in
      if Hash_set.mem retired id
      then
        Pending_disposition_document.retire_canonical document ~limits
        |> Result.map_error ~f:(fun error ->
          Agent_protocol.Error.invalid_request
            (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error)))
      else Ok document)
    |> Result.all
  in
  { state with
    conversation = { state.conversation with pending_dispositions = dispositions }
  }
;;

let inference_ledger_update ?policy_update ledger ~delegated_usage =
  match policy_update, delegated_usage with
  | Some update, usage -> Policy_inference_ledger_changed (ledger, usage, update)
  | None, None -> Inference_ledger_changed ledger
  | None, Some usage -> Delegated_inference_ledger_changed (ledger, usage)
;;

let rec contains_policy_usage = function
  | Batch changes -> List.exists changes ~f:contains_policy_usage
  | Created state -> Option.is_some state.Session_state.policy_canonical_usage
  | Policy_cause_changed _ | Policy_inference_ledger_changed _ -> true
  | Lifecycle_changed _
  | Metadata_changed _
  | Organization_changed _
  | Initial_start_consumed
  | Stop_epoch_changed _
  | Parent_stop_epoch_changed _
  | Workspace_changed _
  | Canonical_entries_appended _
  | Canonical_history_replaced _
  | Recovery_window_enabled _
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
  | Inference_ledger_changed _
  | Delegated_inference_ledger_changed _
  | Goal_policy_contributions_changed _
  | Child_contributions_changed _
  | Goal_descendants_changed _
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
  | Goal_catalog_changed _
  | Goal_accounts_changed _
  | Instruction_selection_changed _
  | Recovery_note_changed _
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
  | Halt_changed _
  | Reset_generation _ -> false
;;

let rec contains_goal_policy = function
  | Batch changes -> List.exists changes ~f:contains_goal_policy
  | Created state ->
    Option.is_some state.Session_state.policy_canonical_usage
    || Option.is_some state.goal_policy_contributions
  | Policy_cause_changed _
  | Policy_inference_ledger_changed _
  | Goal_policy_contributions_changed _ -> true
  | Lifecycle_changed _
  | Metadata_changed _
  | Organization_changed _
  | Initial_start_consumed
  | Stop_epoch_changed _
  | Parent_stop_epoch_changed _
  | Workspace_changed _
  | Canonical_entries_appended _
  | Canonical_history_replaced _
  | Recovery_window_enabled _
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
  | Inference_ledger_changed _
  | Delegated_inference_ledger_changed _
  | Child_contributions_changed _
  | Goal_descendants_changed _
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
  | Goal_catalog_changed _
  | Goal_accounts_changed _
  | Instruction_selection_changed _
  | Recovery_note_changed _
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
  | Halt_changed _
  | Reset_generation _ -> false
;;

let requires_policy_tracking delta ~previous =
  Option.is_some previous || contains_policy_usage delta
;;

let requires_goal_policy_tracking delta ~previous ~canonical =
  Option.is_some previous || Option.is_some canonical || contains_goal_policy delta
;;

let with_canonical_history (state : Session_state.t) canonical_history ~recovery_windows =
  (* Observe the outgoing history as well. This bootstraps legacy authoring
     references without scanning archives on each ordinary append. *)
  let%map.Result authoring_reference_index =
    remember_authoring state [ state.conversation.canonical_history; canonical_history ]
  in
  { state with
    conversation =
      { state.conversation with canonical_history; authoring_reference_index }
  ; recovery_windows
  }
;;

let replace_canonical_history state canonical_history ~retired =
  let%bind.Result recovery_windows =
    Recovery_window_capture.replace state ~history:canonical_history ~retired
  in
  with_canonical_history state canonical_history ~recovery_windows
;;

let attach_archive (state : Session_state.t) archive =
  { state with
    conversation =
      { state.conversation with
        compaction_archives = archive :: state.conversation.compaction_archives
      }
  }
;;

let rec apply ?(limits = native_limits) (state : Session_state.t) = function
  | Batch deltas -> List.fold_result deltas ~init:state ~f:(apply ~limits)
  | Created created ->
    let open Result.Let_syntax in
    let%bind () =
      Session_state.validate_moderator_lifecycle_update
        ~previous_runs:state.run_state
        ~previous_generation:state.identity.generation
        ~previous_executions:state.moderator_executions
        created
    in
    let%bind created = Session_state.upgrade_schema created in
    let%bind () =
      Recovery_window_capture.check_replacement_archive state ~incoming:created
    in
    let%bind recovery_windows =
      Recovery_window_capture.replacement state ~incoming:created
    in
    let created = { created with recovery_windows } in
    let%bind () =
      Inference_ledger.validate_update
        state.inference_ledger
        ~incoming:created.inference_ledger
      |> Result.map_error ~f:(fun error ->
        Agent_protocol.Error.invalid_request
          (Sexp.to_string_hum (Inference_ledger.Error.sexp_of_t error)))
    in
    let%bind () =
      Recovery_note_session.check_unchanged
        state.recovery_notes
        ~incoming:created.recovery_notes
    in
    let%bind () =
      match state.instruction_selection, created.instruction_selection with
      | None, None -> Ok ()
      | None, Some incoming ->
        Instruction_selection.validate_update Instruction_selection.empty ~incoming
      | Some previous, Some incoming ->
        Instruction_selection.validate_update previous ~incoming
      | Some _, None ->
        Error
          (Agent_protocol.Error.invalid_request
             "replacement discarded durable instruction evidence")
    in
    let%bind () =
      match state.goal_catalog, created.goal_catalog with
      | None, None -> Ok ()
      | None, Some incoming ->
        Goal_catalog.validate_transition Goal_catalog.empty ~incoming
      | Some previous, Some incoming ->
        Goal_catalog.validate_transition previous ~incoming
      | Some _, None ->
        Error (Agent_protocol.Error.invalid_request "replacement discarded durable goals")
    in
    let%bind () =
      match state.recurrence_catalog, created.recurrence_catalog with
      | None, None -> Ok ()
      | None, Some incoming ->
        Recurrence_catalog.validate_transition Recurrence_catalog.empty ~incoming
      | Some previous, Some incoming ->
        Recurrence_catalog.validate_transition previous ~incoming
      | Some _, None ->
        Error
          (Agent_protocol.Error.invalid_request
             "replacement discarded durable recurrences")
    in
    let%bind () =
      match state.goal_accounts, created.goal_accounts with
      | None, None -> Ok ()
      | None, Some incoming ->
        Goal_accounts.adopt Goal_accounts.empty ~incoming |> Result.map ~f:ignore
      | Some previous, Some incoming ->
        Goal_accounts.adopt previous ~incoming |> Result.map ~f:ignore
      | Some _, None ->
        Error
          (Agent_protocol.Error.invalid_request "replacement discarded goal accounting")
    in
    let%bind () =
      match state.delegated_usage, created.delegated_usage with
      | None, None -> Ok ()
      | Some previous, Some incoming ->
        Delegated_usage.validate_transition
          previous
          ~incoming
          ~before:state.inference_ledger
          ~after:created.inference_ledger
      | None, Some _ ->
        Error
          (Agent_protocol.Error.invalid_request
             "replacement cannot bootstrap historical delegated usage")
      | Some _, None ->
        Error
          (Agent_protocol.Error.invalid_request "replacement discarded delegated usage")
    in
    let%bind child_contributions =
      match state.child_contributions, created.child_contributions with
      | None, None -> Ok None
      | Some previous, Some incoming ->
        Child_contributions.adopt previous ~incoming |> Result.map ~f:Option.some
      | None, Some incoming ->
        (match Child_contributions.inventory incoming with
         | Historical_children_unknown -> Ok (Some incoming)
         | Complete_inventory ->
           Error
             (Agent_protocol.Error.invalid_request
                "replacement cannot prove historical child completeness"))
      | Some _, None ->
        Error
          (Agent_protocol.Error.invalid_request
             "replacement discarded child contributions")
    in
    let%bind () =
      Job_inference_provenance.validate_transition
        ~previous:state.jobs
        ~incoming:created.jobs
    in
    let%bind () =
      Invocation_inference_provenance.validate_transition
        ~previous:state.invocations
        ~incoming:created.invocations
    in
    let%bind () =
      Queue_provenance_validation.validate_event_retention
        ~previous:(Session_state.queue_provenance state)
        (Session_state.queue_provenance created)
    in
    let%bind () =
      match state.run_state, created.run_state with
      | None, _ -> Ok ()
      | Some previous, Some incoming -> Run_state.validate_transition ~previous incoming
      | Some _, None ->
        Error
          (Agent_protocol.Error.invalid_request
             "replacement discarded durable run custody")
    in
    let%bind goal_policy_contributions =
      match state.goal_policy_contributions, created.goal_policy_contributions with
      | None, None -> Ok None
      | Some previous, Some incoming ->
        Goal_policy_contributions.adopt previous ~incoming |> Result.map ~f:Option.some
      | None, Some _ | Some _, None ->
        Error
          (Agent_protocol.Error.invalid_request "replacement changed causal Goal books")
    in
    let%map policy_canonical_usage =
      match state.policy_canonical_usage, created.policy_canonical_usage with
      | None, None -> Ok None
      | Some previous, Some incoming ->
        let%bind expected =
          Policy_canonical_usage.reconcile_factual
            previous
            ~before:state.inference_ledger
            ~after:created.inference_ledger
        in
        Policy_canonical_usage.adopt expected ~incoming |> Result.map ~f:Option.some
      | None, Some _ ->
        Error
          (Agent_protocol.Error.invalid_request
             "replacement cannot initialize policy causes")
      | Some _, None ->
        Error
          (Agent_protocol.Error.invalid_request "replacement discarded policy accounting")
    in
    { created with
      child_contributions
    ; policy_canonical_usage
    ; goal_policy_contributions
    }
  | Managed_stop_admitted receipt ->
    let open Result.Let_syntax in
    let%bind () = Managed_stop.validate receipt in
    (match
       Int.equal receipt.generation state.identity.generation
       && Option.exists
            state.spec.delegation
            ~f:(Agent_store.Delegation_store.Reference.equal receipt.reference)
       && not
            (List.exists state.managed_stops ~f:(fun previous ->
               Managed_stop.same_key previous receipt
               || Agent_protocol.Id.Transaction.equal previous.id receipt.id))
     with
     | true -> Ok { state with managed_stops = receipt :: state.managed_stops }
     | false ->
       Error
         (Agent_protocol.Error.invalid_request
            "managed stop admission conflicts with retained identity"))
  | Managed_submission_admitted receipt ->
    let open Result.Let_syntax in
    let%bind () = Managed_submission.validate receipt in
    (match receipt.status with
     | Deferred
       when not
              (List.exists state.managed_submissions ~f:(fun previous ->
                 Managed_submission.same_key previous receipt
                 || Agent_protocol.History.Id.equal previous.history_id receipt.history_id))
       -> Ok { state with managed_submissions = receipt :: state.managed_submissions }
     | _ ->
       Error
         (Agent_protocol.Error.invalid_request
            "managed submission admission conflicts with retained receipt"))
  | Managed_submission_changed receipt ->
    let open Result.Let_syntax in
    let%bind previous =
      List.find state.managed_submissions ~f:(Managed_submission.same_key receipt)
      |> Result.of_option
           ~error:(Agent_protocol.Error.invalid_request "managed submission is absent")
    in
    let%map () = Managed_submission.validate_transition ~previous receipt in
    { state with
      managed_submissions =
        List.map state.managed_submissions ~f:(fun value ->
          if Managed_submission.same_key value receipt then receipt else value)
    }
  | Lifecycle_changed lifecycle -> Ok { state with lifecycle }
  | Organization_changed (values, metadata_revision) ->
    let open Result.Let_syntax in
    let%bind organization =
      Agent_protocol.Session_organization.Values.create
        ~project_id:values.project_id
        ~collection_ids:values.collection_ids
    in
    if
      Int64.equal state.identity.metadata_revision Int64.max_value
      || not (Int64.equal metadata_revision (Int64.succ state.identity.metadata_revision))
    then
      Error (Agent_protocol.Error.invalid_request "metadata revision must advance once")
    else
      Ok { state with identity = { state.identity with organization; metadata_revision } }
  | Metadata_changed (values, metadata_revision) ->
    let open Result.Let_syntax in
    let%bind values =
      Agent_protocol.Session_metadata.Values.create
        ~display_name:values.display_name
        ~labels:values.labels
    in
    if
      Int64.equal state.identity.metadata_revision Int64.max_value
      || not (Int64.equal metadata_revision (Int64.succ state.identity.metadata_revision))
    then
      Error (Agent_protocol.Error.invalid_request "metadata revision must advance once")
    else
      Ok
        { state with
          identity =
            { state.identity with
              display_name = values.display_name
            ; labels = values.labels
            ; metadata_revision
            }
        ; spec =
            { state.spec with
              protocol =
                { state.spec.protocol with
                  display_name = values.display_name
                ; labels = values.labels
                }
            }
        }
  | Initial_start_consumed -> Ok { state with pending_initial_start = false }
  | Parent_stop_epoch_changed epoch ->
    (match
       Int64.(epoch >= 0L)
       && Option.for_all state.parent_stop_epoch ~f:(fun previous ->
         Int64.(epoch >= previous))
     with
     | true -> Ok { state with parent_stop_epoch = Some epoch }
     | false ->
       Error (Agent_protocol.Error.invalid_request "parent stop epoch moved backwards"))
  | Stop_epoch_changed stop_epoch ->
    (match Int64.(stop_epoch > state.stop_epoch) with
     | true -> Ok { state with stop_epoch }
     | false -> Error (Agent_protocol.Error.invalid_request "stop epoch did not advance"))
  | Workspace_changed workspace_instance ->
    let quota_key =
      Option.map state.spec.quota_key ~f:(fun quota_key ->
        { quota_key with Quota_key.conflict_domain = workspace_instance.conflict_domain })
    in
    Ok { state with spec = { state.spec with workspace_instance; quota_key } }
  | Authoring_publication_changed context ->
    let open Result.Let_syntax in
    let%map () =
      Chat_response.Authoring_publication.validate_context
        context
        ~session_id:state.identity.session_id
        ~generation:state.identity.generation
    in
    { state with
      conversation = { state.conversation with authoring_publication = Some context }
    }
  | Canonical_entries_appended entries ->
    let open Result.Let_syntax in
    let%bind recovery_windows = Recovery_window_capture.append state entries in
    let%map authoring_reference_index = remember_authoring state [ entries ] in
    { state with
      conversation =
        { state.conversation with
          canonical_history = state.conversation.canonical_history @ entries
        ; authoring_reference_index
        }
    ; recovery_windows
    }
  | Canonical_history_replaced history ->
    replace_canonical_history state history ~retired:[]
  | Recovery_window_enabled (owner, window) ->
    let%map.Result ledger = Recovery_window_capture.bootstrap state ~owner ~window in
    { state with recovery_windows = Some ledger }
  | Recovery_summary_compacted (archive, history) ->
    let open Result.Let_syntax in
    let%bind ledger = Recovery_window_capture.summarize state ~archive ~history in
    let%map next = with_canonical_history state history ~recovery_windows:(Some ledger) in
    attach_archive next archive
  | Authoring_references_forgotten ids ->
    let open Result.Let_syntax in
    let%map index = Session_state.authoring_references state in
    let authoring_reference_index =
      Chat_response.Authoring_reference_index.forget index ids |> retained_authoring
    in
    { state with conversation = { state.conversation with authoring_reference_index } }
  | Initial_prompt_count_changed initial_prompt_entry_count ->
    Ok
      { state with conversation = { state.conversation with initial_prompt_entry_count } }
  | History_deleted (history_id, archive) ->
    let open Result.Let_syntax in
    let%bind plan = History_deletion.prepare state ~history_id in
    let%bind () =
      if
        Session_state.Compaction_archive.equal_kind archive.kind Delete
        && Int64.equal archive.revision state.counters.revision
      then Ok ()
      else
        Error
          (Agent_protocol.Error.invalid_request "history deletion archive basis differs")
    in
    let retired = History_deletion.retired_ids plan in
    let%bind next =
      replace_canonical_history state (History_deletion.canonical_history plan) ~retired
    in
    let%bind next =
      apply
        ~limits
        next
        (Batch
           [ Authoring_references_forgotten retired
           ; Initial_prompt_count_changed
               (History_deletion.initial_prompt_entry_count plan)
           ; Compaction_archived archive
           ])
    in
    retire_pending_dispositions
      next
      ~history_ids:(History_deletion.retired_ids plan)
      ~limits
  | History_edited (edit, archive) ->
    let open Result.Let_syntax in
    let%bind plan = History_edit.prepare state ~edit in
    let%bind () =
      if
        Session_state.Compaction_archive.equal_kind archive.kind Edit
        && Int64.equal archive.revision state.counters.revision
      then Ok ()
      else
        Error (Agent_protocol.Error.invalid_request "history edit archive basis differs")
    in
    let retired = History_edit.retired_ids plan in
    let%bind next =
      replace_canonical_history
        state
        (History_edit.canonical_history plan)
        ~retired:(Agent_protocol.History_edit.history_id edit :: retired)
    in
    let%bind next =
      apply
        ~limits
        next
        (Batch
           [ Authoring_references_forgotten retired
           ; Initial_prompt_count_changed (History_edit.initial_prompt_entry_count plan)
           ; Compaction_archived archive
           ])
    in
    retire_pending_dispositions next ~history_ids:(History_edit.retired_ids plan) ~limits
  | Compaction_archived archive ->
    if
      Option.is_some state.recovery_windows
      && Session_state.Compaction_archive.equal_kind archive.kind Compaction
    then
      Error
        (Agent_protocol.Error.invalid_request
           "enabled compaction requires exact window capture")
    else Ok (attach_archive state archive)
  | Deferred_entries_enqueued entries ->
    let%bind.Result additions =
      List.map entries ~f:(fun entry ->
        let open Result.Let_syntax in
        let%bind input =
          Agent_protocol.Pending_input.create
            ~entry
            ~generation:state.identity.generation
            ~binding:Agent_protocol.Pending_input.Binding.safe_boundary
        in
        Pending_input_document.authored input ~limits
        |> Result.map_error ~f:(fun error ->
          Agent_protocol.Error.invalid_request
            (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error))))
      |> Result.all
    in
    let%map.Result pending_revision =
      Agent_protocol.Pending_input.Revision.succ state.conversation.pending_revision
    in
    { state with
      conversation =
        { state.conversation with
          deferred_user_entries = state.conversation.deferred_user_entries @ additions
        ; pending_revision
        }
    }
  | Deferred_entries_adopted ->
    let%bind.Result plan = Pending_plan.legacy_adoption state ~limits in
    Pending_plan.apply plan state
  | Pending_inputs_changed (mutation, archive, _expiry_archive) ->
    let open Result.Let_syntax in
    let%bind plan = Pending_mutation.prepare mutation state ~limits in
    let%bind () =
      if Pending_plan.requires_archive plan && Option.is_none archive
      then
        Error
          (Agent_protocol.Error.invalid_request
             "pending mutation requires exact prestate archival")
      else Ok ()
    in
    let%map next = Pending_plan.apply plan state in
    { next with
      conversation =
        { next.conversation with
          compaction_archives =
            Option.to_list archive @ next.conversation.compaction_archives
        }
    }
  | Active_operation_changed active_operation ->
    let automatic_turn_budget =
      match active_operation, state.active_operation with
      | Some next, Some previous
        when Agent_protocol.Id.Operation.equal next.id previous.id ->
        state.automatic_turn_budget
      | Some operation, _ ->
        Option.map state.automatic_turn_budget ~f:(fun budget ->
          Automatic_turn_budget.note_operation budget operation)
      | None, _ -> state.automatic_turn_budget
    in
    Ok { state with active_operation; automatic_turn_budget }
  | Automatic_turn_budget_enabled policy ->
    (match state.automatic_turn_budget with
     | Some budget when Chat_response.Runtime_semantics.equal_policy budget.policy policy
       -> Ok state
     | Some _ ->
       Error
         (Agent_protocol.Error.invalid_request
            "automatic-turn policy cannot be rebound by runtime reload")
     | None ->
       let budget = Automatic_turn_budget.create policy in
       Result.map (Automatic_turn_budget.validate budget) ~f:(fun () ->
         { state with automatic_turn_budget = Some budget }))
  | Automatic_turn_pauses_changed conditions ->
    (match state.automatic_turn_budget with
     | None ->
       Error (Agent_protocol.Error.invalid_request "automatic-turn policy is not enabled")
     | Some budget ->
       Ok
         { state with
           automatic_turn_budget =
             Some (Automatic_turn_budget.with_pauses budget conditions)
         })
  | Attachment_added attachment ->
    Ok
      { state with
        attachments =
          replace_by
            Agent_protocol.Id.Attachment.compare
            attachment.id
            attachment
            state.attachments
            ~id_of:(fun value -> value.Agent_protocol.Session.Attachment.id)
      }
  | Attachment_removed attachment_id ->
    Ok
      { state with
        attachments =
          List.filter state.attachments ~f:(fun value ->
            Agent_protocol.Id.Attachment.compare value.id attachment_id <> 0)
      }
  | Permission_changed permission ->
    let open Result.Let_syntax in
    let%bind () =
      match
        List.find state.permissions ~f:(fun previous ->
          Agent_protocol.Id.Permission.equal previous.id permission.id)
      with
      | Some previous
        when not
               (Agent_protocol.Permission.equal_owner previous.owner permission.owner
                && previous.generation = permission.generation
                && Agent_protocol.Id.Session.equal
                     previous.session_id
                     permission.session_id) ->
        Error
          (Agent_protocol.Error.create
             Conflict
             ~message:"permission ownership is immutable"
             ~retryable:false
             ())
      | _ -> Ok ()
    in
    Ok
      { state with
        permissions =
          replace_by
            Agent_protocol.Id.Permission.compare
            permission.id
            permission
            state.permissions
            ~id_of:(fun value -> value.Agent_protocol.Permission.id)
      }
  | Grant_changed grant ->
    Ok
      { state with
        grants =
          replace_by
            Agent_protocol.Id.Grant.compare
            grant.id
            grant
            state.grants
            ~id_of:(fun value -> value.Agent_protocol.Grant.id)
      }
  | Inference_target_captured target ->
    let open Result.Let_syntax in
    let%map inference_target =
      Inference.Selection.capture state.spec.inference_target ~target ~limits
      |> Result.map_error ~f:inference_error
    in
    { state with spec = { state.spec with inference_target } }
  | Configuration_revision_changed revision ->
    if
      Int64.(state.spec.configuration_revision = max_value)
      || not (Int64.equal revision Int64.(state.spec.configuration_revision + 1L))
    then
      Error
        (Agent_protocol.Error.invalid_request "configuration revision must advance once")
    else Ok { state with spec = { state.spec with configuration_revision = revision } }
  | Inference_target_changed target ->
    let open Result.Let_syntax in
    let%map inference_target =
      Inference.Selection.change state.spec.inference_target ~target ~limits
      |> Result.map_error ~f:inference_error
    in
    { state with spec = { state.spec with inference_target } }
  | Model_job_target_captured binding ->
    let open Result.Let_syntax in
    let%bind target =
      match Inference.Selection.view (Model_job_target.source binding) with
      | Captured target -> Ok target
      | Unresolved ->
        Error (Agent_protocol.Error.invalid_request "new model job capture is unresolved")
    in
    let%map binding =
      match
        List.find state.model_job_targets ~f:(fun previous ->
          Agent_protocol.Id.Job.equal
            (Model_job_target.job_id previous)
            (Model_job_target.job_id binding))
      with
      | None -> Ok binding
      | Some previous
        when Int.equal
               (Model_job_target.generation previous)
               (Model_job_target.generation binding) ->
        Model_job_target.capture_source previous ~target ~limits
      | Some _ ->
        Error (Agent_protocol.Error.invalid_request "model job target generation changed")
    in
    { state with
      model_job_targets =
        replace_by
          Agent_protocol.Id.Job.compare
          (Model_job_target.job_id binding)
          binding
          state.model_job_targets
          ~id_of:Model_job_target.job_id
    }
  | Model_job_recipe_target_captured binding ->
    let open Result.Let_syntax in
    let%bind previous =
      List.find state.model_job_targets ~f:(fun previous ->
        Agent_protocol.Id.Job.equal
          (Model_job_target.job_id previous)
          (Model_job_target.job_id binding))
      |> Result.of_option
           ~error:
             (Agent_protocol.Error.invalid_request
                "recipe capture has no model job source")
    in
    let%bind () =
      if
        Int.equal
          (Model_job_target.generation previous)
          (Model_job_target.generation binding)
        && Inference.Selection.equal
             (Model_job_target.source previous)
             (Model_job_target.source binding)
      then Ok ()
      else
        Error
          (Agent_protocol.Error.invalid_request "recipe capture changed model job source")
    in
    let%bind target =
      match Inference.Selection.view (Model_job_target.execution binding) with
      | Captured target -> Ok target
      | Unresolved ->
        Error (Agent_protocol.Error.invalid_request "recipe target capture is unresolved")
    in
    let%map binding = Model_job_target.capture_recipe previous ~target ~limits in
    { state with
      model_job_targets =
        replace_by
          Agent_protocol.Id.Job.compare
          (Model_job_target.job_id binding)
          binding
          state.model_job_targets
          ~id_of:Model_job_target.job_id
    }
  | Model_job_target_restored binding ->
    let open Result.Let_syntax in
    let%bind () =
      match Inference.Selection.view (Model_job_target.source binding) with
      | Unresolved -> Ok ()
      | Captured _ ->
        Error
          (Agent_protocol.Error.invalid_request
             "restored model job binding must be unresolved")
    in
    (match
       List.find state.model_job_targets ~f:(fun previous ->
         Agent_protocol.Id.Job.equal
           (Model_job_target.job_id previous)
           (Model_job_target.job_id binding))
     with
     | None -> Ok { state with model_job_targets = binding :: state.model_job_targets }
     | Some previous
       when Int.equal
              (Model_job_target.generation previous)
              (Model_job_target.generation binding) -> Ok state
     | Some _ ->
       Error
         (Agent_protocol.Error.invalid_request
            "restored model job target generation changed"))
  | Delegated_inference_ledger_changed (inference_ledger, incoming) ->
    let open Result.Let_syntax in
    let%bind () =
      match state.policy_canonical_usage with
      | None -> Ok ()
      | Some _ ->
        Error
          (Agent_protocol.Error.invalid_request
             "caused canonical ledger requires its paired policy mutation")
    in
    let%bind previous =
      Result.of_option
        state.delegated_usage
        ~error:
          (Agent_protocol.Error.invalid_request
             "historical delegated partition cannot bootstrap from a ledger mutation")
    in
    let%bind () =
      Inference_ledger.validate_update state.inference_ledger ~incoming:inference_ledger
      |> Result.map_error ~f:(fun error ->
        Agent_protocol.Error.invalid_request
          (Sexp.to_string_hum (Inference_ledger.Error.sexp_of_t error)))
    in
    let%bind () =
      Inference_ledger.validate
        inference_ledger
        ~limits:Inference_ledger.Limits.default
        ~session_id:state.identity.session_id
        ~generation:state.identity.generation
      |> Result.map_error ~f:(fun error ->
        Agent_protocol.Error.invalid_request
          (Sexp.to_string_hum (Inference_ledger.Error.sexp_of_t error)))
    in
    let%bind () =
      Delegated_usage.validate_transition
        previous
        ~incoming
        ~before:state.inference_ledger
        ~after:inference_ledger
    in
    let%map delegated_usage = Delegated_usage.adopt previous ~incoming in
    { state with inference_ledger; delegated_usage = Some delegated_usage }
  | Goal_policy_contributions_changed update ->
    Ok
      { state with
        goal_policy_contributions =
          Some (Goal_policy_contributions.Update.candidate update)
      }
  | Policy_cause_changed update ->
    Ok
      { state with
        policy_canonical_usage = Some (Policy_canonical_usage.Update.candidate update)
      }
  | Policy_inference_ledger_changed (ledger, delegated_usage, update) ->
    let open Result.Let_syntax in
    let%bind () =
      if Option.is_some state.policy_canonical_usage
      then Ok ()
      else
        Error
          (Agent_protocol.Error.invalid_request
             "policy canonical mutation has no component")
    in
    (* The closed paired leaf reuses the original ledger/delegated checks. The
       actual policy candidate is then verified by the ordered validator; this
       temporary pure receiver is neither published nor execution authority. *)
    let%map next =
      apply
        ~limits
        { state with policy_canonical_usage = None }
        (inference_ledger_update ledger ~delegated_usage)
    in
    { next with
      policy_canonical_usage = Some (Policy_canonical_usage.Update.candidate update)
    }
  | Goal_descendants_changed update ->
    let child_contributions =
      match Goal_descendants.Update.children update with
      | Preserve -> state.child_contributions
      | Replace children -> Some children
    in
    Ok
      { state with
        goal_descendants = Some (Goal_descendants.Update.accounting update)
      ; child_contributions
      }
  | Child_contributions_changed incoming ->
    let open Result.Let_syntax in
    let%bind children =
      match state.child_contributions with
      | Some previous -> Child_contributions.adopt previous ~incoming
      | None ->
        (match Child_contributions.inventory incoming with
         | Historical_children_unknown ->
           let%map _ = Child_contributions.to_jsonaf incoming in
           incoming
         | Complete_inventory ->
           Error
             (Agent_protocol.Error.invalid_request
                "historical child inventory cannot become complete"))
    in
    Ok { state with child_contributions = Some children }
  | Inference_ledger_changed inference_ledger ->
    let open Result.Let_syntax in
    let%bind () =
      match state.policy_canonical_usage with
      | None -> Ok ()
      | Some _ ->
        Error
          (Agent_protocol.Error.invalid_request
             "caused canonical ledger requires its paired policy mutation")
    in
    let%bind () =
      match state.delegated_usage with
      | None -> Ok ()
      | Some _ ->
        Error
          (Agent_protocol.Error.invalid_request
             "partitioned child requires its paired canonical usage mutation")
    in
    let%bind () =
      Inference_ledger.validate_update state.inference_ledger ~incoming:inference_ledger
      |> Result.map_error ~f:(fun error ->
        Agent_protocol.Error.invalid_request
          (Sexp.to_string_hum (Inference_ledger.Error.sexp_of_t error)))
    in
    let%map () =
      Inference_ledger.validate
        inference_ledger
        ~limits:Inference_ledger.Limits.default
        ~session_id:state.identity.session_id
        ~generation:state.identity.generation
      |> Result.map_error ~f:(fun error ->
        Agent_protocol.Error.invalid_request
          (Sexp.to_string_hum (Inference_ledger.Error.sexp_of_t error)))
    in
    { state with inference_ledger }
  | Job_changed job ->
    let open Result.Let_syntax in
    let previous =
      List.find state.jobs ~f:(fun previous ->
        Agent_protocol.Id.Job.equal previous.id job.id)
    in
    let%bind () = Agent_protocol.Job.validate_inference_origin_transition ~previous job in
    let%bind () = Agent_protocol.Job.validate_delivery_transition ~previous job in
    let%bind () =
      match previous with
      | None -> Ok ()
      | Some previous
        when Option.equal Agent_protocol.Job.equal_launch previous.launch job.launch ->
        (match previous.status, job.status with
         | Waiting_completion before, Waiting_completion after
           when Agent_protocol.Job.equal_dependency before after
                && Int.equal previous.attempt job.attempt -> Ok ()
         | ( Waiting_completion _
           , (Waiting_completion _ | Queued | Running | Waiting_permission _) ) ->
           Error
             (Agent_protocol.Error.invalid_request "waiting job dependency is immutable")
         | _, Waiting_completion _ ->
           (match previous.status with
            | Running when Int.equal previous.attempt job.attempt -> Ok ()
            | _ ->
              Error
                (Agent_protocol.Error.invalid_request
                   "only a running attempt can begin waiting"))
         | _ -> Ok ())
      | Some _ ->
        Error (Agent_protocol.Error.invalid_request "job launch provenance is immutable")
    in
    Ok
      { state with
        jobs =
          replace_by
            Agent_protocol.Id.Job.compare
            job.id
            job
            state.jobs
            ~id_of:(fun value -> value.Agent_protocol.Job.id)
      }
  | Schedule_changed schedule ->
    let open Result.Let_syntax in
    let previous =
      List.find state.schedules ~f:(fun old ->
        Agent_protocol.Id.Schedule.equal old.id schedule.id)
    in
    let%map () = Agent_protocol.Schedule.validate_transition ~previous schedule in
    { state with
      schedules =
        replace_by
          Agent_protocol.Id.Schedule.compare
          schedule.id
          schedule
          state.schedules
          ~id_of:(fun value -> value.Agent_protocol.Schedule.id)
    }
  | (Invocation_changed invocation | Invocation_reconciled invocation) as delta ->
    let open Result.Let_syntax in
    let recovery =
      match delta with
      | Invocation_reconciled _ -> true
      | _ -> false
    in
    let context = invocation.Agent_protocol.Invocation.context in
    let%bind () =
      if
        Agent_protocol.Id.Session.compare context.session_id state.identity.session_id
        <> 0
        ||
        if recovery
        then context.generation > state.identity.generation
        else context.generation <> state.identity.generation
      then
        Error
          (Agent_protocol.Error.create
             Conflict
             ~message:"invocation does not belong to the current session generation"
             ~retryable:false
             ())
      else Ok ()
    in
    let previous =
      List.find state.invocations ~f:(fun candidate ->
        Agent_protocol.Id.Invocation.compare candidate.context.id context.id = 0)
    in
    let%bind () =
      match recovery, previous with
      | true, Some previous when not (nonexecuting_intent_transition previous invocation)
        ->
        Error
          (Agent_protocol.Error.invalid_request
             "reconciliation cannot admit handler or observation actions")
      | _ -> Ok ()
    in
    let%bind () =
      if not recovery
      then Ok ()
      else (
        match previous, invocation.status with
        | Some { status = Admitted | Dispatching; _ }, Resolved (Cancelled _) -> Ok ()
        | Some { status = Resolved _; _ }, Published _ -> Ok ()
        | Some { status = Resolved _; _ }, Resolved _
          when Option.is_some invocation.publication_discarded -> Ok ()
        | Some previous, _
          when ((match previous.observation, invocation.observation with
                 | ( Some { status = Observing; _ }
                   , Some { status = Observation_failed _; _ } ) -> true
                 | ( Some
                       { status = Observed
                       ; follow_up =
                           Some (Pending_follow_up _ | Compaction_accepted_follow_up _)
                       ; _
                       }
                   , Some
                       { status = Observed; follow_up = Some (Discarded_follow_up _); _ }
                   ) -> true
                 | _ -> false)
                ||
                match previous.handler_intent, invocation.handler_intent with
                | ( Some
                      { follow_up = Pending_follow_up _ | Compaction_accepted_follow_up _
                      ; _
                      }
                  , Some { follow_up = Discarded_follow_up _; _ } ) -> true
                | _ -> false)
               && Agent_protocol.Invocation.equal_status previous.status invocation.status
               && Option.equal
                    Agent_protocol.History.Id.equal
                    previous.output_entry_id
                    invocation.output_entry_id
               && Option.equal
                    String.equal
                    previous.publication_discarded
                    invocation.publication_discarded -> Ok ()
        | _ ->
          Error
            (Agent_protocol.Error.invalid_request
               "reconciliation only finishes an existing invocation without executing it"))
    in
    let%bind () = Agent_protocol.Invocation.validate_transition ~previous invocation in
    let%bind () =
      Extension_invariants.invocation_event_owner
        ~events:state.moderator_executions
        invocation
    in
    let%bind () =
      match previous, invocation.parent_event with
      | None, Some parent ->
        (match
           List.find state.moderator_executions ~f:(fun event ->
             Agent_protocol.Id.Moderator_execution.equal event.context.id parent)
         with
         | Some { status = Running; _ } -> Ok ()
         | _ ->
           Error
             (Agent_protocol.Error.create
                Conflict
                ~message:"event parent is not executing"
                ~retryable:false
                ()))
      | _ -> Ok ()
    in
    let%bind () =
      Invocation_history.validate_retained
        ~history:state.conversation.canonical_history
        invocation
    in
    let%map () =
      match context.call_entry_id, previous, invocation.status with
      | Some _, None, _ ->
        Invocation_history.validate_call
          ~history:state.conversation.canonical_history
          invocation
      | Some _, Some { status = Resolved _; _ }, Published _ ->
        Invocation_history.validate_publication
          ~history:state.conversation.canonical_history
          invocation
      | _ -> Ok ()
    in
    { state with
      invocations =
        replace_by
          Agent_protocol.Id.Invocation.compare
          context.id
          invocation
          state.invocations
          ~id_of:(fun value -> value.Agent_protocol.Invocation.context.id)
    }
  | ( Subscription_changed subscription
    | Subscription_expired subscription
    | Subscription_cancelled subscription ) as delta ->
    let open Result.Let_syntax in
    let c = subscription.Agent_protocol.Subscription.context in
    let host_terminal =
      match delta with
      | Subscription_expired _ | Subscription_cancelled _ -> true
      | _ -> false
    in
    let%bind () =
      Extension_invariants.owner
        ~session_id:state.identity.session_id
        ~generation:(if host_terminal then c.generation else state.identity.generation)
        c.session_id
        c.generation
    in
    let previous =
      List.find state.subscriptions ~f:(fun old ->
        Agent_protocol.Id.Subscription.compare old.context.id c.id = 0)
    in
    let%bind () =
      match delta, previous, subscription.result with
      | Subscription_changed _, _, _ -> Ok ()
      | Subscription_expired _, Some _, Some Expired
        when c.generation <= state.identity.generation -> Ok ()
      | Subscription_cancelled _, Some _, Some (Cancelled _)
        when c.generation <= state.identity.generation && Option.is_some c.source -> Ok ()
      | _ ->
        Error
          (Agent_protocol.Error.invalid_request
             "host subscription terminalization requires its matching existing record \
              and result")
    in
    let%map () = Agent_protocol.Subscription.validate_transition ~previous subscription in
    { state with
      subscriptions =
        replace_by
          Agent_protocol.Id.Subscription.compare
          c.id
          subscription
          state.subscriptions
          ~id_of:(fun s -> s.Agent_protocol.Subscription.context.id)
    }
  | Ingress_changed registration ->
    let open Result.Let_syntax in
    let context = registration.External_ingress.context in
    let%bind () =
      Extension_invariants.owner
        ~session_id:state.identity.session_id
        ~generation:state.identity.generation
        context.session_id
        context.generation
    in
    let%bind subscription =
      List.find state.subscriptions ~f:(fun value ->
        Agent_protocol.Id.Subscription.equal value.context.id context.subscription_id)
      |> Result.of_option
           ~error:
             (Agent_protocol.Error.invalid_request
                "missing external ingress subscription")
    in
    let previous =
      List.find state.ingress_registrations ~f:(fun value ->
        Agent_protocol.Id.Capability.equal value.context.id context.id)
    in
    let%map () =
      External_ingress.validate_transition ~subscription ~previous registration
    in
    { state with
      ingress_registrations =
        replace_by
          Agent_protocol.Id.Capability.compare
          context.id
          registration
          state.ingress_registrations
          ~id_of:(fun value -> value.External_ingress.context.id)
    }
  | Delivery_changed delivery ->
    let open Result.Let_syntax in
    let c = delivery.Agent_protocol.Delivery.context in
    let%bind () =
      Extension_invariants.owner
        ~session_id:state.identity.session_id
        ~generation:state.identity.generation
        c.session_id
        c.generation
    in
    let previous =
      List.find state.deliveries ~f:(fun old ->
        Agent_protocol.Id.Delivery.compare old.context.id c.id = 0)
    in
    let%bind () =
      match delivery.status with
      | Committed _ ->
        Error
          (Agent_protocol.Error.create
             Invalid_state
             ~message:"delivery commit requires an atomic history insertion"
             ~retryable:false
             ())
      | Pending | Failed _ -> Ok ()
    in
    let%map () = Agent_protocol.Delivery.validate_transition ~previous delivery in
    { state with
      deliveries =
        replace_by
          Agent_protocol.Id.Delivery.compare
          c.id
          delivery
          state.deliveries
          ~id_of:(fun d -> d.Agent_protocol.Delivery.context.id)
    }
  | Delivery_committed (delivery, entry) ->
    let open Result.Let_syntax in
    let c = delivery.Agent_protocol.Delivery.context in
    let invalid message =
      Error (Agent_protocol.Error.create Invalid_state ~message ~retryable:false ())
    in
    let%bind () =
      Extension_invariants.owner
        ~session_id:state.identity.session_id
        ~generation:state.identity.generation
        c.session_id
        c.generation
    in
    let previous =
      List.find state.deliveries ~f:(fun old ->
        Agent_protocol.Id.Delivery.compare old.context.id c.id = 0)
    in
    let%bind () = Agent_protocol.Delivery.validate_transition ~previous delivery in
    let%bind () =
      Extension_invariants.delivery_ready
        ~invocations:state.invocations
        ~jobs:state.jobs
        ~events:state.moderator_executions
        delivery
    in
    let%bind () =
      match delivery.status, entry.Agent_protocol.History.provenance with
      | Committed { history_id; _ }, Runtime_notification id
        when History_entry.Id.equal history_id entry.id
             && Agent_protocol.Id.Delivery.compare id c.id = 0 -> Ok ()
      | _ -> invalid "delivery/history identity or runtime provenance mismatch"
    in
    let%bind decoded = History_codec.of_protocol entry in
    let classified = History_codec.to_protocol decoded in
    let%bind () =
      if
        Agent_protocol.History.equal_role classified.role User
        && Agent_protocol.History.equal_kind classified.kind Message
        && Agent_protocol.History.equal_role entry.role User
        && Agent_protocol.History.equal_kind entry.kind Message
      then Ok ()
      else invalid "notification must be runtime data in a user input representation"
    in
    let already_committed =
      Option.exists previous ~f:(fun old ->
        match old.status with
        | Committed _ -> true
        | _ -> false)
    in
    if already_committed
    then (
      match previous with
      | Some previous when Agent_protocol.Delivery.equal previous delivery -> Ok state
      | _ -> invalid "notification wake changes require their own transition")
    else if
      List.exists state.conversation.canonical_history ~f:(fun old ->
        History_entry.Id.equal old.id entry.id)
    then invalid "notification history identity already exists"
    else (
      let%map recovery_windows = Recovery_window_capture.append state [ entry ] in
      { state with
        recovery_windows
      ; deliveries =
          replace_by
            Agent_protocol.Id.Delivery.compare
            c.id
            delivery
            state.deliveries
            ~id_of:(fun d -> d.Agent_protocol.Delivery.context.id)
      ; conversation =
          { state.conversation with
            canonical_history = state.conversation.canonical_history @ [ entry ]
          }
      })
  | Delivery_wake_changed delivery ->
    let open Result.Let_syntax in
    let module P = Agent_protocol in
    let invalid message = Error (P.Error.invalid_request message) in
    let%bind () =
      Extension_invariants.owner
        ~session_id:state.identity.session_id
        ~generation:state.identity.generation
        delivery.context.session_id
        delivery.context.generation
    in
    let%bind previous =
      List.find state.deliveries ~f:(fun old ->
        P.Id.Delivery.equal old.context.id delivery.context.id)
      |> Result.of_option
           ~error:
             (P.Error.invalid_request "notification wake references an unknown delivery")
    in
    let%bind () =
      match previous.status, delivery.status with
      | Committed _, Committed _ -> Ok ()
      | _ -> invalid "notification wake cannot insert history"
    in
    let%bind () = P.Delivery.validate_transition ~previous:(Some previous) delivery in
    if P.Delivery.equal previous delivery
    then Ok state
    else (
      let%bind () =
        match delivery.wake_disposition with
        | Some (Accepted_wake id) ->
          (match
             ( state.active_operation
             , state.lifecycle.desired
             , state.lifecycle.observed
             , state.halted )
           with
           | ( Some
                 { id = active; generation; kind = Turn _; state = Starting | Running; _ }
             , Running
             , Running_turn observed
             , false )
             when P.Id.Operation.equal id active
                  && P.Id.Operation.equal id observed
                  && Int.equal generation delivery.context.generation -> Ok ()
           | _ ->
             invalid "notification wake acceptance requires its admitted running turn")
        | Some (Discarded_wake _) -> Ok ()
        | None | Some Pending_wake ->
          invalid "notification wake has no terminal disposition"
      in
      Ok
        { state with
          deliveries =
            replace_by
              P.Id.Delivery.compare
              delivery.context.id
              delivery
              state.deliveries
              ~id_of:(fun value -> value.P.Delivery.context.id)
        })
  | Moderator_lifecycle_claimed { receipt; capture } ->
    let open Result.Let_syntax in
    let%bind () =
      if
        List.exists state.moderator_executions ~f:(fun e ->
          Agent_protocol.Id.Moderator_execution.equal
            e.Agent_protocol.Moderator_execution.context.id
            receipt.context.id)
      then
        Error
          (Agent_protocol.Error.invalid_request
             "lifecycle capture requires a new receipt")
      else if
        not
          (Agent_protocol.Id.Moderator_execution.equal
             receipt.context.id
             (Moderator_lifecycle.execution_id capture))
      then
        Error
          (Agent_protocol.Error.invalid_request
             "lifecycle capture selects another receipt")
      else (
        match receipt.status with
        | Running when Moderator_lifecycle.lifecycle_phase receipt.context.phase -> Ok ()
        | Running | Completed _ | Failed _ | Interrupted _ ->
          Error
            (Agent_protocol.Error.invalid_request
               "lifecycle capture requires a new Running lifecycle"))
    in
    let%bind index =
      Result.of_option
        state.run_state
        ~error:
          (Agent_protocol.Error.invalid_request
             "lifecycle capture requires an installed index")
    in
    let%bind index = Run_state.capture_moderator_lifecycle index capture in
    let%bind state = apply ~limits state (Moderator_execution_changed receipt) in
    let next = { state with run_state = Some index } in
    let%map () = Session_state.validate_moderator_lifecycle next in
    next
  | (Moderator_execution_changed execution | Moderator_execution_reconciled execution) as
    delta ->
    let open Result.Let_syntax in
    let module E = Agent_protocol.Moderator_execution in
    let c = execution.E.context in
    let recovery =
      match delta with
      | Moderator_execution_reconciled _ -> true
      | _ -> false
    in
    let%bind () =
      if
        (not (Agent_protocol.Id.Session.equal c.session_id state.identity.session_id))
        ||
        if recovery
        then c.generation > state.identity.generation
        else c.generation <> state.identity.generation
      then
        Error
          (Agent_protocol.Error.create
             Conflict
             ~message:"event execution owner mismatch"
             ~retryable:false
             ())
      else Ok ()
    in
    let previous =
      List.find state.moderator_executions ~f:(fun e ->
        Agent_protocol.Id.Moderator_execution.equal e.E.context.id c.id)
    in
    let%bind () =
      match recovery, previous, execution.status, execution.intent with
      | false, _, _, _ -> Ok ()
      | true, Some { status = Running; _ }, Interrupted _, _ -> Ok ()
      | ( true
        , Some { status = Completed _; intent = Some (Pending | Waiting_compaction _); _ }
        , Completed _
        , Some (Discarded _) ) -> Ok ()
      | _ ->
        Error
          (Agent_protocol.Error.create
             Invalid_state
             ~message:"recovery may only retire existing event work"
             ~retryable:false
             ())
    in
    let%bind () = E.validate_transition ~previous execution in
    let%map () =
      match previous with
      | Some _ -> Ok ()
      | None ->
        Queue_provenance_validation.validate_event_admission
          (Session_state.queue_provenance state)
          execution
    in
    { state with
      run_state =
        (if Option.is_none previous && Moderator_lifecycle.lifecycle_phase c.phase
         then Option.map state.run_state ~f:Run_state.clear_moderator_lifecycle
         else state.run_state)
    ; moderator_executions =
        replace_by
          Agent_protocol.Id.Moderator_execution.compare
          c.id
          execution
          state.moderator_executions
          ~id_of:(fun e -> e.E.context.id)
    }
  | Goal_accounts_changed incoming ->
    let open Result.Let_syntax in
    let previous = Option.value state.goal_accounts ~default:Goal_accounts.empty in
    let%map accounts = Goal_accounts.adopt previous ~incoming in
    { state with goal_accounts = Some accounts }
  | Goal_catalog_changed catalog ->
    let open Result.Let_syntax in
    let previous = Option.value state.goal_catalog ~default:Goal_catalog.empty in
    let%map () = Goal_catalog.validate_transition previous ~incoming:catalog in
    { state with goal_catalog = Some catalog }
  | Recovery_note_changed change ->
    let%map.Result catalog =
      Recovery_note_change.apply
        change
        ~previous:state.recovery_notes
        ~session_id:state.identity.session_id
    in
    { state with recovery_notes = Some catalog }
  | Instruction_selection_changed selection ->
    let open Result.Let_syntax in
    let previous =
      Option.value state.instruction_selection ~default:Instruction_selection.empty
    in
    let%map selection = Instruction_selection.adopt previous ~incoming:selection in
    { state with instruction_selection = Some selection }
  | Recurrence_catalog_changed catalog ->
    let open Result.Let_syntax in
    let previous =
      Option.value state.recurrence_catalog ~default:Recurrence_catalog.empty
    in
    let%map () = Recurrence_catalog.validate_transition previous ~incoming:catalog in
    { state with recurrence_catalog = Some catalog }
  | Run_state_changed run_state ->
    let open Result.Let_syntax in
    let%bind () =
      Session_state.validate_moderator_lifecycle_index_update state run_state
    in
    let previous = Option.value state.run_state ~default:Run_state.empty in
    let%map () = Run_state.validate_transition ~previous run_state in
    { state with run_state = Some run_state }
  | Moderator_changed moderator ->
    let%map.Result () =
      Moderator_checkpoint.validate_update ~previous:state.moderator moderator
    in
    { state with moderator }
  | Shell_changed shell -> Ok { state with shell }
  | History_block_reserved reserved_history_through ->
    if
      Int64.(reserved_history_through < state.conversation.reserved_history_through)
      || Int64.(reserved_history_through < state.conversation.next_history_sequence)
    then
      Error
        (Agent_protocol.Error.create
           Journal_corrupt
           ~message:"history reservation regressed"
           ~retryable:false
           ())
    else
      Ok
        { state with
          conversation =
            { state.conversation with
              next_history_sequence = reserved_history_through
            ; reserved_history_through
            }
        }
  | Compaction_generation_changed compaction_generation ->
    if compaction_generation <= state.conversation.compaction_generation
    then
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"compaction generation did not advance"
           ~retryable:false
           ())
    else
      Ok { state with conversation = { state.conversation with compaction_generation } }
  | Owner_lease_generation_changed owner_lease_generation ->
    if Int64.(owner_lease_generation <= state.counters.owner_lease_generation)
    then
      Error
        (Agent_protocol.Error.create
           Lease_stale
           ~message:"owner lease generation did not advance"
           ~retryable:false
           ())
    else Ok { state with counters = { state.counters with owner_lease_generation } }
  | Failure_changed failure -> Ok { state with failure }
  | Halt_changed halt_reason ->
    Ok { state with halted = Option.is_some halt_reason; halt_reason }
  | Reset_generation generation ->
    if generation <= state.identity.generation
    then
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"session generation did not advance"
           ~retryable:false
           ())
    else if Option.is_some state.recovery_windows
    then
      Error
        (Agent_protocol.Error.invalid_request
           "enabled recovery requires an archived administrative generation change")
    else
      let open Result.Let_syntax in
      let%bind inference_ledger =
        Inference_ledger.with_generation state.inference_ledger ~generation
        |> Result.map_error ~f:(fun error ->
          Agent_protocol.Error.invalid_request
            (Sexp.to_string_hum (Inference_ledger.Error.sexp_of_t error)))
      in
      let%bind delegated_usage =
        match state.delegated_usage with
        | None -> Ok None
        | Some own ->
          Delegated_usage.reconcile
            own
            ~before:state.inference_ledger
            ~after:inference_ledger
          |> Result.map ~f:Option.some
      in
      let%map policy_canonical_usage =
        Option.value_map
          state.policy_canonical_usage
          ~default:(Ok None)
          ~f:(fun previous ->
            Policy_canonical_usage.reconcile_factual
              previous
              ~before:state.inference_ledger
              ~after:inference_ledger
            |> Result.map ~f:Option.some)
      in
      { state with
        inference_ledger
      ; delegated_usage
      ; policy_canonical_usage
      ; identity = { state.identity with generation }
      ; run_state = Option.map state.run_state ~f:Run_state.clear_moderator_lifecycle
      ; pending_initial_start = false
      ; conversation =
          { state.conversation with
            authoring_reference_index = None
          ; authoring_publication = None
          }
      }
;;

let capture_new_model_jobs ?(limits = native_limits) state delta =
  let open Result.Let_syntax in
  let rec prepare (state : Session_state.t) delta =
    match delta with
    | Batch changes ->
      let%map state, changes =
        List.fold_result changes ~init:(state, []) ~f:(fun (state, changes) change ->
          let%map state, change = prepare state change in
          state, change :: changes)
      in
      state, Batch (List.rev changes)
    | Job_changed job ->
      let%bind delta =
        match job.kind with
        | Model_call
          when not
                 (List.exists state.model_job_targets ~f:(fun binding ->
                    Agent_protocol.Id.Job.equal (Model_job_target.job_id binding) job.id))
          ->
          let%bind () =
            if
              List.exists state.jobs ~f:(fun previous ->
                Agent_protocol.Id.Job.equal previous.id job.id)
            then
              Error
                (Agent_protocol.Error.invalid_request
                   "existing model job has no retained source binding")
            else Ok ()
          in
          let%bind target =
            match Inference.Selection.view state.spec.inference_target with
            | Captured target -> Ok target
            | Unresolved ->
              Error
                (Agent_protocol.Error.invalid_request
                   "model job source requires explicit inference migration")
          in
          let%map binding = Model_job_target.create job ~target ~limits in
          Batch [ delta; Model_job_target_captured binding ]
        | Model_call
        | Nested_agent
        | Scheduled_event
        | Async_tool
        | Shell_process
        | Compaction -> Ok delta
      in
      let%map state = apply ~limits state delta in
      state, delta
    | delta ->
      let%map state = apply ~limits state delta in
      state, delta
  in
  let%map _, delta = prepare state delta in
  delta
;;

let validate_goal_accounts delta ~(previous : Session_state.t) ~(next : Session_state.t) =
  let module B = Goal_accounts.Ordered_bootstrap in
  let rec observe cursor = function
    | Batch changes -> List.fold_result changes ~init:cursor ~f:observe
    | Goal_catalog_changed catalog -> B.advance cursor (Catalog catalog)
    | Goal_accounts_changed accounts -> B.advance cursor (Accounts accounts)
    | Created state ->
      B.advance
        cursor
        (Replacement { catalog = state.goal_catalog; accounts = state.goal_accounts })
    | Lifecycle_changed _
    | Metadata_changed _
    | Organization_changed _
    | Initial_start_consumed
    | Stop_epoch_changed _
    | Parent_stop_epoch_changed _
    | Workspace_changed _
    | Canonical_entries_appended _
    | Canonical_history_replaced _
    | Recovery_window_enabled _
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
    | Inference_ledger_changed _
    | Delegated_inference_ledger_changed _
    | Goal_policy_contributions_changed _
    | Child_contributions_changed _
    | Goal_descendants_changed _
    | Policy_cause_changed _
    | Policy_inference_ledger_changed _
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
    | Halt_changed _
    | Reset_generation _ -> Ok cursor
  in
  let open Result.Let_syntax in
  let cursor =
    B.begin_
      ~previous:previous.goal_accounts
      ~before:previous.inference_ledger
      ~catalog:previous.goal_catalog
  in
  let%bind cursor = observe cursor delta in
  B.check
    cursor
    ~incoming:next.goal_accounts
    ~after:next.inference_ledger
    ~catalog:next.goal_catalog
;;

let validate_goal_descendants
      delta
      ~(previous : Session_state.t)
      ~(next : Session_state.t)
  =
  let module B = Goal_accounts.Ordered_bootstrap in
  let module O = Goal_descendants_ordered in
  let open Result.Let_syntax in
  let rec observe (accounts, cursor) = function
    | Batch deltas -> List.fold_result deltas ~init:(accounts, cursor) ~f:observe
    | Goal_catalog_changed catalog ->
      let%bind accounts = B.advance accounts (Catalog catalog) in
      let%map cursor = O.advance cursor (Catalog catalog) in
      accounts, cursor
    | Goal_accounts_changed incoming ->
      let%bind accounts = B.advance accounts (Accounts incoming) in
      let%map cursor = O.advance cursor (Accounts incoming) in
      accounts, cursor
    | Created state ->
      let%bind accounts =
        B.advance
          accounts
          (Replacement { catalog = state.goal_catalog; accounts = state.goal_accounts })
      in
      let%map cursor =
        O.advance cursor (Replacement (Session_descendant_projection.of_state state))
      in
      accounts, cursor
    | Goal_descendants_changed update ->
      let creation =
        match Goal_descendants.Update.reason update with
        | Register_new_root { goal_id; _ } -> B.creation accounts ~goal_id
        | Register_first_unrelated_child _
        | Register_child _
        | Register_branch _
        | Register_retained_branch _
        | Reserve _
        | Record_included _
        | Import _
        | Close_before_writer _ -> None
      in
      let%map cursor = O.advance cursor (Update { update; creation }) in
      accounts, cursor
    | Inference_ledger_changed ledger
    | Delegated_inference_ledger_changed (ledger, _)
    | Policy_inference_ledger_changed (ledger, _, _) ->
      let%map cursor = O.advance cursor (Canonical ledger) in
      accounts, cursor
    | Reset_generation generation ->
      let%map cursor = O.advance cursor (Reset_generation generation) in
      accounts, cursor
    | Child_contributions_changed children ->
      let%map cursor = O.advance cursor (Ordinary_children children) in
      accounts, cursor
    | Active_operation_changed operation ->
      let%map cursor = O.advance cursor (Active_operation operation) in
      accounts, cursor
    | Invocation_changed invocation | Invocation_reconciled invocation ->
      let%map cursor = O.advance cursor (Invocation invocation) in
      accounts, cursor
    | Moderator_lifecycle_claimed { receipt = execution; _ }
    | Moderator_execution_changed execution
    | Moderator_execution_reconciled execution ->
      let%map cursor = O.advance cursor (Moderator_execution execution) in
      accounts, cursor
    | Lifecycle_changed _
    | Metadata_changed _
    | Organization_changed _
    | Initial_start_consumed
    | Stop_epoch_changed _
    | Parent_stop_epoch_changed _
    | Workspace_changed _
    | Canonical_entries_appended _
    | Canonical_history_replaced _
    | Recovery_window_enabled _
    | Recovery_summary_compacted _
    | Authoring_references_forgotten _
    | Authoring_publication_changed _
    | Initial_prompt_count_changed _
    | Deferred_entries_enqueued _
    | Deferred_entries_adopted
    | Pending_inputs_changed _
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
    | Goal_policy_contributions_changed _
    | Policy_cause_changed _
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
    | Instruction_selection_changed _
    | Recovery_note_changed _
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
    | Halt_changed _ -> Ok (accounts, cursor)
  in
  let accounts =
    B.begin_
      ~previous:previous.goal_accounts
      ~before:previous.inference_ledger
      ~catalog:previous.goal_catalog
  in
  let%bind cursor = Session_descendant_projection.begin_ previous in
  let%bind _, cursor = observe (accounts, cursor) delta in
  O.check cursor ~incoming:(Session_descendant_projection.of_state next)
;;

let rec validate_descendant_publication delta ~revision =
  match delta with
  | Batch deltas ->
    List.fold_result deltas ~init:() ~f:(fun () delta ->
      validate_descendant_publication delta ~revision)
  | Goal_descendants_changed update ->
    (match Goal_descendants.Update.reason update with
     | Record_included { fact; _ } ->
       let frame =
         Goal_descendant_occurrence.frame
           (Goal_descendants.Canonical.Fact.occurrence fact)
       in
       if
         Int64.equal
           revision
           (Goal_descendant_occurrence.Frame.candidate_session_revision frame)
       then Ok ()
       else
         Error
           (Agent_protocol.Error.invalid_request
              "local Included receipt differs from published session revision")
     | Register_new_root _
     | Register_first_unrelated_child _
     | Register_child _
     | Register_branch _
     | Register_retained_branch _
     | Reserve _
     | Import _
     | Close_before_writer _ -> Ok ())
  | Created _
  | Goal_catalog_changed _
  | Goal_accounts_changed _
  | Lifecycle_changed _
  | Metadata_changed _
  | Organization_changed _
  | Initial_start_consumed
  | Stop_epoch_changed _
  | Parent_stop_epoch_changed _
  | Workspace_changed _
  | Canonical_entries_appended _
  | Canonical_history_replaced _
  | Recovery_window_enabled _
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
  | Inference_ledger_changed _
  | Policy_cause_changed _
  | Policy_inference_ledger_changed _
  | Delegated_inference_ledger_changed _
  | Goal_policy_contributions_changed _
  | Child_contributions_changed _
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
  | Halt_changed _
  | Reset_generation _ -> Ok ()
;;

let policy_usage_step delta ~(previous : Session_state.t) ~(next : Session_state.t) =
  let module O = Policy_canonical_usage.Ordered in
  let open Result.Let_syntax in
  match delta with
  | Batch _ ->
    Error (Agent_protocol.Error.invalid_request "policy evidence consumes actual leaves")
  | Created _ | Reset_generation _ ->
    let%map context = Session_state.policy_context next in
    Some
      (O.Replacement
         { context
         ; ledger = next.inference_ledger
         ; accounting = next.policy_canonical_usage
         })
  | Policy_cause_changed update -> Ok (Some (O.Metadata update))
  | Policy_inference_ledger_changed (_, _, update) ->
    Ok
      (Some
         (O.Canonical
            { before = previous.inference_ledger; after = next.inference_ledger; update }))
  | Inference_ledger_changed _ | Delegated_inference_ledger_changed _ ->
    Ok
      (Some
         (O.Ordinary_canonical
            { before = previous.inference_ledger; after = next.inference_ledger }))
  | Goal_catalog_changed _ -> Ok (Some (O.Catalog next.goal_catalog))
  | Goal_accounts_changed _ -> Ok (Some (O.Accounts next.goal_accounts))
  | Goal_descendants_changed _ -> Ok (Some (O.Descendants next.goal_descendants))
  | Moderator_lifecycle_claimed { receipt; capture } ->
    let%map context = Session_state.policy_context next in
    Some (O.Lifecycle_work { context; receipt; capture = Some capture })
  | Moderator_execution_changed receipt
    when Moderator_lifecycle.lifecycle_phase receipt.context.phase
         && not
              (List.exists previous.moderator_executions ~f:(fun execution ->
                 Agent_protocol.Id.Moderator_execution.equal
                   execution.Agent_protocol.Moderator_execution.context.id
                   receipt.context.id)) ->
    let%map context = Session_state.policy_context next in
    Some (O.Lifecycle_work { context; receipt; capture = None })
  | Job_changed _
  | Invocation_changed _
  | Invocation_reconciled _
  | Moderator_execution_changed _
  | Moderator_execution_reconciled _ ->
    let%map context = Session_state.policy_context next in
    Some (O.Source_work context)
  | Moderator_changed _ | Run_state_changed _ ->
    let%map context = Session_state.policy_context next in
    Some
      (O.Source_changed { current_source = context.current_source; runs = context.runs })
  | Lifecycle_changed _
  | Metadata_changed _
  | Organization_changed _
  | Initial_start_consumed
  | Stop_epoch_changed _
  | Parent_stop_epoch_changed _
  | Workspace_changed _
  | Canonical_entries_appended _
  | Canonical_history_replaced _
  | Recovery_window_enabled _
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
  | Goal_policy_contributions_changed _
  | Child_contributions_changed _
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
  | Instruction_selection_changed _
  | Recovery_note_changed _
  | Recurrence_catalog_changed _
  | Shell_changed _
  | History_block_reserved _
  | Compaction_generation_changed _
  | Compaction_archived _
  | History_deleted _
  | History_edited _
  | Owner_lease_generation_changed _
  | Failure_changed _
  | Halt_changed _ -> Ok None
;;

let validate_policy_usage
      ?(limits = native_limits)
      delta
      ~(previous : Session_state.t)
      ~(next : Session_state.t)
      ~revision
  =
  if not (requires_policy_tracking delta ~previous:previous.policy_canonical_usage)
  then
    if Option.is_none next.policy_canonical_usage
    then Ok ()
    else
      Error
        (Agent_protocol.Error.invalid_request
           "ordinary absent policy path became positive")
  else
    let module O = Policy_canonical_usage.Ordered in
    let open Result.Let_syntax in
    let%bind context = Session_state.policy_context previous in
    let%bind cursor =
      O.begin_
        ~previous:previous.policy_canonical_usage
        ~context
        ~ledger:previous.inference_ledger
    in
    let rec observe (state, cursor) = function
      | Batch changes -> List.fold_result changes ~init:(state, cursor) ~f:observe
      | leaf ->
        let%bind incoming = apply ~limits state leaf in
        let%bind step = policy_usage_step leaf ~previous:state ~next:incoming in
        let%map cursor =
          Option.value_map step ~default:(Ok cursor) ~f:(O.advance cursor)
        in
        incoming, cursor
    in
    let%bind _, cursor = observe (previous, cursor) delta in
    let%bind context = Session_state.policy_context next in
    let%bind () =
      O.check
        cursor
        ~incoming:next.policy_canonical_usage
        ~context
        ~ledger:next.inference_ledger
    in
    O.validate_publication cursor ~revision
;;

let goal_policy_step delta ~(previous : Session_state.t) ~(next : Session_state.t) =
  let module O = Goal_policy_ordered in
  match delta with
  | Batch _ ->
    Error
      (Agent_protocol.Error.invalid_request "Goal policy cursor consumes actual leaves")
  | Created _ | Reset_generation _ ->
    let%map.Result projection = Session_goal_policy_projection.of_state next in
    Some (O.Replacement projection)
  | Goal_catalog_changed _ -> Ok (Some (O.Catalog next.goal_catalog))
  | Goal_accounts_changed _ -> Ok (Some (O.Accounts next.goal_accounts))
  | Child_contributions_changed _ -> Ok (Some (O.Children next.child_contributions))
  | Goal_descendants_changed update -> Ok (Some (O.Descendants update))
  | Goal_policy_contributions_changed update -> Ok (Some (O.Update update))
  | Policy_cause_changed update -> Ok (Some (O.Canonical_policy update))
  | Policy_inference_ledger_changed (_, _, update) ->
    Ok
      (Some
         (O.Canonical
            { before = previous.inference_ledger
            ; after = next.inference_ledger
            ; policy = Some update
            }))
  | Inference_ledger_changed _ | Delegated_inference_ledger_changed _ ->
    Ok
      (Some
         (O.Canonical
            { before = previous.inference_ledger
            ; after = next.inference_ledger
            ; policy = None
            }))
  | Lifecycle_changed _
  | Metadata_changed _
  | Organization_changed _
  | Initial_start_consumed
  | Stop_epoch_changed _
  | Parent_stop_epoch_changed _
  | Workspace_changed _
  | Canonical_entries_appended _
  | Canonical_history_replaced _
  | Recovery_window_enabled _
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
  | Halt_changed _ -> Ok None
;;

let validate_goal_policy
      ?(limits = native_limits)
      delta
      ~(previous : Session_state.t)
      ~(next : Session_state.t)
  =
  let open Result.Let_syntax in
  if
    not
      (requires_goal_policy_tracking
         delta
         ~previous:previous.goal_policy_contributions
         ~canonical:previous.policy_canonical_usage)
  then
    if
      Option.is_none next.goal_policy_contributions
      && Option.is_none next.policy_canonical_usage
    then Ok ()
    else
      Error
        (Agent_protocol.Error.invalid_request
           "absence-only Goal policy path became positive")
  else (
    let%bind cursor = Session_goal_policy_projection.begin_ previous in
    let rec observe (state, cursor) = function
      | Batch changes -> List.fold_result changes ~init:(state, cursor) ~f:observe
      | leaf ->
        let%bind incoming = apply ~limits state leaf in
        let%bind step = goal_policy_step leaf ~previous:state ~next:incoming in
        let%map cursor =
          Option.value_map
            step
            ~default:(Ok cursor)
            ~f:(Goal_policy_ordered.advance cursor)
        in
        incoming, cursor
    in
    let%bind _, cursor = observe (previous, cursor) delta in
    let%bind projection = Session_goal_policy_projection.of_state next in
    Goal_policy_ordered.check cursor projection)
;;
