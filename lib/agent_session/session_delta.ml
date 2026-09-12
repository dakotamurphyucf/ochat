open! Core

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
  | Initial_prompt_count_changed of int
  | Deferred_entries_enqueued of Agent_protocol.History.entry list
  | Deferred_entries_adopted
  | Active_operation_changed of Agent_protocol.Operation.t option
  | Automatic_turn_budget_enabled of Chat_response.Runtime_semantics.policy
  | Automatic_turn_pauses_changed of Chat_response.Runtime_semantics.pause_condition list
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
  | Moderator_execution_changed of Agent_protocol.Moderator_execution.t
  | Moderator_execution_reconciled of Agent_protocol.Moderator_execution.t
  | Subscription_changed of Agent_protocol.Subscription.t
  | Subscription_expired of Agent_protocol.Subscription.t
  | Subscription_cancelled of Agent_protocol.Subscription.t
  | Delivery_changed of Agent_protocol.Delivery.t
  | Ingress_changed of External_ingress.t
  | Delivery_committed of Agent_protocol.Delivery.t * Agent_protocol.History.entry
  | Delivery_wake_changed of Agent_protocol.Delivery.t
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

let rec apply state = function
  | Batch deltas -> List.fold_result deltas ~init:state ~f:apply
  | Created created -> Session_state.upgrade_schema created
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
  | Canonical_entries_appended entries ->
    let open Result.Let_syntax in
    let%map authoring_reference_index = remember_authoring state [ entries ] in
    { state with
      conversation =
        { state.conversation with
          canonical_history = state.conversation.canonical_history @ entries
        ; authoring_reference_index
        }
    }
  | Canonical_history_replaced canonical_history ->
    let open Result.Let_syntax in
    (* Observe the outgoing history at the replacement boundary as well. This
       bootstraps legacy snapshots without scanning archives or every old entry
       on each ordinary append. Both changes are part of the same transaction. *)
    let%map authoring_reference_index =
      remember_authoring state [ state.conversation.canonical_history; canonical_history ]
    in
    { state with
      conversation =
        { state.conversation with canonical_history; authoring_reference_index }
    }
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
  | Compaction_archived archive ->
    Ok
      { state with
        conversation =
          { state.conversation with
            compaction_archives = archive :: state.conversation.compaction_archives
          }
      }
  | Deferred_entries_enqueued entries ->
    Ok
      { state with
        conversation =
          { state.conversation with
            deferred_user_entries = state.conversation.deferred_user_entries @ entries
          }
      }
  | Deferred_entries_adopted ->
    Ok
      { state with
        conversation =
          { state.conversation with
            canonical_history =
              state.conversation.canonical_history
              @ state.conversation.deferred_user_entries
          ; deferred_user_entries = []
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
  | Job_changed job ->
    let open Result.Let_syntax in
    let previous =
      List.find state.jobs ~f:(fun previous ->
        Agent_protocol.Id.Job.equal previous.id job.id)
    in
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
    else
      Ok
        { state with
          deliveries =
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
        }
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
    let%map () = E.validate_transition ~previous execution in
    { state with
      moderator_executions =
        replace_by
          Agent_protocol.Id.Moderator_execution.compare
          c.id
          execution
          state.moderator_executions
          ~id_of:(fun e -> e.E.context.id)
    }
  | Moderator_changed moderator -> Ok { state with moderator }
  | Shell_changed shell -> Ok { state with shell }
  | History_block_reserved reserved_history_through ->
    if Int64.(reserved_history_through < state.conversation.reserved_history_through)
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
    else
      Ok
        { state with
          identity = { state.identity with generation }
        ; pending_initial_start = false
        ; conversation = { state.conversation with authoring_reference_index = None }
        }
;;
