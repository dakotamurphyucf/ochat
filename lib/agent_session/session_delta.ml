open! Core

type t =
  | Batch of t list
  | Created of Session_state.t
  | Lifecycle_changed of Session_state.Lifecycle.t
  | Workspace_changed of Workspace_instance.t
  | Canonical_entries_appended of Agent_protocol.History.entry list
  | Canonical_history_replaced of Agent_protocol.History.entry list
  | Initial_prompt_count_changed of int
  | Deferred_entries_enqueued of Agent_protocol.History.entry list
  | Deferred_entries_adopted
  | Active_operation_changed of Agent_protocol.Operation.t option
  | Attachment_added of Agent_protocol.Session.Attachment.t
  | Attachment_removed of Agent_protocol.Id.Attachment.t
  | Permission_changed of Agent_protocol.Permission.t
  | Grant_changed of Agent_protocol.Grant.t
  | Job_changed of Agent_protocol.Job.t
  | Schedule_changed of Agent_protocol.Schedule.t
  | Invocation_changed of Agent_protocol.Invocation.t
  | Invocation_reconciled of Agent_protocol.Invocation.t
  | Moderator_execution_changed of Agent_protocol.Moderator_execution.t
  | Moderator_execution_reconciled of Agent_protocol.Moderator_execution.t
  | Subscription_changed of Agent_protocol.Subscription.t
  | Delivery_changed of Agent_protocol.Delivery.t
  | Delivery_committed of Agent_protocol.Delivery.t * Agent_protocol.History.entry
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

let rec apply state = function
  | Batch deltas -> List.fold_result deltas ~init:state ~f:apply
  | Created created -> Session_state.upgrade_schema created
  | Lifecycle_changed lifecycle -> Ok { state with lifecycle }
  | Workspace_changed workspace_instance ->
    let quota_key =
      Option.map state.spec.quota_key ~f:(fun quota_key ->
        { quota_key with Quota_key.conflict_domain = workspace_instance.conflict_domain })
    in
    Ok { state with spec = { state.spec with workspace_instance; quota_key } }
  | Canonical_entries_appended entries ->
    Ok
      { state with
        conversation =
          { state.conversation with
            canonical_history = state.conversation.canonical_history @ entries
          }
      }
  | Canonical_history_replaced canonical_history ->
    Ok { state with conversation = { state.conversation with canonical_history } }
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
  | Active_operation_changed active_operation -> Ok { state with active_operation }
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
    Ok
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
      if not recovery
      then Ok ()
      else (
        match previous, invocation.status with
        | Some { status = Admitted | Dispatching; _ }, Resolved (Cancelled _) -> Ok ()
        | Some { status = Resolved _; _ }, Published _ -> Ok ()
        | Some { status = Resolved _; _ }, Resolved _
          when Option.is_some invocation.publication_discarded -> Ok ()
        | Some previous, _
          when (match previous.observation, invocation.observation with
                | ( Some { status = Observing; _ }
                  , Some { status = Observation_failed _; _ } ) -> true
                | ( Some
                      { status = Observed
                      ; follow_up =
                          Some (Pending_follow_up _ | Compaction_accepted_follow_up _)
                      ; _
                      }
                  , Some
                      { status = Observed; follow_up = Some (Discarded_follow_up _); _ } )
                  -> true
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
  | Subscription_changed subscription ->
    let open Result.Let_syntax in
    let c = subscription.Agent_protocol.Subscription.context in
    let%bind () =
      Extension_invariants.owner
        ~session_id:state.identity.session_id
        ~generation:state.identity.generation
        c.session_id
        c.generation
    in
    let previous =
      List.find state.subscriptions ~f:(fun old ->
        Agent_protocol.Id.Subscription.compare old.context.id c.id = 0)
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
      Extension_invariants.delivery_ready ~invocations:state.invocations delivery
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
    then Ok state
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
    else Ok { state with identity = { state.identity with generation } }
;;
