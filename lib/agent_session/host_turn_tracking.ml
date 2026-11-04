open! Core

let apply ~previous state delta payloads =
  let open Result.Let_syntax in
  let ledger_error _ =
    Agent_protocol.Error.create
      Invalid_state
      ~message:"host turn tracking admission failed"
      ~retryable:false
      ()
  in
  let actual_operation operation active =
    Option.exists active ~f:(fun active ->
      Agent_protocol.Id.Operation.equal
        active.Agent_protocol.Operation.id
        operation.Agent_protocol.Operation.id
      && Int.equal active.generation operation.generation
      && Agent_protocol.Operation.equal_kind active.kind operation.kind)
  in
  let admit_operation operation =
    Agent_protocol.Operation.of_json (Agent_protocol.Operation.to_json operation)
    |> Result.map ~f:ignore
  in
  let invalid message = Error (Agent_protocol.Error.invalid_request message) in
  let reconcile (ledger, accounts, changed) after ~admission =
    let%map prepared =
      Goal_ledger_reconciliation.prepare
        ~accounts
        ~catalog:state.Session_state.goal_catalog
        ~before:ledger
        ~after
        ~admission
    in
    ( after
    , Goal_ledger_reconciliation.accounts prepared
    , changed || Goal_ledger_reconciliation.changed prepared )
  in
  let started ((ledger, _, _) as current) operation =
    let%bind () = admit_operation operation in
    let%bind () =
      match operation.Agent_protocol.Operation.state with
      | Starting | Running -> Ok ()
      | Cancelling | Completed | Failed _ | Cancelled | Interrupted _ ->
        invalid "operation start has a non-started state"
    in
    match operation.kind with
    | Compaction -> Ok current
    | Turn _ ->
      let exact_active =
        Option.exists state.Session_state.active_operation ~f:(fun active ->
          String.equal
            (Jsonaf.to_string (Agent_protocol.Operation.to_json active))
            (Jsonaf.to_string (Agent_protocol.Operation.to_json operation)))
      in
      if
        exact_active
        && not (actual_operation operation previous.Session_state.active_operation)
      then (
        let%bind after, handle, tracking =
          Inference_ledger.admit_turn ledger operation |> Result.map_error ~f:ledger_error
        in
        let%bind work =
          Agent_protocol.Run_work.create
            ~key:(Operation operation.id)
            ~generation:operation.generation
        in
        let%bind attribution = Goal_work_attribution.of_work state work in
        let admission =
          Option.map attribution ~f:(fun attribution ->
            ( Goal_work_attribution.goal_id attribution
            , Goal_accounting.Admission.Turn
                { episode = Goal_work_attribution.episode attribution
                ; operation
                ; handle
                ; tracking
                } ))
        in
        reconcile current after ~admission)
      else invalid "host turn start lacks actual operation admission"
  in
  let finished ((ledger, _, _) as current) payload operation =
    let%bind () = admit_operation operation in
    let consistent =
      match payload, operation.Agent_protocol.Operation.state with
      | Agent_protocol.Event.Durable.Payload.Operation_completed _, Completed
      | Operation_failed _, Failed _
      | Operation_cancelled _, Cancelled
      | Operation_interrupted _, Interrupted _ -> true
      | _ -> false
    in
    let%bind () =
      if consistent
      then Ok ()
      else invalid "operation terminal payload differs from its actual state"
    in
    match operation.kind with
    | Compaction -> Ok current
    | Turn _ ->
      if actual_operation operation state.active_operation
      then invalid "host turn terminal did not retire the active operation"
      else if not (actual_operation operation previous.active_operation)
      then Ok current
      else (
        let%bind () =
          if
            Option.exists previous.active_operation ~f:(fun active ->
              Agent_protocol.Timestamp.compare active.started_at operation.started_at = 0
              && Agent_protocol.Timestamp.compare operation.updated_at active.updated_at
                 >= 0)
          then Ok ()
          else invalid "host turn terminal changed operation occurrence"
        in
        match
          Inference_ledger.find_turn_handle
            ledger
            ~operation_id:operation.id
            ~generation:operation.generation
        with
        | None -> Ok current
        | Some handle ->
          let%bind after =
            Inference_ledger.finish_turn ledger handle operation
            |> Result.map_error ~f:ledger_error
          in
          reconcile current after ~admission:None)
  in
  let%map inference_ledger, goal_accounts, accounts_changed =
    List.fold_result
      payloads
      ~init:(state.Session_state.inference_ledger, state.goal_accounts, false)
      ~f:(fun ledger payload ->
        match payload with
        | Agent_protocol.Event.Durable.Payload.Operation_started operation ->
          started ledger operation
        | Operation_completed operation
        | Operation_failed operation
        | Operation_cancelled operation
        | Operation_interrupted operation -> finished ledger payload operation
        | _ -> Ok ledger)
  in
  if
    (not accounts_changed)
    && Int64.equal
         (Inference_ledger.revision inference_ledger)
         (Inference_ledger.revision state.inference_ledger)
  then state, delta
  else (
    let state = { state with inference_ledger; goal_accounts } in
    let delta =
      match Session_replacement_delta.classify delta with
      | Some replacement -> Session_replacement_delta.with_state replacement state
      | None ->
        let accounting =
          if accounts_changed
          then
            Option.to_list
              (Option.map goal_accounts ~f:(fun accounts ->
                 Session_delta.Goal_accounts_changed accounts))
          else []
        in
        Session_delta.Batch
          (delta :: Session_delta.Inference_ledger_changed inference_ledger :: accounting)
    in
    state, delta)
;;
