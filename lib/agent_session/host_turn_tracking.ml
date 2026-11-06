open! Core

module Ledger_state = struct
  type t =
    { ledger : Inference_ledger.t
    ; accounts : Goal_accounts.t option
    ; delegated_usage : Delegated_usage.t option
    ; accounts_changed : bool
    ; changes_rev : Session_delta.t list
    }
end

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
  let reconcile (current : Ledger_state.t) after ~admission =
    let%map prepared =
      Canonical_usage_update.prepare
        current.ledger
        ~after
        ~accounts:current.accounts
        ~catalog:state.Session_state.goal_catalog
        ~delegated_usage:current.delegated_usage
        ~admission
    in
    let accounts = Canonical_usage_update.accounts prepared in
    let delegated_usage = Canonical_usage_update.delegated_usage prepared in
    let accounts_changed = Canonical_usage_update.accounts_changed prepared in
    let changes =
      Session_delta.inference_ledger_update after ~delegated_usage
      ::
      (if accounts_changed
       then
         Option.to_list
           (Option.map accounts ~f:(fun accounts ->
              Session_delta.Goal_accounts_changed accounts))
       else [])
    in
    { Ledger_state.ledger = after
    ; accounts
    ; delegated_usage
    ; accounts_changed = current.accounts_changed || accounts_changed
    ; changes_rev = List.rev_append changes current.changes_rev
    }
  in
  let started ({ Ledger_state.ledger; _ } as current) operation =
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
  let finished ({ Ledger_state.ledger; _ } as current) payload operation =
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
  let%map final =
    List.fold_result
      payloads
      ~init:
        { Ledger_state.ledger = state.Session_state.inference_ledger
        ; accounts = state.goal_accounts
        ; delegated_usage = state.delegated_usage
        ; accounts_changed = false
        ; changes_rev = []
        }
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
  let { Ledger_state.ledger = inference_ledger
      ; accounts = goal_accounts
      ; delegated_usage
      ; accounts_changed
      ; changes_rev
      }
    =
    final
  in
  if
    (not accounts_changed)
    && Int64.equal
         (Inference_ledger.revision inference_ledger)
         (Inference_ledger.revision state.inference_ledger)
  then state, delta
  else (
    let state = { state with inference_ledger; goal_accounts; delegated_usage } in
    (* Retain each actual edge. Collapsing a finish and later retirement into
       one before/after leaf would erase the row needed by the cumulative fold.
       Session_transition runs replacement reconciliation before this phase. *)
    let delta = Session_delta.Batch (delta :: List.rev changes_rev) in
    state, delta)
;;
