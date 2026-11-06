open! Core

module Inherited = struct
  type t =
    { occurrence : Goal_descendant_occurrence.t
    ; receipts : Goal_descendant_included.t list
    }

  let create ~occurrence ~receipts =
    let open Result.Let_syntax in
    let invalid message = Error (Agent_protocol.Error.invalid_request message) in
    let%bind () =
      match Goal_descendant_occurrence.kind occurrence with
      | Host_turn _ -> Ok ()
      | Attempt _ -> invalid "inherited Turn input requires a Host_turn occurrence"
    in
    let%bind () =
      if List.is_empty receipts
      then invalid "inherited Turn input requires nonempty receipts"
      else Ok ()
    in
    let%map _ =
      List.fold_result receipts ~init:None ~f:(fun previous included ->
        let pending = Goal_descendant_included.pending included in
        let scope = Goal_descendant_pending.scope pending in
        if
          not
            (Goal_descendant_occurrence.equal
               occurrence
               (Goal_descendant_pending.occurrence pending))
        then invalid "inherited Turn receipt names a different occurrence"
        else (
          match previous with
          | None -> Ok (Some scope)
          | Some previous ->
            if Agent_store.Delegation_goal_origin.Scope.compare_root previous scope < 0
            then Ok (Some scope)
            else invalid "inherited Turn receipts are not strictly root-ordered"))
    in
    { occurrence; receipts }
  ;;
end

module Inherited_edge = struct
  type t =
    { before : Inference_ledger.t
    ; after : Inference_ledger.t
    ; canonical : Goal_descendants.Canonical.t
    }

  let before t = t.before
  let after t = t.after
  let canonical t = t.canonical
end

module Ledger_state = struct
  type t =
    { ledger : Inference_ledger.t
    ; accounts : Goal_accounts.t option
    ; delegated_usage : Delegated_usage.t option
    ; descendants : Goal_descendants.t option
    ; children : Child_contributions.t option
    ; inherited_edge : Inherited_edge.t option
    ; accounts_changed : bool
    ; changes_rev : Session_delta.t list
    }
end

let apply_internal ?inherited_turn ~previous state delta payloads =
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
    { current with
      Ledger_state.ledger = after
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
        let%bind () =
          match inherited_turn with
          | None -> Ok ()
          | Some inherited ->
            if Option.is_some current.inherited_edge
            then invalid "inherited Turn input was already consumed"
            else (
              match Goal_descendant_occurrence.kind inherited.Inherited.occurrence with
              | Attempt _ -> invalid "inherited Turn input is not a Host_turn"
              | Host_turn { operation_id } ->
                if Agent_protocol.Id.Operation.equal operation_id operation.id
                then Ok ()
                else invalid "inherited Turn input names another operation")
        in
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
        let%bind reconciled = reconcile current after ~admission in
        match inherited_turn with
        | None -> Ok reconciled
        | Some inherited ->
          let%bind accounting, children =
            match current.descendants, current.children with
            | Some accounting, Some children -> Ok (accounting, children)
            | None, (None | Some _) | Some _, None ->
              invalid "inherited Turn input lacks actual descendant accounting"
          in
          (* This is the genuine immediate admission edge, not the transaction's
             original ledger or a flattened final projection. Earlier canonical
             leaves/receipts and each terminal edge retain their own progression. *)
          let canonical =
            Goal_descendants.Canonical.Host_turn (operation, handle, tracking)
          in
          let%map update =
            Goal_descendants.record_included
              accounting
              ~children
              ~before:ledger
              ~after
              ~canonical
              ~occurrence:inherited.occurrence
              ~receipts:inherited.receipts
          in
          let children =
            match Goal_descendants.Update.children update with
            | Preserve -> current.children
            | Replace children -> Some children
          in
          { reconciled with
            descendants = Some (Goal_descendants.Update.accounting update)
          ; children
          ; inherited_edge = Some { Inherited_edge.before = ledger; after; canonical }
          ; changes_rev = Goal_descendants_changed update :: reconciled.changes_rev
          })
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
  let%bind final =
    List.fold_result
      payloads
      ~init:
        { Ledger_state.ledger = state.Session_state.inference_ledger
        ; accounts = state.goal_accounts
        ; delegated_usage = state.delegated_usage
        ; descendants = state.goal_descendants
        ; children = state.child_contributions
        ; inherited_edge = None
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
  let%map () =
    match inherited_turn with
    | None -> Ok ()
    | Some _ ->
      if Option.is_some final.inherited_edge
      then Ok ()
      else invalid "inherited Turn input lacks one actual matching Turn start"
  in
  let { Ledger_state.ledger = inference_ledger
      ; accounts = goal_accounts
      ; delegated_usage
      ; descendants = goal_descendants
      ; children = child_contributions
      ; inherited_edge
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
  then state, delta, inherited_edge
  else (
    let state =
      { state with
        inference_ledger
      ; goal_accounts
      ; delegated_usage
      ; goal_descendants
      ; child_contributions
      }
    in
    (* Retain each actual edge. Collapsing a finish and later retirement into
       one before/after leaf would erase the row needed by the cumulative fold.
       Session_transition runs replacement reconciliation before this phase. *)
    let delta = Session_delta.Batch (delta :: List.rev changes_rev) in
    state, delta, inherited_edge)
;;

let apply ?inherited_turn ~previous state delta payloads =
  let%map.Result state, delta, _ =
    apply_internal ?inherited_turn ~previous state delta payloads
  in
  state, delta
;;

let apply_inherited ~inherited_turn ~previous state delta payloads =
  let open Result.Let_syntax in
  let%bind state, delta, edge =
    apply_internal ~inherited_turn ~previous state delta payloads
  in
  let%map edge =
    Result.of_option
      edge
      ~error:
        (Agent_protocol.Error.invalid_request
           "inherited Turn input lacks one actual matching Turn edge")
  in
  state, delta, edge
;;
