open Core
module P = Agent_protocol
module M = Managed_submission

let apply ~previous ~(state : Session_state.t) ~delta ~payloads ~now =
  let open Result.Let_syntax in
  let previous_receipts = previous.Session_state.managed_submissions in
  let%bind receipts =
    match delta with
    | Session_delta.Created _ ->
      List.fold_result
        previous_receipts
        ~init:state.managed_submissions
        ~f:(fun values receipt ->
          match List.find values ~f:(M.same_key receipt) with
          | Some retained ->
            Result.map (M.validate_transition ~previous:receipt retained) ~f:(fun () ->
              values)
          | None -> Ok (values @ [ receipt ]))
    | _ -> Ok state.managed_submissions
  in
  if List.is_empty receipts
  then Ok (state, delta)
  else (
    let terminals =
      List.filter_map payloads ~f:(function
        | P.Event.Durable.Payload.Operation_completed operation ->
          Some (operation.id, M.Completed)
        | Operation_failed operation -> Some (operation.id, M.Failed)
        | Operation_cancelled operation -> Some (operation.id, M.Cancelled)
        | Operation_interrupted operation -> Some (operation.id, M.Interrupted)
        | _ -> None)
    in
    let operation =
      match state.active_operation with
      | Some _ as operation -> operation
      | None ->
        Option.filter previous.active_operation ~f:(fun operation ->
          List.Assoc.mem terminals operation.id ~equal:P.Id.Operation.equal)
    in
    let ids entries =
      Hash_set.of_list
        (module P.History.Id)
        (List.map entries ~f:(fun entry -> entry.P.History.id))
    in
    let previous_ids = ids previous.conversation.canonical_history in
    let canonical_ids = ids state.conversation.canonical_history in
    let contains ids id = Hash_set.mem ids id in
    let canonical = state.conversation.canonical_history in
    let appended =
      List.filter canonical ~f:(fun entry -> not (contains previous_ids entry.id))
    in
    let compacted =
      state.conversation.compaction_generation
      > previous.conversation.compaction_generation
    in
    let reconcile receipt =
      let discarded =
        (not compacted)
        && contains previous_ids receipt.M.history_id
        && not (contains canonical_ids receipt.history_id)
      in
      M.reconcile
        ~generation:state.identity.generation
        ~reference:state.spec.delegation
        ~discarded
        ~adopted:(contains canonical_ids receipt.history_id)
        ~appended
        ~operation
        ~terminals
        ~now
        receipt
    in
    let next = List.map receipts ~f:reconcile in
    let%bind () =
      List.map2_exn receipts next ~f:(fun previous value ->
        M.validate_transition ~previous value)
      |> Result.all_unit
    in
    let changes =
      List.filter_map (List.zip_exn receipts next) ~f:(fun (before, after) ->
        if M.equal before after
        then None
        else Some (Session_delta.Managed_submission_changed after))
    in
    let state = { state with managed_submissions = next } in
    match delta with
    | Session_delta.Created _ -> Ok (state, Session_delta.Created state)
    | _ ->
      Ok
        ( state
        , if List.is_empty changes then delta else Session_delta.Batch (delta :: changes)
        ))
;;
