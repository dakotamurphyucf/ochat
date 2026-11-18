open! Core
module P = Agent_protocol

type selection =
  { action : P.Run_action.t option
  ; goal_finish : P.Goal_achievement.Intent.t option
  }

type staged =
  { action : P.Run_action.t
  ; goal_finish : P.Goal_achievement.Intent.t option
  }

type preparation =
  | Open
  | Prepared of selection
  | Invalidated

type t =
  { scope : Run_scope.t
  ; mutable staged : staged Int.Map.t
  ; mutable preparation : preparation
  }

(* Process-unique native receipts prevent a surviving effect from one lexical
   callback from aliasing a receipt issued by another callback. This allocator
   conveys no authority; only the owning service's staged map admits a receipt. *)
let next_receipt = Atomic.make 0

let rec allocate_receipt () =
  let receipt = Atomic.get next_receipt in
  if receipt = Int.max_value
  then Error "native run receipt identities exhausted"
  else if Atomic.compare_and_set next_receipt receipt (receipt + 1)
  then Ok receipt
  else allocate_receipt ()
;;

let create ~scope = { scope; staged = Int.Map.empty; preparation = Open }
let scope t = t.scope

let close t =
  Run_scope.close t.scope;
  t.staged <- Int.Map.empty;
  t.preparation <- Invalidated
;;

let alive t =
  if Run_scope.is_open t.scope then Ok () else Error "run callback scope is closed"
;;

let stage_selection t action ~goal_finish =
  let open Result.Let_syntax in
  let%bind () = alive t in
  let%bind () =
    match t.preparation with
    | Open -> Ok ()
    | Prepared _ | Invalidated -> Error "run action selection is sealed"
  in
  let%bind () =
    Result.map_error (P.Run_action.validate action) ~f:(fun error ->
      error.P.Error.message)
  in
  let%bind () =
    match action with
    | Continue | Finish _ -> Ok ()
    | Wait wake ->
      if
        P.Id.Run.equal wake.run_id (Run_scope.run_id t.scope)
        && P.Run_source.equal wake.source (Run_scope.source t.scope)
      then Ok ()
      else Error "run wake belongs to another callback scope"
  in
  if Map.length t.staged >= P.Run_limits.max_occurrences
  then Error "run callback action bound exceeded"
  else (
    let%bind receipt = allocate_receipt () in
    t.staged <- Map.set t.staged ~key:receipt ~data:{ action; goal_finish };
    Ok receipt)
;;

let stage t action = stage_selection t action ~goal_finish:None

let stage_goal_finish t intent =
  stage_selection t (P.Goal_achievement.Intent.action intent) ~goal_finish:(Some intent)
;;

let rollback t receipt =
  if Map.mem t.staged receipt
  then (
    t.staged <- Map.remove t.staged receipt;
    match t.preparation with
    | Open | Invalidated -> ()
    | Prepared _ -> close t)
;;

let prepare t receipts =
  let open Result.Let_syntax in
  let result =
    let%bind () = alive t in
    let%bind () =
      match t.preparation with
      | Open -> Ok ()
      | Prepared _ | Invalidated -> Error "run action selection was already prepared"
    in
    if
      List.length receipts > P.Run_limits.max_occurrences
      || List.contains_dup receipts ~compare:Int.compare
    then Error "invalid surviving run action receipts"
    else
      List.fold_result
        receipts
        ~init:({ action = None; goal_finish = None } : selection)
        ~f:(fun (selected : selection) receipt ->
          let%bind (staged : staged) =
            Map.find t.staged receipt
            |> Result.of_option ~error:"foreign or rolled-back run action receipt"
          in
          let%bind action =
            P.Run_action.combine selected.action (Some staged.action)
            |> Result.map_error ~f:(fun error -> error.P.Error.message)
          in
          let%map goal_finish =
            match selected.goal_finish, staged.goal_finish with
            | Some _, Some _ -> Error "multiple surviving goal finish requests"
            | None, intent | intent, None -> Ok intent
          in
          ({ action; goal_finish } : selection))
  in
  match result with
  | Error message ->
    close t;
    Error message
  | Ok selected ->
    t.preparation <- Prepared selected;
    Ok selected.action
;;

let prepared t =
  let open Result.Let_syntax in
  let%bind () = alive t in
  match t.preparation with
  | Prepared selected -> Ok selected.action
  | Open | Invalidated -> Error "run callback has no prepared action selection"
;;

let prepared_goal_finish t =
  let%bind.Result () = alive t in
  match t.preparation with
  | Prepared selected -> Ok selected.goal_finish
  | Open | Invalidated -> Error "run callback has no prepared action selection"
;;

let verify t selected =
  let%bind.Result actual = prepared t in
  if Option.equal P.Run_action.equal actual selected
  then Ok ()
  else Error "run outcome disagrees with its prepared callback selection"
;;

let transaction t : Chat_response.Run_operations.transaction =
  { handlers =
      { stage = stage t; stage_goal_finish = stage_goal_finish t; rollback = rollback t }
  ; prepare = prepare t
  }
;;

let compose_requests t ~action ~(requests : P.Invocation.follow_up) =
  let open Result.Let_syntax in
  let%bind () = verify t action in
  match action with
  | None -> Ok requests
  | Some Continue ->
    (match requests.end_session with
     | Some _ -> Error "run continue cannot also end the session"
     | None -> Ok { requests with request_turn = true })
  | Some (Wait _ | Finish _) ->
    if requests.request_turn
    then Error "run wait or finish cannot also request a turn"
    else Ok requests
;;
