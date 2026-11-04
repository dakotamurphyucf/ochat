open! Core
module P = Agent_protocol

type decision =
  | Allow
  | Held of
      { goal_id : P.Id.Goal.t
      ; delta : Session_delta.t option
      }

let prepare ~previous ~(candidate : Session_state.t) ~payloads ~now =
  let open Result.Let_syntax in
  let basis =
    { candidate with
      inference_ledger = previous.Session_state.inference_ledger
    ; goal_accounts = previous.goal_accounts
    }
  in
  List.fold_result payloads ~init:Allow ~f:(fun decision payload ->
    match decision, payload with
    | Held _, _ -> Ok decision
    | Allow, P.Event.Durable.Payload.Operation_started operation ->
      (match operation.kind with
       | Compaction -> Ok Allow
       | Turn _ ->
         let%bind work =
           P.Run_work.create
             ~key:(Operation operation.id)
             ~generation:operation.generation
         in
         let%bind attribution = Goal_work_attribution.of_work candidate work in
         let%bind admission =
           Goal_charge_admission.prepare basis ~attribution ~now ~dimension:Host_turn
         in
         (match admission with
          | Unbound | Ready _ -> Ok Allow
          | Held { goal_id; reason; delta = _ } ->
            let%map delta = Goal_charge_admission.hold_delta previous ~goal_id ~reason in
            Held { goal_id; delta }))
    | Allow, _ -> Ok Allow)
;;
