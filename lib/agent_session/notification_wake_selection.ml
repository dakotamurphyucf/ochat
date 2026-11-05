open! Core
module P = Agent_protocol

module Context = struct
  type t =
    | Ordinary
    | Goal_wake of P.Run_wake.t
end

let select (state : Session_state.t) ~context deliveries =
  let open Result.Let_syntax in
  let index = state.Session_state.run_state in
  List.map deliveries ~f:(fun (delivery : P.Delivery.t) ->
    let matching =
      Option.value_map index ~default:[] ~f:(fun index ->
        List.filter_map (Run_state.runs index) ~f:(fun run ->
          match run.P.Run.lifecycle with
          | Waiting wake ->
            (match wake.occurrence with
             | Subscription_delivery { delivery_id; _ }
               when P.Id.Delivery.equal delivery_id delivery.context.id -> Some (run, wake)
             | Subscription_delivery _ | Job_completion _ | Delivered_timer _ -> None)
          | Admitted | Active | Terminal _ -> None))
    in
    match matching with
    | [] ->
      (match context with
       | Context.Ordinary -> Ok (Some delivery)
       | Goal_wake _ -> Ok None)
    | [ (run, wake) ] ->
      let%bind () = Run_wake_owner.validate state ~run ~wake in
      let%bind attribution = Goal_work_attribution.of_run state run in
      (match context, attribution with
       | Ordinary, None -> Ok (Some delivery)
       | Ordinary, Some _ | Goal_wake _, None -> Ok None
       | Goal_wake expected, Some _ ->
         if not (P.Run_wake.equal expected wake)
         then Ok None
         else (
           let%map selected = Goal_execution_selection.of_run state run in
           match selected with
           | Eligible -> Some delivery
           | Suspended_goal _ -> None))
    | _ ->
      Error
        (P.Error.create
           Conflict
           ~message:"notification has conflicting actual Wait owners"
           ~retryable:false
           ()))
  |> Result.all
  |> Result.map ~f:List.filter_opt
;;
