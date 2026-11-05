open! Core
module A = Agent_session
module P = Agent_protocol
module Actor = A.Session_actor
module Factory = Session_factory

module Activation_result = struct
  type t =
    | Activated of P.Run_receipt.t
    | Retained of P.Run_receipt.t
    | Admission_uncertain of P.Error.t
    | Activation_failed of
        { admission : P.Run_receipt.t
        ; error : P.Error.t
        }
end

module Callback_success = struct
  type t =
    | Activated of P.Run_receipt.t
    | Retained of P.Run_receipt.t
end

module Admission_fact = struct
  type t =
    | Before_admission
    | Uncertain
    | Known of P.Run_receipt.t
end

module Attempt = struct
  type t =
    { actual : Callback_success.t Factory.Deferred_run_attempt.t
    ; fact : Admission_fact.t
    }

  let result t =
    match Factory.Deferred_run_attempt.result t.actual with
    | Ok (Callback_success.Activated admission) ->
      Ok (Activation_result.Activated admission)
    | Ok (Retained admission) -> Ok (Activation_result.Retained admission)
    | Error error ->
      (match t.fact with
       | Before_admission -> Error error
       | Uncertain -> Ok (Activation_result.Admission_uncertain error)
       | Known admission -> Ok (Activation_result.Activation_failed { admission; error }))
  ;;

  let no_effects t = Factory.Deferred_run_attempt.no_effects t.actual
end

module Outcome = struct
  type t =
    | Retained of P.Run_receipt.t
    | Attempted of Attempt.t
end

type t =
  { factory : Factory.t
  ; authorize : Operator_authorization.t -> A.Session_state.t -> (unit, P.Error.t) result
  }

let create ~factory ~authorize = { factory; authorize }

module Context = struct
  type service = t

  type t =
    { service : service
    ; entry : Session_registry.entry
    ; actor : Operator_authorization.t
    ; session : P.Session_ref.t
    ; request : A.Goal_activation_request.t
    }

  let check_entry t =
    Factory.check_registered_entry t.service.factory t.entry ~session:t.session
  ;;

  let authorize t state =
    let open Result.Let_syntax in
    let%bind () = check_entry t in
    let%bind () = t.service.authorize t.actor state in
    check_entry t
  ;;

  let receipt t =
    let open Result.Let_syntax in
    let%bind () = check_entry t in
    Actor.goal_run_receipt
      t.entry.actor
      ~actor:t.actor
      ~authorize:(authorize t)
      ~request:t.request
      ~session:t.session
  ;;

  let validate_retained t admission =
    let%bind.Result actual = receipt t in
    if Option.exists actual ~f:(P.Run_receipt.equal admission)
    then Ok admission
    else
      Error
        (P.Error.create
           Conflict
           ~message:"retained Run does not match the requested Goal association"
           ~retryable:false
           ())
  ;;
end

let activate context prepared activation =
  let held = Actor.Goal_activation.held activation in
  let checked = Actor.Goal_activation.checked activation in
  match (A.Goal_activation_request.request context.Context.request).input with
  | Authored_start ->
    Runtime_owner.start_held_authored_runtime
      context.entry.runtime
      ~prepared
      ~held
      ~checked
  | User_submission _ ->
    Runtime_owner.with_held_input_runtime
      context.entry.runtime
      ~prepared
      ~held
      ~checked
      (fun runtime ->
         Runtime_owner.Held_input_runtime.submit runtime ~prepare:(fun ~runtime ~scope ->
           Factory.prepare_held_input
             context.service.factory
             ~entry:context.entry
             ~runtime
             ~scope))
;;

let admit_selected context fact prepared =
  let open Result.Let_syntax in
  let%bind () = Context.check_entry context in
  let%bind decision =
    Actor.admit_held_goal_run
      context.Context.entry.actor
      ~binding:(Runtime_owner.Prepared.binding prepared)
      ~intent:(A.Goal_activation_request.intent context.request)
      ~session:context.session
  in
  match decision with
  | Rejected error -> Error error
  | Uncertain error ->
    fact := Admission_fact.Uncertain;
    Error error
  | Retained admission ->
    fact := Admission_fact.Known admission;
    Ok (Callback_success.Retained admission)
  | Activation_unavailable { admission; error } ->
    fact := Admission_fact.Known admission;
    Error error
  | Admitted activation ->
    fact
    := Admission_fact.Known
         (Actor.Held_run.admission (Actor.Goal_activation.held activation));
    let%map admission = activate context prepared activation in
    Callback_success.Activated admission
;;

let start_episode service ~entry ~actor ~session ~request =
  let open Result.Let_syntax in
  let context = { Context.service; entry; actor; session; request } in
  let%bind receipt = Context.receipt context in
  match receipt with
  | Some admission -> Ok (Outcome.Retained admission)
  | None ->
    let fact = ref Admission_fact.Before_admission in
    let%bind outcome =
      Factory.with_effect_deferred_run_source
        service.factory
        entry
        ~actor
        ~authorize:(Context.authorize context)
        ~request:(A.Goal_activation_request.request request)
        (fun _deferred prepared -> admit_selected context fact prepared)
    in
    (match outcome with
     | Retained admission ->
       let%map admission = Context.validate_retained context admission in
       Outcome.Retained admission
     | Attempted actual -> Ok (Outcome.Attempted { Attempt.actual; fact = !fact }))
;;

module Resume_result = Actor.Goal_resume_result

let resume_episode service ~entry ~actor ~session ~request =
  let module Source = Factory.Goal_resume_source in
  let open Result.Let_syntax in
  let check_entry () = Factory.check_registered_entry service.factory entry ~session in
  let authorize state =
    let%bind () = check_entry () in
    let%bind () = service.authorize actor state in
    check_entry ()
  in
  let%bind () = check_entry () in
  let%bind retained =
    Actor.goal_resume_receipt
      entry.Session_registry.actor
      ~actor
      ~authorize
      ~session
      ~request
  in
  match retained with
  | Some receipt -> Ok (Resume_result.Retained receipt)
  | None ->
    let uncertain = ref false in
    let%bind report =
      Factory.with_goal_resume_source_reported
        service.factory
        entry
        ~actor
        ~authorize
        ~session
        ~request
        (fun prepared ->
           let%bind () = check_entry () in
           let%bind decision =
             Actor.admit_goal_resume
               entry.actor
               ~binding:(Runtime_owner.Prepared_goal_continuation.binding prepared)
           in
           let finish = function
             | (Resume_result.Resumed _ | Retained _) as result -> Ok result
             | Resume_failed { receipt = _; error } | Rejected error -> Error error
             | Uncertain error ->
               uncertain := true;
               Error error
           in
           match decision with
           | Retained receipt -> Ok (Resume_result.Retained receipt)
           | Waiting { receipt; wake } ->
             Ok (Resume_result.Resumed { receipt; continuation = Waiting wake })
           | Rejected error -> Error error
           | Uncertain error ->
             uncertain := true;
             Error error
           | Resume_failed { receipt = _; error } -> Error error
           | Ready continuation ->
             Runtime_owner.resume_goal_episode entry.runtime ~prepared ~continuation
             |> Result.bind ~f:finish)
    in
    (match Source.result report with
     | Ok (Retained receipt) -> Ok (Resume_result.Retained receipt)
     | Ok (Fresh result) -> Ok result
     | Error error ->
       (match Source.receipt report with
        | Some receipt -> Ok (Resume_result.Resume_failed { receipt; error })
        | None ->
          Ok
            (if !uncertain
             then Resume_result.Uncertain error
             else Resume_result.Rejected error)))
;;
