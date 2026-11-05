open! Core
module A = Agent_session
module P = Agent_protocol
module Actor = A.Session_actor
module Factory = Session_factory
module Intent = A.Recurrence_execution_intent
module Execution_attempt = A.Recurrence_execution_attempt

module Retained = struct
  type t =
    { admission : P.Run_receipt.t
    ; reconciliation : (Actor.Recurrence_import.t, P.Error.t) result
    }
end

module Activation_result = struct
  type t =
    | Activated of P.Run_receipt.t
    | Retained of Retained.t
    | Admission_uncertain of P.Error.t
    | Activation_failed of
        { admission : P.Run_receipt.t
        ; error : P.Error.t
        }
end

(* Callback success cannot contain a negative disposition. Every actual lower
   error crosses the full Factory cleanup as Error before any classification. *)
module Callback_success = struct
  type t =
    | Activated of P.Run_receipt.t
    | Retained of
        { admission : P.Run_receipt.t
        ; reconciliation : Actor.Recurrence_import.t
        }
end

module Admission_fact = struct
  type t =
    | Before_admission
    | Uncertain
    | Retained of P.Run_receipt.t
    | Admitted of P.Run_receipt.t
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
    | Ok (Retained { admission; reconciliation }) ->
      Ok (Activation_result.Retained { admission; reconciliation = Ok reconciliation })
    | Error error ->
      (match t.fact with
       | Before_admission -> Error error
       | Uncertain -> Ok (Activation_result.Admission_uncertain error)
       | Retained admission ->
         Ok (Activation_result.Retained { admission; reconciliation = Error error })
       | Admitted admission ->
         Ok (Activation_result.Activation_failed { admission; error }))
  ;;

  let no_effects t = Factory.Deferred_run_attempt.no_effects t.actual
  let retry t = Factory.Deferred_run_attempt.retry t.actual
end

module Outcome = struct
  type t =
    | Retained of Retained.t
    | Attempted of Attempt.t
end

module Reconciliation = struct
  type t =
    | Absent
    | Retained of Retained.t
end

type t =
  { factory : Factory.t
  ; authorize : Operator_authorization.t -> A.Session_state.t -> (unit, P.Error.t) result
  ; automatic_policy :
      A.Session_state.t -> A.Recurrence_definition.t -> (bool, P.Error.t) result
  }

let create ~factory ~authorize ~automatic_policy =
  { factory; authorize; automatic_policy }
;;

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

module Context = struct
  type service = t

  type t =
    { service : service
    ; controller : Session_registry.entry
    ; controller_actor : Operator_authorization.t
    ; target : Session_registry.entry
    ; target_actor : Operator_authorization.t
    ; intent : Intent.t
    }

  let create service ~controller ~controller_actor ~target ~target_actor ~intent =
    { service; controller; controller_actor; target; target_actor; intent }
  ;;

  let check_entries t =
    let open Result.Let_syntax in
    let controller =
      Intent.occurrence t.intent
      |> A.Recurrence_identity.Occurrence.definition
      |> A.Recurrence_identity.Definition.controller
    in
    let%bind () =
      Factory.check_registered_entry t.service.factory t.controller ~session:controller
    in
    Factory.check_registered_entry
      t.service.factory
      t.target
      ~session:(Intent.target t.intent)
  ;;

  let authorize t actor state =
    let open Result.Let_syntax in
    let%bind () = check_entries t in
    let%bind () = t.service.authorize actor state in
    check_entries t
  ;;

  let target_policy t = authorize t t.target_actor
  let controller_policy t state _definition = authorize t t.controller_actor state
  let self t = phys_equal t.controller.actor t.target.actor

  let observe t attempt =
    let open Result.Let_syntax in
    let%bind () = check_entries t in
    Actor.observe_recurrence_run
      t.target.actor
      ~actor:t.target_actor
      ~authorize:(target_policy t)
      ~intent:t.intent
      ~attempt
  ;;

  let observe_admission t attempt =
    let open Result.Let_syntax in
    let%bind () = check_entries t in
    let authorize state =
      let%bind () =
        if
          P.Id.Principal.equal
            (Operator_authorization.principal t.target_actor).id
            (Execution_attempt.principal_id attempt)
        then Ok ()
        else
          Error
            (P.Error.create
               Permission_denied
               ~message:"activation requires the original attempt principal"
               ~retryable:false
               ())
      in
      A.Run_command_authorization.check t.target_actor ~authorize:(target_policy t) state
    in
    Actor.observe_recurrence_run
      t.target.actor
      ~actor:t.target_actor
      ~authorize
      ~intent:t.intent
      ~attempt
  ;;

  let import t ?held observation =
    let open Result.Let_syntax in
    let%bind () = check_entries t in
    let authorize = controller_policy t in
    match held with
    | Some held when self t ->
      Actor.import_held_recurrence_execution
        t.controller.actor
        ~held
        ~actor:t.controller_actor
        ~authorize
        ~automatic_policy:t.service.automatic_policy
        ~observation
    | Some _ | None ->
      Actor.import_recurrence_execution
        t.controller.actor
        ~actor:t.controller_actor
        ~authorize
        ~automatic_policy:t.service.automatic_policy
        ~observation
  ;;

  let reconcile_known t attempt admission =
    let open Result.Let_syntax in
    let%bind observation = observe t attempt in
    match observation with
    | None -> conflict "known recurrence Admission has no retained target origin"
    | Some observation ->
      let actual =
        Actor.Recurrence_run_observation.origin observation
        |> A.Recurrence_run_origin.admission
      in
      if P.Run_receipt.equal actual admission
      then import t observation
      else conflict "retained recurrence Admission differs from the original receipt"
  ;;
end

(* Permit acquisition cannot abandon an installed cell on pending cancellation.
   Release is joined before returning, with the existing primary-error law. *)
let with_permit context observation f =
  let open Result.Let_syntax in
  let%bind permit =
    Eio.Cancel.protect (fun () ->
      Actor.acquire_recurrence_activation
        context.Context.controller.actor
        ~actor:context.controller_actor
        ~authorize:(Context.controller_policy context)
        ~observation)
  in
  let capture f =
    try Ok (f ()) with
    | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  let primary =
    capture (fun () ->
      Eio.Fiber.check ();
      f permit)
  in
  let cleanup =
    capture (fun () ->
      Eio.Cancel.protect (fun () ->
        Actor.release_recurrence_activation context.controller.actor permit))
  in
  match primary, cleanup with
  | Error (exn, backtrace), _ -> Exn.raise_with_original_backtrace exn backtrace
  | Ok (Error error), _ -> Error error
  | Ok (Ok value), Ok (Ok ()) -> Ok value
  | Ok (Ok _), Ok (Error error) -> Error error
  | Ok (Ok _), Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let activate_held context prepared held attempt =
  let open Result.Let_syntax in
  let observe () =
    let%bind observation = Context.observe context attempt in
    Result.of_option
      observation
      ~error:
        (P.Error.create
           Conflict
           ~message:"held recurrence origin is unavailable"
           ~retryable:false
           ())
  in
  let%bind observation = observe () in
  let%bind _imported = Context.import context ~held observation in
  let%bind observation = observe () in
  with_permit context observation (fun permit ->
    let%bind observation = observe () in
    let%bind checked =
      Actor.check_recurrence_activation context.controller.actor permit ~observation
    in
    let%bind () = Context.check_entries context in
    let checked = Actor.Recurrence_activation.Checked.admitted checked in
    let%map admission =
      match Intent.input context.intent with
      | Authored_start ->
        Runtime_owner.start_held_authored_runtime
          context.target.runtime
          ~prepared
          ~held
          ~checked
      | User_submission _ ->
        Runtime_owner.with_held_input_runtime
          context.target.runtime
          ~prepared
          ~held
          ~checked
          (fun runtime ->
             Runtime_owner.Held_input_runtime.submit
               runtime
               ~prepare:(fun ~runtime ~scope ->
                 Factory.prepare_held_input
                   context.service.factory
                   ~entry:context.target
                   ~runtime
                   ~scope))
    in
    Callback_success.Activated admission)
;;

let admit_selected context fact prepared attempt =
  let open Result.Let_syntax in
  let%bind () = Context.check_entries context in
  let%bind admission =
    Actor.admit_held_recurrence_run
      context.Context.target.actor
      ~binding:(Runtime_owner.Prepared.binding prepared)
      ~intent:context.intent
      ~attempt
      ~session:(Intent.target context.intent)
  in
  match admission with
  | Rejected error -> Error error
  | Uncertain error ->
    fact := Admission_fact.Uncertain;
    Error error
  | Retained admission ->
    fact := Admission_fact.Retained admission;
    let%map reconciliation = Context.reconcile_known context attempt admission in
    Callback_success.Retained { admission; reconciliation }
  | Held held ->
    fact := Admission_fact.Admitted (Actor.Held_run.admission held);
    activate_held context prepared held attempt
;;

let record_initial context prepared attempt =
  let open Result.Let_syntax in
  let%bind () = Context.check_entries context in
  let authorize = Context.controller_policy context in
  if Context.self context
  then
    Actor.record_prepared_recurrence_execution
      context.controller.actor
      ~binding:(Runtime_owner.Prepared.binding prepared)
      ~actor:context.controller_actor
      ~authorize
      ~intent:context.intent
      ~attempt
  else
    Actor.record_recurrence_execution
      context.controller.actor
      ~actor:context.controller_actor
      ~authorize
      ~intent:context.intent
      ~attempt
;;

let retained context attempt admission =
  let reconciliation = Context.reconcile_known context attempt admission in
  { Retained.admission; reconciliation }
;;

let activate service ~controller ~controller_actor ~target ~target_actor ~intent ~attempt =
  let open Result.Let_syntax in
  let context =
    Context.create service ~controller ~controller_actor ~target ~target_actor ~intent
  in
  let%bind () = Context.check_entries context in
  let%bind request = Execution_attempt.request attempt ~intent in
  let%bind observation = Context.observe_admission context attempt in
  match observation with
  | Some observation ->
    let admission =
      Actor.Recurrence_run_observation.origin observation
      |> A.Recurrence_run_origin.admission
    in
    let reconciliation = Context.import context observation in
    Ok (Outcome.Retained { admission; reconciliation })
  | None ->
    let fact = ref Admission_fact.Before_admission in
    let%map outcome =
      Factory.with_effect_deferred_run_source
        service.factory
        target
        ~actor:target_actor
        ~authorize:(Context.target_policy context)
        ~request
        (fun _deferred prepared ->
           let%bind _execution = record_initial context prepared attempt in
           admit_selected context fact prepared attempt)
    in
    (match outcome with
     | Retained admission -> Outcome.Retained (retained context attempt admission)
     | Attempted actual -> Outcome.Attempted { Attempt.actual; fact = !fact })
;;

let retry_selected context fact retry prepared =
  let open Result.Let_syntax in
  let%bind () = Context.check_entries context in
  let%bind _execution =
    Actor.rotate_prepared_recurrence_execution
      context.Context.controller.actor
      ~retry
      ~next:(Runtime_owner.Prepared.binding prepared)
      ~actor:context.controller_actor
      ~authorize:(Context.controller_policy context)
  in
  admit_selected context fact prepared (Actor.Recurrence_retry.next_attempt retry)
;;

let retry
      service
      ~controller
      ~controller_actor
      ~target
      ~target_actor
      ~observation
      ~intent
      ~previous
      ~request
  =
  let open Result.Let_syntax in
  let context =
    Context.create service ~controller ~controller_actor ~target ~target_actor ~intent
  in
  let%bind () = Context.check_entries context in
  let fact = ref Admission_fact.Before_admission in
  let%map actual =
    Factory.with_recurrence_retry_run_source
      service.factory
      target
      ~observation
      ~actor:target_actor
      ~authorize:(Context.target_policy context)
      ~intent
      ~previous
      ~request
      (retry_selected context fact)
  in
  { Attempt.actual; fact = !fact }
;;

let resume_retry service ~controller ~controller_actor ~target ~target_actor ~retry =
  let open Result.Let_syntax in
  let intent = Actor.Recurrence_retry.intent retry in
  let context =
    Context.create service ~controller ~controller_actor ~target ~target_actor ~intent
  in
  let%bind () = Context.check_entries context in
  let fact = ref Admission_fact.Before_admission in
  let%map actual =
    Factory.resume_recurrence_retry_run_source
      service.factory
      target
      ~retry
      ~actor:target_actor
      ~authorize:(Context.target_policy context)
      (retry_selected context fact)
  in
  { Attempt.actual; fact = !fact }
;;

let reconcile service ~controller ~controller_actor ~target ~target_actor ~intent ~attempt
  =
  let open Result.Let_syntax in
  let context =
    Context.create service ~controller ~controller_actor ~target ~target_actor ~intent
  in
  let%bind observation = Context.observe context attempt in
  match observation with
  | None -> Ok Reconciliation.Absent
  | Some observation ->
    let admission =
      Actor.Recurrence_run_observation.origin observation
      |> A.Recurrence_run_origin.admission
    in
    let reconciliation = Context.import context observation in
    Ok (Reconciliation.Retained { admission; reconciliation })
;;
