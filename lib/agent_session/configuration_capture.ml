open! Core
module C = Agent_protocol.Session_configuration
module Capacity = Session_context_capacity
module R = Inference_runtime
module Reminder = Recovery_reminder_preparation

module Capacity_basis = struct
  type t =
    { selection : Capacity.Selection.t
    ; generation : int
    ; spec : Session_state.Spec.Resource_identity.t
    ; run_source : Run_source_installation.t option
    ; authoring : Chat_response.Authoring_publication.context option
    ; instructions : int64 option
    ; window : Agent_protocol.Id.Context_window.t option
    }

  let capture (state : Session_state.t) ~selection =
    { selection
    ; generation = state.identity.generation
    ; spec = Session_state.Spec.Resource_identity.capture state.spec
    ; run_source = Option.map state.run_state ~f:Run_state.installation
    ; authoring = state.conversation.authoring_publication
    ; instructions =
        Option.map state.instruction_selection ~f:Instruction_selection.revision
    ; window =
        Option.map state.recovery_windows ~f:(fun ledger ->
          Recovery_window_ledger.Window.id (Recovery_window_ledger.active ledger))
    }
  ;;

  let automatic t =
    Capacity.Mode.equal (Capacity.availability t.selection).effective Automatic
  ;;

  let equal a b =
    Int.equal a.generation b.generation
    && Session_state.Spec.Resource_identity.equal a.spec b.spec
    && Option.equal Run_source_installation.equal a.run_source b.run_source
    && Option.equal
         Chat_response.Authoring_publication.equal_context
         a.authoring
         b.authoring
    && Option.equal Int64.equal a.instructions b.instructions
    && Option.equal Agent_protocol.Id.Context_window.equal a.window b.window
    && Capacity.Selection.equal a.selection b.selection
  ;;
end

module Capacity_observation = struct
  type t =
    { configuration : Inference.Observation.Configuration.t
    ; assessment : Capacity.Capacity.Assessment.t
    ; binding_is_current : unit -> bool
    }
end

module Cut_basis = struct
  type t =
    { history : Agent_protocol.History.entry list
    ; attempt_high_water : int64
    }

  let capture (state : Session_state.t) =
    { history = state.conversation.canonical_history
    ; attempt_high_water = Inference_ledger.last_attempt_ordinal state.inference_ledger
    }
  ;;
end

module Token = struct
  type recovery_binding =
    { configuration : Inference.Observation.Configuration.t
    ; source_revision : int64
    }

  type t =
    { identity : unit ref
    ; operation_id : Agent_protocol.Id.Operation.t
    ; revision : int64
    ; generation : int
    ; target : Inference.Request.Target.t
    ; policy : Configuration_policy.t
    ; capacity : Capacity_basis.t option
    ; cut : Cut_basis.t option
    ; recovery_scope : Agent_protocol.Recovery_window.Scope.t option
    ; reminder : Reminder.t option
    ; recovery_binding : recovery_binding option
    ; capture_recovery : bool
    ; assessment : Capacity_observation.t option
    ; blocked_configuration : Inference.Observation.Configuration.t option
    ; configuration : Inference.Observation.Configuration.t option
    }

  let operation_id t = t.operation_id
end

type t = { mutable current : (Token.t * C.phase) option }

let create () = { current = None }

let begin_capture
      ?capacity
      ?cut
      ?recovery_scope
      ~capture_recovery
      ~can_remind
      t
      ~operation_id
      ~generation
      ~revision
      ~target
      ~policy
  =
  let reminder =
    let open Option.Let_syntax in
    let%bind scope = recovery_scope in
    let%bind basis = capacity in
    let%bind policy = Reminder.of_selection basis.Capacity_basis.selection in
    Option.some_if (can_remind ~scope ~policy:(Reminder.revision policy)) policy
  in
  let token : Token.t =
    { identity = ref ()
    ; operation_id
    ; generation
    ; revision
    ; target
    ; policy
    ; capacity
    ; cut
    ; recovery_scope
    ; reminder
    ; recovery_binding = None
    ; capture_recovery
    ; assessment = None
    ; blocked_configuration = None
    ; configuration = None
    }
  in
  t.current <- Some (token, C.Preparing);
  token
;;

let owns t token =
  Option.exists t.current ~f:(fun (current, _) ->
    phys_equal current.Token.identity token.Token.identity)
;;

let check_recovery t token ~scope =
  match t.current with
  | Some (current, Preparing)
    when owns t token
         && current.capture_recovery
         && Option.exists
              current.recovery_scope
              ~f:(Agent_protocol.Recovery_window.Scope.equal scope) -> Ok ()
  | _ -> Error (Agent_protocol.Error.invalid_request "root preparation custody changed")
;;

let reminder_custody t token ~scope ~current =
  match t.current, current with
  | Some (captured, phase), Some current
    when owns t token
         && Option.exists captured.capacity ~f:(Capacity_basis.equal current)
         && Option.exists
              captured.recovery_scope
              ~f:(Agent_protocol.Recovery_window.Scope.equal scope) ->
    Option.map captured.reminder ~f:(fun policy -> captured, phase, policy)
  | _ -> None
;;

let claim_reminder t token ~scope ~current prepared =
  match reminder_custody t token ~scope ~current with
  | Some (_, Preparing, policy) when Reminder.fits policy prepared ->
    Recovery_reminder.Claim.create
      ~scope
      ~policy:(Reminder.revision policy)
      ~operation:token.operation_id
      ~preparation:(R.Prepared.preparation_id prepared)
    |> Result.map ~f:Option.some
  | _ -> Ok None
;;

let check_reminder_dispatch t token ~scope ~current claim prepared attempt =
  match reminder_custody t token ~scope ~current with
  | Some (captured, Effective, _)
    when Option.exists
           captured.configuration
           ~f:(phys_equal (R.Attempt.configuration attempt))
         && String.equal
              (Recovery_reminder.Claim.preparation claim)
              (R.Prepared.preparation_id prepared)
         && R.Prepared.binding_guard prepared ()
         && R.Prepared.owns_attempt prepared attempt
         &&
         match (R.Attempt.scope attempt).relation with
         | Root -> true
         | Nested _ -> false -> Ok ()
  | _ ->
    Error (Agent_protocol.Error.invalid_request "reminder preparation custody changed")
;;

let record_recovery_binding t token ~configuration ~source_revision =
  match t.current with
  | Some (current, Preparing) when owns t token && current.capture_recovery ->
    let recovery_binding = Some { Token.configuration; source_revision } in
    t.current <- Some ({ current with recovery_binding }, Preparing)
  | _ -> ()
;;

let recovery_source_revision t ~operation_id ~configuration =
  let open Option.Let_syntax in
  let%bind token, phase = t.current in
  let%bind binding = token.recovery_binding in
  match phase with
  | Preparing
    when Agent_protocol.Id.Operation.equal token.operation_id operation_id
         && phys_equal binding.configuration configuration -> Some binding.source_revision
  | _ -> None
;;

let mark t token configuration =
  match t.current with
  | Some (current, _) when owns t token ->
    let current =
      match current.assessment with
      | Some observation
        when not
               (Inference.Observation.Configuration.equal
                  observation.configuration
                  configuration) -> { current with capacity = None; assessment = None }
      | None | Some _ -> current
    in
    t.current
    <- Some
         ( { current with configuration = Some configuration; recovery_binding = None }
         , C.Effective );
    Ok ()
  | _ ->
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"root capture no longer owns preparation"
         ~retryable:false
         ())
;;

let hard_limit () =
  Error
    (Agent_protocol.Error.create
       Resource_limit
       ~message:"prepared request exceeds the current context input limit"
       ~retryable:false
       ())
;;

let mark_capacity t token ~current observation =
  match t.current, current, token.Token.capacity with
  | Some (captured, Preparing), Some current, Some selected
    when owns t token
         && Option.is_none captured.assessment
         && Capacity_basis.equal current selected
         && observation.Capacity_observation.binding_is_current () ->
    let blocked_configuration =
      match Capacity.Capacity.Assessment.view observation.assessment with
      | Assessed { pressure = Hard_limit_exceeded; _ } -> Some observation.configuration
      | Unavailable _ | Assessed _ -> None
    in
    t.current
    <- Some
         ( { captured with assessment = Some observation; blocked_configuration }
         , Preparing );
    if Option.is_some blocked_configuration && Option.is_none captured.cut
    then hard_limit ()
    else Ok ()
  | _ ->
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"context capacity basis changed during request preparation"
         ~retryable:false
         ())
;;

let check_allocation t ~operation_id configuration =
  match t.current with
  | Some (token, _)
    when Agent_protocol.Id.Operation.equal token.operation_id operation_id
         && Option.exists token.blocked_configuration ~f:(phys_equal configuration) ->
    hard_limit ()
  | _ -> Ok ()
;;

let capacity_status t ~operation_id ~current =
  match current with
  | None -> Capacity.Disabled
  | Some current ->
    let availability = Capacity.availability current.Capacity_basis.selection in
    (match t.current with
     | Some (token, phase)
       when Agent_protocol.Id.Operation.equal token.operation_id operation_id ->
       (match token.capacity, token.assessment with
        | Some basis, Some observation
          when Capacity_basis.equal basis current && observation.binding_is_current () ->
          Capacity.Current
            { availability
            ; phase
            ; configuration = observation.configuration
            ; assessment = observation.assessment
            }
        | Some basis, None when Capacity_basis.equal basis current ->
          Capacity.Awaiting_preparation availability
        | _ ->
          t.current <- Some ({ token with capacity = None; assessment = None }, phase);
          Capacity.Stale_preparation availability)
     | _ -> Capacity.Awaiting_preparation availability)
;;

let capacity_rollover t ~operation_id ~current ~(state : Session_state.t) stage =
  let module R = Chat_response.Capacity_rollover in
  let module Prepared = Inference_runtime.Prepared in
  let module Attempt = Inference_runtime.Attempt in
  let module Assessment = Capacity.Capacity.Assessment in
  let conflict () =
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"automatic rollover preparation is no longer current"
         ~retryable:true
         ())
  in
  let matching token selection =
    match token.Token.cut, token.capacity, token.assessment with
    | Some cut, Some captured, Some assessment
      when Agent_protocol.Id.Operation.equal token.operation_id operation_id
           && Capacity_basis.equal captured selection
           && assessment.binding_is_current ()
           && phys_equal cut.history state.conversation.canonical_history ->
      Some (cut, assessment)
    | _ -> None
  in
  let last_ordinal = Inference_ledger.last_attempt_ordinal state.inference_ledger in
  match t.current, current with
  | Some (token, _), (None | Some _)
    when Agent_protocol.Id.Operation.equal token.operation_id operation_id
         && Option.is_some token.cut
         && not (Option.exists current ~f:Capacity_basis.automatic) -> conflict ()
  | _, None -> Ok R.No_transition
  | _, Some basis when not (Capacity_basis.automatic basis) -> Ok R.No_transition
  | Some (token, phase), Some basis ->
    (match matching token basis with
     | None -> conflict ()
     | Some (cut, observation) ->
       (match stage, phase with
        | R.Unsent prepared, Preparing
          when Int64.equal cut.attempt_high_water last_ordinal
               && phys_equal observation.configuration (Prepared.configuration prepared)
               && Inference.Request.Target.equal token.target (Prepared.target prepared)
               && Prepared.capacity_assessment_is_current
                    ?policy:(Capacity.Selection.policy basis.selection)
                    prepared
                    observation.assessment ->
          (match Assessment.view observation.assessment with
           | Assessed { pressure = Hard_limit_exceeded; _ } -> Ok R.Hard_limit
           | Assessed { pressure = Checkpoint_due; _ } -> Ok R.Checkpoint_opportunity
           | Assessed _ | Unavailable _ -> Ok R.No_transition)
        | R.Rejected rejected, Effective
          when Int64.(cut.attempt_high_water < last_ordinal)
               && phys_equal
                    observation.configuration
                    (Attempt.configuration (R.attempt rejected))
               && Option.exists
                    token.configuration
                    ~f:(phys_equal observation.configuration) -> Ok R.Qualified_rejection
        | _ -> conflict ()))
  | _ -> conflict ()
;;

module Native_window = struct
  type t =
    { identity : unit ref
    ; scope : Agent_protocol.Recovery_window.Scope.t
    }

  let scope t = t.scope
  let same_preparation a b = phys_equal a.identity b.identity
end

let native_window t ~operation_id ~configuration_revision ~target =
  match t.current with
  | Some (token, (C.Effective | Retained))
    when Agent_protocol.Id.Operation.equal token.operation_id operation_id
         && Int64.equal token.revision configuration_revision
         && Inference.Request.Target.equal token.target target
         && Option.is_some token.configuration
         && Option.exists token.assessment ~f:(fun observation ->
           observation.binding_is_current ()) ->
    Option.map token.recovery_scope ~f:(fun selected_scope ->
      Native_window.{ identity = token.identity; scope = selected_scope })
  | _ -> None
;;

let finish t token ~success =
  if owns t token
  then
    if success
    then
      t.current
      <- Option.map t.current ~f:(fun (current, _) ->
           { current with recovery_binding = None }, C.Retained)
    else t.current <- None
;;

let view t ~generation ~revision ~selected =
  let open Result.Let_syntax in
  let selection = selected in
  let%bind selected =
    match Inference.Selection.view selection with
    | Unresolved -> Ok None
    | Captured target ->
      Result.map (Configuration_transition.safe_view target) ~f:Option.some
  in
  let retained =
    Option.filter t.current ~f:(fun (token, _) ->
      Int.equal token.Token.generation generation)
  in
  let%map capture =
    match retained with
    | None -> Ok None
    | Some (token, phase) ->
      let%map configuration =
        match token.configuration with
        | Some configuration -> Ok configuration
        | None -> Configuration_transition.safe_view token.target
      in
      Some
        C.
          { operation_id = token.operation_id
          ; revision = token.revision
          ; phase
          ; configuration
          }
  in
  C.
    { revision
    ; selected
    ; capture
    ; pending =
        Option.exists retained ~f:(fun (token, _) ->
          match Inference.Selection.view selection with
          | Unresolved -> true
          | Captured selected ->
            not (Inference.Request.Target.equal token.target selected))
    }
;;

let require = function
  | Ok value -> value
  | Error error -> raise (Native_tool_dispatch.Dispatch_error error)
;;

let root_port
      ~begin_capture
      ~publish_capacity
      ~publish_recovery
      ~claim_reminder
      ~accept_reminder
      ~mark
      ~finish
  =
  Chat_response.Root_context.
    { with_context =
        (fun ~previous ~history f ->
          let token : Token.t = begin_capture () |> require in
          let complete success =
            Eio.Cancel.protect (fun () -> finish token ~success |> require)
          in
          match
            let context = token.policy.resolve token.target |> require in
            let context = Inference_runtime.Context.reuse_unchanged context ~previous in
            Inference_runtime.Context.preflight_history context history
            |> Result.map_error ~f:(fun _ ->
              Agent_protocol.Error.invalid_request
                "captured target cannot retain current history")
            |> require;
            let on_prepared prepared =
              Option.iter token.capacity ~f:(fun capacity ->
                let module Prepared = Inference_runtime.Prepared in
                let policy = Capacity.Selection.policy capacity.selection in
                let assessment = Prepared.assess_capacity ?policy prepared in
                if
                  not
                    (Prepared.capacity_assessment_is_current ?policy prepared assessment
                     && Inference.Request.Target.equal
                          token.target
                          (Prepared.target prepared))
                then
                  Error
                    (Agent_protocol.Error.create
                       Conflict
                       ~message:"capacity assessment does not match the prepared target"
                       ~retryable:false
                       ())
                  |> require
                else
                  publish_capacity
                    token
                    Capacity_observation.
                      { configuration = Prepared.configuration prepared
                      ; assessment
                      ; binding_is_current = Prepared.binding_guard prepared
                      }
                  |> require);
              if token.capture_recovery && Option.is_some token.recovery_scope
              then
                let module Prepared = Inference_runtime.Prepared in
                let binding_is_current = Prepared.binding_guard prepared in
                if
                  Inference.Request.Target.equal token.target (Prepared.target prepared)
                  && binding_is_current ()
                then
                  Option.iter (Prepared.recovery_input prepared) ~f:(fun input ->
                    publish_recovery
                      token
                      input
                      ~configuration:(Prepared.configuration prepared)
                      ~binding_is_current
                    |> require)
            in
            let select_prepared =
              let open Option.Let_syntax in
              let%map policy = token.reminder in
              fun ~request ~prepare_isolated baseline ->
                let%bind () = Option.some_if (Reminder.warns policy baseline) () in
                let%bind request = Reminder.with_reminder request in
                let%bind candidate = prepare_isolated request |> Result.ok in
                let prepared = Inference_client.Prepared_selection.prepared candidate in
                let%bind () =
                  Option.some_if
                    (Reminder.fits policy prepared
                     && R.Prepared.includes_history
                          prepared
                          (Inference.Request.history request))
                    ()
                in
                let%map claim = claim_reminder token prepared |> require in
                let for_attempt attempt =
                  R.Submission_guard.create_write_admission ~refresh:(fun () ->
                    accept_reminder token claim prepared attempt |> require)
                in
                Inference_client.Prepared_selection.choose candidate ~for_attempt
            in
            f context ~select_prepared ~on_prepared ~on_dispatch:(fun configuration ->
              mark token configuration |> require)
          with
          | result ->
            complete true;
            result
          | exception exn ->
            let backtrace = Stdlib.Printexc.get_raw_backtrace () in
            (* Cleanup cannot replace the request's original failure/cancellation. *)
            (try complete false with
             | _ -> ());
            Stdlib.Printexc.raise_with_backtrace exn backtrace)
    }
;;
