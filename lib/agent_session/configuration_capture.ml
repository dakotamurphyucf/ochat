open! Core
module C = Agent_protocol.Session_configuration
module Capacity = Session_context_capacity

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

module Token = struct
  type t =
    { identity : unit ref
    ; operation_id : Agent_protocol.Id.Operation.t
    ; revision : int64
    ; generation : int
    ; target : Inference.Request.Target.t
    ; policy : Configuration_policy.t
    ; capacity : Capacity_basis.t option
    ; assessment : Capacity_observation.t option
    ; configuration : Inference.Observation.Configuration.t option
    }

  let operation_id t = t.operation_id
end

type t = { mutable current : (Token.t * C.phase) option }

let create () = { current = None }

let begin_capture ?capacity t ~operation_id ~generation ~revision ~target ~policy =
  let token : Token.t =
    { identity = ref ()
    ; operation_id
    ; generation
    ; revision
    ; target
    ; policy
    ; capacity
    ; assessment = None
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
    t.current <- Some ({ current with configuration = Some configuration }, C.Effective);
    Ok ()
  | _ ->
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"root capture no longer owns preparation"
         ~retryable:false
         ())
;;

let mark_capacity t token ~current observation =
  match t.current with
  | Some (captured, Preparing) when owns t token && Option.is_none captured.assessment ->
    let captured =
      match current, captured.capacity with
      | Some current, Some selected
        when Capacity_basis.equal current selected
             && observation.Capacity_observation.binding_is_current () ->
        { captured with assessment = Some observation }
      | _ -> { captured with capacity = None }
    in
    t.current <- Some (captured, Preparing)
  | _ -> ()
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

let finish t token ~success =
  if owns t token
  then
    if success
    then t.current <- Option.map t.current ~f:(fun (current, _) -> current, C.Retained)
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

let root_port ~begin_capture ~publish_capacity ~mark ~finish =
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
                  Prepared.capacity_assessment_is_current ?policy prepared assessment
                  && Inference.Request.Target.equal
                       token.target
                       (Prepared.target prepared)
                then
                  publish_capacity
                    token
                    Capacity_observation.
                      { configuration = Prepared.configuration prepared
                      ; assessment
                      ; binding_is_current = Prepared.binding_guard prepared
                      }
                  |> require)
            in
            f context ~on_prepared ~on_dispatch:(fun configuration ->
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
