open! Core
module P = Agent_protocol
module C = Recovery_rollover_capacity
module L = Inference_ledger
module Config = Inference.Observation.Configuration
module Actual = Inference_runtime.Attempt
module R = Inference.Observation.Attempt_record
module Terminal = Inference.Event.Terminal

let conflict message = Error (P.Error.create Conflict ~message ~retryable:true ())

let history_digest (state : Session_state.t) =
  let history = state.conversation.canonical_history in
  if Stdlib.List.compare_length_with history Recovery_window_record.max_members > 0
  then conflict "automatic rollover history exceeds the recovery window bound"
  else (
    let identities =
      List.map history ~f:(fun (entry : P.History.entry) ->
        `Array
          [ P.History.Id.to_json entry.id
          ; P.History.Content_revision.to_json entry.content_revision
          ])
    in
    Ok
      (Chatml_source.Digest.of_contents
         (Jsonaf.to_string (`Array identities))
         ~checkpoint:ignore))
;;

let root state ~(operation : P.Id.Operation.t) handle =
  P.Id.Session.equal (L.Handle.session_id handle) state.Session_state.identity.session_id
  && Int.equal (L.Handle.generation handle) state.identity.generation
  && Option.equal P.Id.Operation.equal (L.Handle.operation_id handle) (Some operation)
  && Option.is_none (L.Handle.invocation_id handle)
  &&
  match (L.Handle.scope handle).relation with
  | Root -> true
  | Nested _ -> false
;;

let pending_root state ~operation =
  List.exists (L.rows state.Session_state.inference_ledger) ~f:(fun row ->
    root state ~operation (L.Row.handle row)
    &&
    match R.state (L.Row.record row) with
    | Prepared | Running -> true
    | Terminal _ | Interrupted _ -> false)
;;

let rejected_row state ~operation ~ordinal configuration =
  match L.find state.Session_state.inference_ledger ~ordinal with
  | Some row when root state ~operation (L.Row.handle row) ->
    let record = L.Row.record row in
    (match R.state record with
     | Terminal terminal
       when Terminal.is_context_window_rejection terminal
            && Config.equal (R.configuration record) configuration -> Ok (row, terminal)
     | _ -> conflict "capacity rejection has no matching settled accounting row")
  | _ -> conflict "capacity rejection has no retained foreground accounting"
;;

let active (state : Session_state.t) operation =
  match
    state.active_operation, state.lifecycle.observed, state.runtime_initialization
  with
  | Some selected, Running_turn running, Ready
    when P.Session.equal_desired_state state.lifecycle.desired Running
         && (not state.halted)
         && P.Id.Operation.equal selected.id operation
         && P.Id.Operation.equal running operation
         && Int.equal state.identity.generation selected.generation
         &&
         match selected.kind, selected.state with
         | Turn _, Running -> true
         | _ -> false -> Ok ()
  | _ -> conflict "automatic rollover no longer owns its original foreground"
;;

let recursive_successor (state : Session_state.t) operation =
  let open Option.Let_syntax in
  Option.value
    ~default:false
    (let%bind windows = state.recovery_windows in
     let%bind rollovers = state.recovery_rollover in
     let scope =
       Recovery_window_ledger.Window.scope (Recovery_window_ledger.active windows)
     in
     let%map selected = Recovery_rollover.foreground rollovers ~scope in
     match selected.phase with
     | Preparing id | Admitted { operation = id; _ } -> P.Id.Operation.equal id operation
     | Awaiting | Waiting | Held _ | Finished _ -> false)
;;

let check_target state configuration =
  let module T = Inference.Request.Target in
  let%bind.Result target = Configuration_transition.target state in
  if
    String.equal (T.adapter target) (Config.adapter configuration)
    && String.equal (T.profile target) (Config.profile configuration)
    && Option.equal
         String.equal
         (T.profile_revision target)
         (Config.profile_revision configuration)
    && Option.equal String.equal (T.account target) (Config.account configuration)
    && String.equal (T.model target) (Config.model configuration)
  then Ok ()
  else conflict "automatic rollover target changed"
;;

let validate state (cut : C.t) =
  let open Result.Let_syntax in
  let%bind () = active state cut.operation in
  let%bind history = history_digest state in
  let%bind () = check_target state cut.configuration in
  let%bind () =
    if
      Int64.equal state.spec.configuration_revision cut.configuration_revision
      && Int64.equal
           (L.last_attempt_ordinal state.inference_ledger)
           cut.attempt_high_water
      && Chatml_source.Digest.equal history cut.history
      && (not (pending_root state ~operation:cut.operation))
      && not (recursive_successor state cut.operation)
    then Ok ()
    else conflict "automatic rollover preparation or accounting cut changed"
  in
  match cut.boundary with
  | Unsent -> Ok ()
  | Rejected ->
    rejected_row
      state
      ~operation:cut.operation
      ~ordinal:cut.attempt_high_water
      cut.configuration
    |> Result.map ~f:ignore
;;

let capture state ~operation ~trigger stage =
  let module Live = Chat_response.Capacity_rollover in
  let open Result.Let_syntax in
  let attempt_high_water = L.last_attempt_ordinal state.Session_state.inference_ledger in
  let%bind boundary, configuration =
    match stage with
    | Live.Unsent prepared ->
      Ok (C.Unsent, Inference_runtime.Prepared.configuration prepared)
    | Rejected rejection ->
      let attempt = Live.attempt rejection in
      let configuration = Actual.configuration attempt in
      let%bind row, terminal =
        rejected_row state ~operation ~ordinal:attempt_high_water configuration
      in
      let handle = L.Row.handle row in
      if
        Transcript.Scope.equal (Actual.scope attempt) (L.Handle.scope handle)
        && Inference.Observation.Observation_id.equal
             (Actual.accounting_id attempt)
             (L.Handle.accounting_id handle)
        && Terminal.equal terminal (Live.terminal rejection)
      then Ok (C.Rejected, configuration)
      else conflict "capacity rejection does not match the acknowledged actual attempt"
  in
  let%bind history = history_digest state in
  C.create
    ~operation
    ~trigger
    ~boundary
    ~attempt_high_water
    ~configuration_revision:state.spec.configuration_revision
    ~configuration
    ~history
;;
