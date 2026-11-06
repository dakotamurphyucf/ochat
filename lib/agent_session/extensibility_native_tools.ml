open Core
module P = Agent_protocol
module Moderation = Chat_response.Moderation

type t =
  | Run_chatml
  | Agent_create
  | Agent_status
  | Agent_send
  | Agent_read
  | Agent_wait
  | Agent_stop
  | Authoring_validation
  | Authoring_context
  | Instructions_refresh
  | Instructions_read
  | Instructions_activate
  | Instructions_deactivate
  | Instructions_status
  | Goal_get
  | Goal_create
  | Goal_hold
  | Goal_abandon
  | Goal_objective
  | Goal_progress
  | Goal_start
  | Recurrence_create
  | Recurrence_list
  | Recurrence_get
  | Recurrence_edit
  | Recurrence_pause
  | Recurrence_resume
  | Recurrence_cancel
  | Recurrence_status
  | Recurrence_history
[@@deriving enumerate]

let name = function
  | Run_chatml -> Run_chatml_tool.name
  | Agent_create -> Generated_session_tool.name
  | Agent_status -> Managed_session_tool.status_name
  | Agent_send -> Managed_send_tool.name
  | Agent_read -> Managed_read_tool.name
  | Agent_wait -> Managed_wait_tool.name
  | Agent_stop -> Managed_stop_tool.name
  | Authoring_validation -> Authoring_validation_tool.name
  | Authoring_context -> Authoring_context_tool.name
  | Instructions_refresh -> Instruction_native_admission.Effect.name Refresh
  | Instructions_read -> Instruction_native_admission.Effect.name Read
  | Instructions_activate -> Instruction_native_admission.Effect.name Activate
  | Instructions_deactivate -> Instruction_native_admission.Effect.name Deactivate
  | Instructions_status -> Instruction_native_admission.Effect.name Status
  | Goal_get -> Goal_native_admission.Effect.name Get
  | Goal_create -> Goal_native_admission.Effect.name Create
  | Goal_hold -> Goal_native_admission.Effect.name Hold
  | Goal_abandon -> Goal_native_admission.Effect.name Abandon
  | Goal_objective -> Goal_native_admission.Effect.name Objective
  | Goal_progress -> Goal_native_admission.Effect.name Progress
  | Goal_start -> Goal_native_admission.Effect.name Start
  | Recurrence_create -> Recurrence_native_admission.Effect.name Create
  | Recurrence_list -> Recurrence_native_admission.Effect.name List
  | Recurrence_get -> Recurrence_native_admission.Effect.name Get
  | Recurrence_edit -> Recurrence_native_admission.Effect.name Edit
  | Recurrence_pause -> Recurrence_native_admission.Effect.name Pause
  | Recurrence_resume -> Recurrence_native_admission.Effect.name Resume
  | Recurrence_cancel -> Recurrence_native_admission.Effect.name Cancel
  | Recurrence_status -> Recurrence_native_admission.Effect.name Status
  | Recurrence_history -> Recurrence_native_admission.Effect.name History
;;

let names = List.map all ~f:name

let run_services env () =
  let open Result.Let_syntax in
  let%bind script_tools = Script_tool_calls.current_native_services () in
  let%map moderation = Native_tool_moderation.current () in
  Run_chatml_tool.
    { script_tools
    ; observer = Native_tool_moderation.observer moderation
    ; now =
        (fun () ->
          Eio.Time.now (Eio.Stdenv.clock env)
          |> Time_ns.Span.of_sec
          |> Time_ns.of_span_since_epoch
          |> P.Timestamp.of_time_ns)
    ; moderate_tool =
        (fun _ call ->
          Native_tool_moderation.prepare moderation call
          |> Result.map ~f:(fun tool_moderation ->
            Some { Moderation.Outcome.empty with tool_moderation }))
    ; prepare_outcome =
        (fun outcome ->
          P.Invocation.validate_outcome outcome
          |> Result.map_error ~f:(fun error -> error.P.Error.message))
    }
;;

let registration ~env ~one_off_policy ~authoring_validation_host kind =
  let present value = Ok (Some value) in
  let unavailable message = P.Error.create Invalid_state ~message ~retryable:false () in
  match kind, one_off_policy with
  | Goal_get, _ -> Goal_native_metadata_tool.get |> present
  | Goal_create, _ -> Goal_native_metadata_tool.create |> present
  | Goal_hold, _ -> Goal_native_metadata_tool.hold |> present
  | Goal_abandon, _ -> Goal_native_metadata_tool.abandon |> present
  | Goal_objective, _ -> Goal_native_metadata_tool.objective |> present
  | Goal_progress, _ -> Goal_native_metadata_tool.progress |> present
  | Goal_start, _ -> Goal_native_start_tool.registration |> present
  | Recurrence_create, _ -> Recurrence_native_tool.registration Create |> present
  | Recurrence_list, _ -> Recurrence_native_tool.registration List |> present
  | Recurrence_get, _ -> Recurrence_native_tool.registration Get |> present
  | Recurrence_edit, _ -> Recurrence_native_tool.registration Edit |> present
  | Recurrence_pause, _ -> Recurrence_native_tool.registration Pause |> present
  | Recurrence_resume, _ -> Recurrence_native_tool.registration Resume |> present
  | Recurrence_cancel, _ -> Recurrence_native_tool.registration Cancel |> present
  | Recurrence_status, _ -> Recurrence_native_tool.registration Status |> present
  | Recurrence_history, _ -> Recurrence_native_tool.registration History |> present
  | Instructions_refresh, _ -> Instruction_native_tool.registration Refresh |> present
  | Instructions_read, _ -> Instruction_native_tool.registration Read |> present
  | Instructions_activate, _ -> Instruction_native_tool.registration Activate |> present
  | Instructions_deactivate, _ ->
    Instruction_native_tool.registration Deactivate |> present
  | Instructions_status, _ -> Instruction_native_tool.registration Status |> present
  | ( ( Run_chatml
      | Agent_create
      | Agent_status
      | Agent_send
      | Agent_read
      | Agent_wait
      | Agent_stop )
    , None ) -> Ok None
  | Run_chatml, Some policy ->
    Run_chatml_tool.registration ~env ~policy ~services:(run_services env) |> present
  | Agent_create, Some _ -> Generated_session_tool.registration () |> present
  | Agent_status, Some _ -> Managed_session_tool.status_registration () |> present
  | Agent_send, Some _ -> Managed_send_tool.registration () |> present
  | Agent_read, Some _ -> Managed_read_tool.registration () |> present
  | Agent_wait, Some _ -> Managed_wait_tool.registration () |> present
  | Agent_stop, Some _ -> Managed_stop_tool.registration () |> present
  | Authoring_validation, _ ->
    (match authoring_validation_host with
     | Some host -> Authoring_validation_tool.registration ~env ~host |> present
     | None ->
       Error
         (unavailable
            "authoring.unavailable: readonly validation needs an explicit host target"))
  | Authoring_context, _ ->
    (match authoring_validation_host with
     | Some host ->
       Authoring_context_tool.registration ~host
       |> Result.map ~f:Option.some
       |> Result.map_error ~f:unavailable
     | None ->
       Error
         (unavailable
            "authoring.unavailable: documentation queries need an explicit host target"))
;;

let declares elements name =
  List.exists elements ~f:(function
    | Prompt.Chat_markdown.Tool (Builtin declared) -> String.equal declared name
    | _ -> false)
;;

let declares_any elements = List.exists names ~f:(declares elements)

let registrations ~env ~elements ~one_off_policy ~authoring_validation_host =
  List.filter all ~f:(fun kind -> declares elements (name kind))
  |> List.fold_result ~init:[] ~f:(fun reversed kind ->
    let open Result.Let_syntax in
    let%map registration =
      registration ~env ~one_off_policy ~authoring_validation_host kind
    in
    match registration with
    | None -> reversed
    | Some registration -> registration :: reversed)
  |> Result.map ~f:List.rev
;;
