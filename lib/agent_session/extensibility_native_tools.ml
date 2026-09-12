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
