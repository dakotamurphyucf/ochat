open! Core
module P = Agent_protocol
module R = Chatml_host_runtime
module L = Chatml.Chatml_lang
module V = Chatml.Chatml_value_codec
module X = Chatml_execution
module Jobs = Developer_script_job_service

type t =
  { completion : P.Completion.t
  ; local_effects : L.eff list
  }

exception Fatal_effect of P.Error.t

let completion t = t.completion
let local_effects t = t.local_effects
let ( let* ) result f = Result.bind result ~f

let utf8_prefix text max_bytes ~fallback =
  let prefix = String.prefix text (Int.min (String.length text) max_bytes) in
  let rec shorten length =
    if length <= 0
    then fallback
    else (
      let candidate = String.prefix prefix length in
      if Stdlib.String.is_valid_utf_8 candidate then candidate else shorten (length - 1))
  in
  shorten (String.length prefix)
;;

let failure ~code ~message =
  P.Completion.Failed
    { code = utf8_prefix code 128 ~fallback:"chatml.execution_failed"
    ; message = utf8_prefix message 1024 ~fallback:"Script execution failed."
    ; retryable = false
    ; details = `Null
    }
;;

let failed_error (error : X.error) = failure ~code:error.code ~message:error.message

let result ~(policy : Chat_response.One_off_request.policy) completion local_effects =
  let* () = P.Completion.validate completion in
  let* () =
    P.Json_codec.validate_limits
      ~max_depth:policy.Chat_response.One_off_request.execution.max_depth
      ~max_bytes:policy.max_output_bytes
      (P.Completion.to_json completion)
    |> Result.map_error ~f:(fun _ ->
      P.Error.create
        Resource_limit
        ~message:"Script completion exceeds its saved output limit"
        ~retryable:false
        ())
  in
  Ok { completion; local_effects }
;;

let expired ~policy = result ~policy P.Completion.Expired []

let outcome_value ?control ~policy outcome =
  let open Result.Let_syntax in
  let%bind () = P.Invocation.validate_outcome outcome in
  let%bind () =
    P.Json_codec.validate_limits
      ~max_depth:policy.Chat_response.One_off_request.execution.max_depth
      ~max_bytes:policy.execution.max_value_bytes
      (P.Invocation.outcome_to_json outcome)
  in
  match outcome with
  | P.Invocation.Complete value ->
    Ok (L.VVariant ("Ok", [ V.import_json ?control value ]))
  | Pending (Job _, acknowledgement) ->
    Ok (L.VVariant ("Ok", [ V.import_json ?control acknowledgement ]))
  | Fail error -> Ok (L.VVariant ("Error", [ L.VString error.message ]))
  | Cancelled reason -> Ok (L.VVariant ("Error", [ L.VString reason ]))
  | Pending (Subscription _, _) ->
    Error (P.Error.invalid_request "Script Tool.call returned unsupported work")
;;

let execute ~scope ~prepared ~input ~policy ~call_tool control =
  let local_effects = ref [] in
  let handlers =
    { R.default_handlers with
      on_tool_call =
        (fun _ ~name ~args ->
          let* args = V.export_json ?control args in
          let outcome =
            match call_tool ~name ~args with
            | Ok outcome -> outcome
            | Error error -> raise (Fatal_effect error)
          in
          match outcome_value ?control ~policy outcome with
          | Ok value -> Ok value
          | Error error -> raise (Fatal_effect error))
    }
  in
  let config : R.runtime_config =
    { surface = Chatml.Chatml_extension_surface.one_off_v1
    ; operations = R.default_operations ~handlers ()
    }
  in
  let config = Jobs.Scope.install ?control scope config in
  let prepare_result ~value:_ ~local_effects:effects =
    local_effects := effects;
    Ok Fn.id
  in
  let argument = V.import_json ?control input in
  match
    X.run_in_scope
      ~prepare_result
      ~control
      ~config
      ~program:(Chat_response.One_off_script.program prepared)
      ~entrypoint:Chat_response.One_off_script.entrypoint
      ~arguments:[ argument ]
      ()
  with
  | Error error -> result ~policy (failed_error error) []
  | Ok value ->
    (match V.export_json ?control value with
     | Error message -> result ~policy (failure ~code:"chatml.invalid_output" ~message) []
     | Ok value ->
       let completion = P.Completion.Succeeded value in
       result ~policy completion !local_effects)
;;

let run
      ~env
      ~scope
      ~prepared
      ~input
      ~(policy : Chat_response.One_off_request.policy)
      ~deadline
      ~now
      ~call_tool
  =
  let remaining =
    Time_ns.diff (P.Timestamp.to_time_ns deadline) (P.Timestamp.to_time_ns (now ()))
    |> Time_ns.Span.to_sec
    |> Float.min policy.execution.wall_seconds
  in
  if Float.(remaining <= 0.)
  then expired ~policy
  else (
    let limits = { policy.execution with wall_seconds = remaining } in
    match
      try
        X.with_control
          ~env
          ~policy:(Bounded limits)
          (execute ~scope ~prepared ~input ~policy ~call_tool)
      with
      | Fatal_effect error -> Ok (Error error)
    with
    | Error error when String.equal error.code "chatml.execution_timeout" ->
      expired ~policy
    | Error error -> result ~policy (failed_error error) []
    | Ok outcome -> outcome)
;;
