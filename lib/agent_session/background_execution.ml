open Core
module B = Chat_response.Background_request
module P = Chat_response.One_off_request
module C = Chat_response.Tool_capability
module I = Agent_protocol.Invocation
module N = Native_tool_invocation
module Requests = Chat_response.Runtime_request_scope
module M = Chat_response.Moderation

type result =
  { resolved : I.t
  ; runtime_requests : M.Runtime_request.t list
  }

let failure code message = I.Fail { code; message; retryable = false; details = `Null }

let run
      ?observer
      ~env
      ~(job : Agent_protocol.Job.t)
      ~deadline
      ~execute
      ~request
      ~policy
      ~script_tools
      ~now
      ~moderate_tool
      ~prepare_outcome
      ()
  =
  let open Result.Let_syntax in
  let%bind saved = Agent_protocol.Json_codec.canonical job.payload in
  let%bind supplied = Agent_protocol.Json_codec.canonical (B.to_json request) in
  let%bind () =
    match Jsonaf.exactly_equal saved supplied with
    | true -> Ok ()
    | false ->
      Error
        (Agent_protocol.Error.invalid_request
           "background request differs from the persisted job intent")
  in
  let stored = B.policy request in
  let remaining =
    Time_ns.diff
      (Agent_protocol.Timestamp.to_time_ns deadline)
      (Agent_protocol.Timestamp.to_time_ns (now ()))
    |> Time_ns.Span.to_sec
    |> Float.min stored.execution.wall_seconds
  in
  let%bind () =
    match job.status, Float.(remaining > 0.) with
    | Running, true -> Ok ()
    | _ ->
      Error
        (Agent_protocol.Error.invalid_request
           "background job is not runnable within its deadline")
  in
  let effective : P.policy =
    { stored with execution = { stored.execution with wall_seconds = remaining } }
  in
  let limits = P.script_limits_for effective in
  let work control =
    let%bind prepared =
      B.prepare
        ~env
        ~policy
        ~current_capabilities:(fun () ->
          Script_tool_calls.current_capabilities script_tools)
        request
    in
    let selected, input =
      match prepared with
      | B.Tool { capabilities; input; _ } -> capabilities, input
      | Script { prepared; input; _ } ->
        Chat_response.One_off_script.capabilities prepared, input
    in
    let%bind root =
      I.create
        { id = Agent_protocol.Id.Invocation.create ()
        ; session_id = job.session_id
        ; generation = job.generation
        ; origin = Script
        ; provider_call_id = None
        ; call_entry_id = None
        ; parent_invocation = None
        ; parent_job = Some job.id
        ; tool_name = "chatml.background"
        ; implementation_revision = B.fingerprint request
        ; capability_fingerprint = C.fingerprint selected
        ; input
        ; created_at = now ()
        ; deadline = Some deadline
        }
    in
    execute ~invocation:root (fun ~dispatched ->
      N.with_dispatched_scope ~execute ~selected ~invocation:dispatched (fun () ->
        let%bind borrowed = N.borrow () in
        let moderate call =
          let%bind outcome = moderate_tool dispatched call in
          match outcome with
          | None -> Ok None
          | Some (outcome : M.Outcome.t) ->
            let%map () = Requests.emit outcome.runtime_requests in
            (match
               Chat_response.Runtime_semantics.should_end_session outcome.runtime_requests
             with
             | Some _ -> Some (M.Tool_moderation.Reject "The session has ended.")
             | None -> outcome.tool_moderation)
        in
        let perform () =
          Native_tool_moderation.with_handler ~observer ~prepare:moderate (fun () ->
            match prepared with
            | B.Tool { reference; input; _ } ->
              Option.iter control ~f:(fun control ->
                control.Chatml.Chatml_lang.before_effect ~name:"Tool.call" ~spawned:false);
              Script_tool_calls.call_background
                ?observer
                script_tools
                ~borrowed
                ~limits
                ~max_nested_calls:effective.execution.max_calls
                ~moderate
                ~name:reference.name
                ~args:input
              |> Result.map_error ~f:(fun code ->
                failure code "The background tool call could not be completed.")
            | Script { prepared; input; _ } ->
              One_off_execution.run
                ?observer
                ~env
                ~prepared
                ~borrowed
                ~script_tools
                ~input
                ~limits
                ~allocation_bytes:effective.execution.allocation_bytes
                ~max_nested_calls:effective.execution.max_calls
                ~max_invocation_depth:effective.execution.max_invocation_depth
                ~now
                ~moderate_tool
                ~prepare_outcome
                ()
              |> Result.map_error ~f:(fun _ ->
                failure
                  "background.execution_failed"
                  "The background script could not be completed.")
              |> Result.bind ~f:(fun result ->
                let%bind () =
                  Requests.emit result.runtime_requests
                  |> Result.map_error ~f:(fun _ ->
                    failure
                      "background.request_scope"
                      "Background runtime requests could not be retained.")
                in
                match result.resolved.status with
                | Resolved outcome -> Ok outcome
                | _ ->
                  Error
                    (failure
                       "background.invalid_outcome"
                       "Background script has no resolved outcome.")))
        in
        let outcome =
          match perform () with
          | Ok outcome | Error outcome -> outcome
        in
        let%bind () = I.validate_outcome outcome in
        let%bind () =
          Agent_protocol.Json_codec.validate_limits
            ~max_depth:effective.execution.max_depth
            ~max_bytes:effective.max_output_bytes
            (I.outcome_to_json outcome)
        in
        let%bind () =
          prepare_outcome outcome
          |> Result.map_error ~f:(fun _ ->
            Agent_protocol.Error.invalid_request
              "background outcome failed host validation")
        in
        Option.iter control ~f:(fun control ->
          control.Chatml.Chatml_lang.before_json_import (I.outcome_to_json outcome));
        Ok outcome))
  in
  let result, collected =
    Requests.collect (fun () ->
      Chatml_execution.with_host_budget ~env ~policy:(Bounded effective.execution) work
      |> Result.map_error ~f:(fun error ->
        Agent_protocol.Error.create
          Resource_limit
          ~message:error.Chatml_execution.message
          ~retryable:false
          ())
      |> Result.join)
  in
  let%map resolved = result in
  { resolved; runtime_requests = collected }
;;
