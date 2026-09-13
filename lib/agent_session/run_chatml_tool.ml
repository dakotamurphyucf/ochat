open Core
module I = Agent_protocol.Invocation
module N = Native_tool_invocation
module Q = Chat_response.One_off_request
module P = Chat_response.One_off_script
module D = Chatmd_shell_spec.Diagnostic

type response =
  { outcome : I.outcome
  ; script_invocation : I.t option
  ; runtime_requests : Chat_response.Moderation.Runtime_request.t list
  }

let name = "run_chatml"
let parameters = Q.parameters

let rejected diagnostics =
  let code =
    match diagnostics with
    | [] -> "chatml.invalid_request"
    | diagnostic :: _ -> diagnostic.D.code
  in
  { outcome =
      I.Fail
        { code
        ; message = "The one-off script could not be prepared."
        ; retryable = false
        ; details =
            `Object [ "diagnostics", `Array (List.map diagnostics ~f:D.jsonaf_of_t) ]
        }
  ; script_invocation = None
  ; runtime_requests = []
  }
;;

let timeout () =
  { outcome =
      I.Fail
        { code = "chatml.execution_timeout"
        ; message = "The one-off request deadline elapsed."
        ; retryable = false
        ; details = `Null
        }
  ; script_invocation = None
  ; runtime_requests = []
  }
;;

let output response =
  Openai.Responses.Tool_output.Output.Text
    (Jsonaf.to_string (I.outcome_to_json response.outcome))
;;

let execute ?observer ~env ~policy ~script_tools ~now ~moderate_tool ~prepare_outcome json
  =
  let open Result.Let_syntax in
  let%bind borrowed = N.borrow () in
  let%bind ceiling = N.borrowed_capabilities borrowed in
  match Q.decode ~policy json with
  | Error diagnostics -> Ok (rejected diagnostics)
  | Ok request ->
    let parent = N.borrowed_invocation borrowed in
    let seconds =
      match parent.context.deadline with
      | None -> request.policy.execution.wall_seconds
      | Some deadline ->
        Float.min
          request.policy.execution.wall_seconds
          (Time_ns.diff
             (Agent_protocol.Timestamp.to_time_ns deadline)
             (Agent_protocol.Timestamp.to_time_ns (now ()))
           |> Time_ns.Span.to_sec)
    in
    (match Float.(seconds <= 0.) with
     | true -> Ok (timeout ())
     | false ->
       let clock = Eio.Stdenv.mono_clock env in
       let started = Eio.Time.Mono.now clock in
       let requests = ref [] in
       let run () =
         let compilation =
           { request.policy.compilation with
             wall_seconds = Float.min seconds request.policy.compilation.wall_seconds
           }
         in
         match
           P.prepare_in_domain
             ~limits:compilation
             ~env
             ~capabilities:ceiling
             ~tools:request.tools
             ~source:request.source
             ()
         with
         | Error diagnostics -> Ok (rejected diagnostics)
         | Ok prepared ->
           let elapsed =
             Mtime.span started (Eio.Time.Mono.now clock) |> Mtime.Span.to_float_ns
           in
           let remaining = seconds -. (elapsed /. 1e9) in
           (match Float.(remaining <= 0.) with
            | true -> Ok (timeout ())
            | false ->
              let limits = Q.script_limits request in
              let limits =
                { limits with
                  wall_time =
                    Chatmd_shell_spec.Duration.parse (Float.to_string remaining ^ "s")
                    |> Result.ok_or_failwith
                }
              in
              let result =
                One_off_execution.run
                  ?observer
                  ~env
                  ~prepared
                  ~borrowed
                  ~script_tools
                  ~input:request.input
                  ~limits
                  ~allocation_bytes:request.policy.execution.allocation_bytes
                  ~max_nested_calls:request.policy.execution.max_calls
                  ~max_invocation_depth:request.policy.execution.max_invocation_depth
                  ~now
                  ~moderate_tool:(fun invocation call ->
                    let%map outcome = moderate_tool invocation call in
                    Option.iter outcome ~f:(fun outcome ->
                      requests
                      := !requests
                         @ outcome.Chat_response.Moderation.Outcome.runtime_requests);
                    outcome)
                  ~prepare_outcome
                  ()
              in
              (match result with
               | Error _ ->
                 Ok
                   { outcome =
                       I.Fail
                         { code = "chatml.execution_failed"
                         ; message = "The owned one-off execution could not be completed."
                         ; retryable = false
                         ; details = `Null
                         }
                   ; script_invocation = None
                   ; runtime_requests = !requests
                   }
               | Ok result ->
                 let%map outcome =
                   match result.resolved.status with
                   | Resolved outcome -> Ok outcome
                   | _ ->
                     Error
                       (Agent_protocol.Error.invalid_request
                          "one-off executor returned an unresolved invocation")
                 in
                 { outcome
                 ; script_invocation = Some result.resolved
                 ; runtime_requests = result.runtime_requests
                 }))
       in
       (match Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock seconds) run with
        | response -> response
        | exception Eio.Time.Timeout ->
          Ok { (timeout ()) with runtime_requests = !requests }))
;;

type services =
  { script_tools : Script_tool_calls.t
  ; observer : I.observer option
  ; now : unit -> Agent_protocol.Timestamp.t
  ; moderate_tool :
      I.t
      -> Chat_response.Moderation.Tool_call.t
      -> (Chat_response.Moderation.Outcome.t option, string) result
  ; prepare_outcome : I.outcome -> (unit, string) result
  }

let registration ~env ~policy ~services =
  let module Definition = struct
    type input = Jsonaf.t

    let name = name

    let description =
      Some
        "Execute a one-off ChatML main : json -> json task using an explicit tool \
         subset. Calls use f(x, y). Returns one structured outcome without creating a \
         session. Read runtime.native.requests for request limits and result handling."
    ;;

    let type_ = "function"
    let parameters = parameters
    let input_of_string = Jsonaf.of_string
  end
  in
  let implementation =
    Ochat_function.create_function
      (module Definition)
      ~strict:false
      (fun request ->
         (* Probe both lexical scopes before acquiring host services or compiling.
         A callable descriptor alone does not provide an execution owner. *)
         ignore
           (N.borrow ()
            |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
            |> Result.ok_or_failwith
            : N.borrowed);
         Chat_response.Runtime_request_scope.emit [] |> Result.ok_or_failwith;
         let services = services () |> Result.ok_or_failwith in
         execute
           ?observer:services.observer
           ~env
           ~policy
           ~script_tools:services.script_tools
           ~now:services.now
           ~moderate_tool:(fun invocation call ->
             let open Result.Let_syntax in
             let%bind outcome = services.moderate_tool invocation call in
             let%map () =
               match outcome with
               | None -> Ok ()
               | Some outcome ->
                 Chat_response.Runtime_request_scope.emit outcome.runtime_requests
             in
             outcome)
           ~prepare_outcome:services.prepare_outcome
           request
         |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
         |> Result.ok_or_failwith
         |> output)
  in
  Chat_response.Agent_runtime.
    { implementation
    ; implementation_revision =
        Chatmd_shell_spec.Source_ref.digest "ochat.run-chatml.native.v1"
    ; result_contract = Chat_response.Tool_capability.Invocation_v1
    ; authoring_metadata =
        Some
          Chatmd_shell_spec.Authoring_metadata.
            { authoring = Some (Chat_response.Authoring_validation.help One_off_script)
            ; helper = None
            }
    }
;;
