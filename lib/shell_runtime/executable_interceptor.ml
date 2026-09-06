open! Core

let command = function
  | [] -> Error "hook returned an empty rewrite"
  | program :: arguments -> Ok (Shell_access.Command.create program arguments)
;;

let exit_status = function
  | Hook_protocol.Exited code -> `Exited code
  | Signaled signal -> `Signaled signal
;;

let before worker context =
  let open Result.Let_syntax in
  let%bind action = Hook_worker.invoke worker (Hook_payload.context ~event:"before_command" context) in
  match action with
  | Hook_protocol.Continue -> Ok Shell_access.Interceptor.Continue
  | Rewrite argv ->
    Result.map_error (command argv) ~f:(fun message ->
      Hook_worker.{ code = "shell.hook_rewrite"; message; stderr = None })
    |> Result.map ~f:(fun command -> Shell_access.Interceptor.Rewrite command)
  | Respond { status; stdout; stderr } ->
    Ok
      (Respond
         { command = context.command
         ; executable = None
         ; status = exit_status status
         ; stdout
         ; stderr
         ; stdout_truncated = false
         ; stderr_truncated = false
         ; intercepted_by = None
         ; untrusted_output = true
         })
  | Reject reason -> Ok (Reject reason)
  | _ -> Error Hook_worker.{ code = "shell.hook_action"; message = "invalid before action"; stderr = None }
;;

let after worker result =
  let open Result.Let_syntax in
  let%bind action = Hook_worker.invoke worker (Hook_payload.command_result result) in
  match action with
  | Hook_protocol.Continue -> Ok result
  | Replace_result { status; stdout; stderr } ->
    Ok { result with status = exit_status status; stdout; stderr; untrusted_output = true }
  | Reject reason ->
    Error Hook_worker.{ code = "shell.hook_reject"; message = reason; stderr = None }
  | _ -> Error Hook_worker.{ code = "shell.hook_action"; message = "invalid after action"; stderr = None }
;;
