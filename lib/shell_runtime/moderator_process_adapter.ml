module Runtime_result = Result
open! Core
module Lang = Chatml.Chatml_lang

let error code message = Error (sprintf "[%s] %s" code message)

let arguments = function
  | Lang.VArray values ->
    Array.to_list values
    |> List.map ~f:(function
      | Lang.VString value -> Ok value
      | _ -> error "shell.moderator_invalid_arguments" "Process.run args must be strings")
    |> Result.all
  | _ ->
    error
      "shell.moderator_invalid_arguments"
      "Process.run args must be an array of strings"
;;

let execute registry runtime command arguments =
  let request =
    Shell_access.Request.command (Shell_access.Command.create command arguments)
  in
  match
    Shell_access.Executor.run
      (Runtime.executor_config runtime)
      { request
      ; input = Shell_access.Input.Empty
      ; rationale = Some "ChatML moderator Process.run"
      ; origin = Shell_access.Context.Moderator
      }
  with
  | Error executor_error ->
    let executor_error = Runtime_result.error_of_executor executor_error in
    error executor_error.code executor_error.message
  | Ok executor_result ->
    let manifest = Registry.manifest registry in
    let result =
      Runtime_result.of_executor
        ~runtime_id:(Runtime.id runtime)
        ~manifest_sha256:manifest.Chatmd_shell_spec.Manifest.sha256
        executor_result
    in
    Ok (result.stdout ^ result.stderr)
;;

let handler ~registry ~runtime_id _session ~command ~args =
  match Registry.runtime registry runtime_id with
  | None ->
    error
      "shell.moderator_runtime_missing"
      ("moderator shell runtime is not instantiated: " ^ runtime_id)
  | Some runtime -> Result.bind (arguments args) ~f:(execute registry runtime command)
;;
