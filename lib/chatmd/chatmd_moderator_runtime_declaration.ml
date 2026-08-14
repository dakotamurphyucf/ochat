open! Core
module D = Chatmd_shell_spec.Diagnostic
module MC = Chatmd_shell_spec.Manifest_compiler

let error source code message =
  D.error ~source ~path:[ "moderator_runtime" ] ~code message
;;

let has_content children =
  List.exists children ~f:(function
    | Chatmd_ast.Text text -> not (String.is_empty (String.strip text))
    | Element _ -> true)
;;

let parse ~source = function
  | Chatmd_ast.Element (Moderator_runtime, attributes, children) ->
    if has_content children
    then
      Error
        [ error source "shell.moderator_runtime_content" "moderator_runtime must be empty"
        ]
    else (
      match
        Chatmd_attributes.create
          ~source
          ~path:[ "moderator_runtime" ]
          ~allowed:[ "shell_runtime" ]
          attributes
      with
      | Error diagnostic -> Error [ diagnostic ]
      | Ok attributes ->
        Chatmd_attributes.required attributes "shell_runtime"
        |> Result.map ~f:(fun runtime -> MC.{ runtime; source })
        |> Result.map_error ~f:List.return)
  | _ ->
    Error
      [ error
          source
          "shell.invalid_moderator_runtime"
          "expected moderator_runtime element"
      ]
;;
