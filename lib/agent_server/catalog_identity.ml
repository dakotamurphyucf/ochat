open! Core

let generator domain value =
  let raw = Digestif.SHA256.(digest_string (domain ^ "\000" ^ value) |> to_raw_string) in
  Agent_protocol.Id.Generator.create ~bytes:(fun length -> String.prefix raw length)
;;

let prompt_definition value =
  Agent_protocol.Id.Prompt_definition.create_with (generator "prompt-definition:v1" value)
;;

let workspace_definition value =
  Agent_protocol.Id.Workspace_definition.create_with
    (generator "workspace-definition:v1" value)
;;
