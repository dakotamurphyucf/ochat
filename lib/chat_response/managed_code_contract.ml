open Core
module CM = Prompt.Chat_markdown
module Spec = Chatmd_shell_spec.Extension_spec
module D = Chatmd_shell_spec.Diagnostic

let validate ~original ~selected ~checkpoint =
  let open Result.Let_syntax in
  let fail () =
    Error
      [ D.error
          ~code:"chatml.resource_changed"
          "selected managed code changes its original tool authority"
      ]
  in
  let poll = checkpoint in
  let equal_string left right =
    poll ();
    if Int.equal (String.length left) (String.length right)
    then (
      let rec chunks offset =
        if offset = String.length left
        then true
        else (
          poll ();
          let next = Int.min (String.length left) (offset + 4096) in
          let rec equal_bytes index =
            if index = next
            then chunks next
            else if Char.equal (String.get left index) (String.get right index)
            then equal_bytes (index + 1)
            else false
          in
          equal_bytes offset)
      in
      chunks 0)
    else false
  in
  let equal_option equal left right =
    match left, right with
    | None, None -> true
    | Some left, Some right -> equal left right
    | None, Some _ | Some _, None -> false
  in
  let equal_list equal left right =
    let rec loop left right =
      poll ();
      match left, right with
      | [], [] -> true
      | left :: lefts, right :: rights -> equal left right && loop lefts rights
      | [], _ :: _ | _ :: _, [] -> false
    in
    loop left right
  in
  let collect items =
    let rec loop count scripts tools helps = function
      | [] -> Ok (List.rev scripts, List.rev tools, List.rev helps)
      | item :: rest ->
        poll ();
        if count >= 16_384
        then fail ()
        else (
          match item with
          | CM.Extension_script script ->
            loop (count + 1) (script :: scripts) tools helps rest
          | Tool (Extension tool) -> loop (count + 1) scripts (tool :: tools) helps rest
          | Authoring_help help -> loop (count + 1) scripts tools (help :: helps) rest
          | Msg _
          | Developer _
          | System _
          | User _
          | Assistant _
          | Tool_call _
          | Tool_response _
          | Reasoning _
          | Config _
          | Script _
          | Shell_script _
          | Shell_runtime _
          | Moderator_runtime _
          | Tool
              ( Builtin _
              | Read_file _
              | Custom _
              | Shell _
              | Agent _
              | Mcp _
              | Inherited _
              | Persistent_agent _ )
          | Authoring_context _ -> loop (count + 1) scripts tools helps rest)
    in
    loop 0 [] [] [] items
  in
  let schema (left : Spec.schema) (right : Spec.schema) =
    equal_string left.source_text right.source_text
    && equal_string left.source_sha256 right.source_sha256
  in
  let implementation (left : Spec.implementation) (right : Spec.implementation) =
    match left, right with
    | Spec.Moderator left, Moderator right -> equal_string left right
    | Standalone left, Standalone right ->
      equal_string left.script right.script
      && equal_string left.entrypoint right.entrypoint
    | Moderator _, Standalone _ | Standalone _, Moderator _ -> false
  in
  let tool (left : Spec.tool) (right : Spec.tool) =
    Int.equal left.version right.version
    && equal_string left.name right.name
    && equal_option equal_string left.description right.description
    && implementation left.implementation right.implementation
    && schema left.input_schema right.input_schema
    && schema left.output_schema right.output_schema
    && equal_option schema left.completion_schema right.completion_schema
    && equal_list equal_string left.uses right.uses
  in
  let script (left : Spec.script) (right : Spec.script) =
    poll ();
    Int.equal left.version right.version
    && equal_string left.id right.id
    && Spec.equal_script_kind left.kind right.kind
    && Chatmd_shell_spec.Chatmd_script_spec.equal_limits left.limits right.limits
  in
  let help (left : Spec.authoring_help) (right : Spec.authoring_help) =
    equal_string left.tool right.tool
    && Int.equal left.help.version right.help.version
    && equal_string left.help.package right.help.package
    && equal_list
         Chatmd_shell_spec.Authoring_metadata.equal_task
         left.help.tasks
         right.help.tasks
    && equal_list equal_string left.help.topics right.help.topics
    && equal_list
         Chatmd_shell_spec.Authoring_metadata.equal_helper
         left.help.required_helpers
         right.help.required_helpers
  in
  let%bind old_scripts, old_tools, old_helps = collect original in
  let%bind scripts, tools, helps = collect selected in
  if
    equal_list script old_scripts scripts
    && equal_list tool old_tools tools
    && equal_list help old_helps helps
  then Ok ()
  else fail ()
;;
