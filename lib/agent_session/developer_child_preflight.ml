open! Core
module CM = Prompt.Chat_markdown

let max_nodes = 16_384
let max_text_bytes = 8 * 1024 * 1024

let denied ~selected () =
  let message = "developer child effect needs a selected child owner" in
  if selected
  then Error (Agent_protocol.Error.invalid_request message)
  else
    Error
      (Agent_protocol.Error.create
         Agent_protocol.Error.Invalid_state
         ~message
         ~retryable:false
         ())
;;

let limited () =
  Error
    (Agent_protocol.Error.create
       Agent_protocol.Error.Resource_limit
       ~message:"developer child preflight exceeds host allowance"
       ~retryable:false
       ())
;;

let elements revision parsed =
  let invalid () =
    Error
      (Agent_protocol.Error.create
         Agent_protocol.Error.Invalid_state
         ~message:"developer child projection belongs to another parser"
         ~retryable:false
         ())
  in
  match Prompt_revision.parser_context revision with
  | None -> invalid ()
  | Some actual when not (phys_equal actual parsed) -> invalid ()
  | Some _ ->
    (match Prompt_revision.document_projection revision with
     | None -> Ok (CM.Parsed.elements parsed)
     | Some projection ->
       if phys_equal parsed (CM.Document_projection.parsed projection)
       then Ok (CM.Document_projection.elements projection)
       else invalid ())
;;

let admit_surface ~selected ~revision ~parsed ~input ~checkpoint =
  let open Result.Let_syntax in
  let remaining_nodes = ref max_nodes in
  let remaining_text_bytes = ref max_text_bytes in
  let poll () =
    checkpoint ();
    if !remaining_nodes <= 0
    then limited ()
    else (
      decr remaining_nodes;
      Ok ())
  in
  let text value =
    let%bind () = poll () in
    let length = String.length value in
    if length > !remaining_text_bytes
    then limited ()
    else (
      remaining_text_bytes := !remaining_text_bytes - length;
      Ok ())
  in
  let admit_basic (basic : CM.basic_content_item) =
    let%bind () = poll () in
    if
      String.equal basic.type_ "text"
      && Option.is_none basic.image_url
      && Option.is_none basic.document_url
      && (not basic.is_local)
      && (not basic.cleanup_html)
      && not basic.markdown
    then Option.value_map basic.text ~default:(Ok ()) ~f:text
    else denied ~selected ()
  in
  let content = function
    | CM.Basic basic -> admit_basic basic
    | CM.Agent _ -> denied ~selected ()
  in
  let admit_message (message : CM.msg) =
    let%bind () = poll () in
    if
      Option.is_some message.function_call
      || Option.is_some message.tool_call
      || Option.is_some message.tool_call_id
      || (selected
          && not
               (List.mem
                  [ "system"; "developer"; "user"; "assistant" ]
                  message.role
                  ~equal:String.equal))
    then denied ~selected ()
    else (
      match message.content with
      | None -> Ok ()
      | Some (Text text_value) -> text text_value
      | Some (Items items) ->
        List.fold_result items ~init:() ~f:(fun () item -> content item))
  in
  let tool (declaration : CM.tool) =
    match declaration with
    (* Tool_call.run_tool intercepts this exact built-in through [on_fork],
       outside the selected child dispatcher. Its child owner is separate. *)
    | CM.Builtin "fork" -> denied ~selected ()
    | CM.Builtin _ -> Ok ()
    | Read_file _ | Custom _ | Shell _ | Mcp _ | Extension _ -> Ok ()
    | Agent _ | Persistent_agent _ | Inherited _ -> denied ~selected ()
  in
  let element (element : CM.top_level_elements) =
    let%bind () = poll () in
    match element with
    | CM.Msg message
    | Developer message
    | System message
    | User message
    | Assistant message -> admit_message message
    | Config _ | Reasoning _ -> Ok ()
    | Tool declaration when selected -> tool declaration
    | Shell_runtime _ when selected -> Ok ()
    | Shell_script script when selected ->
      (match script.kind with
       | Chatmd_shell_spec.Chatmd_script_spec.Moderator -> denied ~selected ()
       | Shell_matcher
       | Shell_reviewer
       | Shell_before_interceptor
       | Shell_after_interceptor
       | Shell_effect_analyzer
       | Shell_audit_filter -> Ok ())
    | Extension_script script when selected ->
      (match script.kind with
       | Chatmd_shell_spec.Extension_spec.Tool_script -> Ok ()
       | Moderator_script -> denied ~selected ())
    | Tool_call _
    | Tool_response _
    | Tool _
    | Shell_runtime _
    | Moderator_runtime _
    | Script _
    | Shell_script _
    | Extension_script _
    | Authoring_context _
    | Authoring_help _ -> denied ~selected ()
  in
  let%bind actual_elements = elements revision parsed in
  let%bind () =
    List.fold_result actual_elements ~init:() ~f:(fun () item -> element item)
  in
  List.fold_result input ~init:() ~f:(fun () item -> content item)
;;

let admit_literal ~revision ~parsed ~checkpoint =
  admit_surface ~selected:false ~revision ~parsed ~input:[] ~checkpoint
;;

let admit_selected ~revision ~parsed ~input ~checkpoint =
  admit_surface ~selected:true ~revision ~parsed ~input ~checkpoint
;;
