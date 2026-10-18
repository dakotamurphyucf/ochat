open! Core
module CM = Prompt.Chat_markdown

let max_nodes = 16_384
let max_text_bytes = 8 * 1024 * 1024

type ordinary_agent_tools =
  | Denied
  | Registered

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

let admit_surface ~selected ~ordinary_agent_tools ~revision ~parsed ~input ~checkpoint =
  let open Result.Let_syntax in
  let needs_dynamic_source = ref false in
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
  let admit_basic ~inline_image (basic : CM.basic_content_item) =
    let%bind () = poll () in
    match basic.image_url, basic.document_url with
    | None, None ->
      if
        String.equal basic.type_ "text"
        && (not basic.is_local)
        && (not basic.cleanup_html)
        && not basic.markdown
      then Option.value_map basic.text ~default:(Ok ()) ~f:text
      else denied ~selected ()
    | Some { url }, None when selected ->
      if not inline_image then needs_dynamic_source := true;
      let%bind () = text url in
      Option.value_map basic.text ~default:(Ok ()) ~f:text
    | None, Some url when selected ->
      needs_dynamic_source := true;
      let%bind () = text url in
      Option.value_map basic.text ~default:(Ok ()) ~f:text
    | Some _, Some _ | Some _, None | None, Some _ -> denied ~selected ()
  in
  let rec content ~inline_image ~depth = function
    | CM.Basic basic -> admit_basic ~inline_image basic
    | CM.Agent agent ->
      if not selected
      then denied ~selected ()
      else if depth >= 128
      then limited ()
      else (
        let%bind () = poll () in
        let%bind () = text agent.url in
        List.fold_result agent.items ~init:() ~f:(fun () item ->
          content ~inline_image:false ~depth:(depth + 1) item))
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
        List.fold_result items ~init:() ~f:(fun () item ->
          content ~inline_image:(String.equal message.role "assistant") ~depth:0 item))
  in
  let tool (declaration : CM.tool) =
    match declaration with
    (* Tool_call.run_tool intercepts this exact built-in through [on_fork],
       outside the selected child dispatcher. Its child owner is separate. *)
    | CM.Builtin "fork" -> denied ~selected ()
    | CM.Builtin _ -> Ok ()
    | Read_file _ | Custom _ | Shell _ | Mcp _ | Extension _ -> Ok ()
    | Agent declaration ->
      (match ordinary_agent_tools with
       | Registered when selected ->
         let%bind () = text declaration.name in
         let%bind () =
           Option.value_map declaration.description ~default:(Ok ()) ~f:text
         in
         text declaration.agent
       | Registered | Denied -> denied ~selected ())
    | Persistent_agent _ | Inherited _ -> denied ~selected ()
  in
  let moderator_script (script : CM.script) =
    let%bind () = text script.id in
    let%bind () = text script.language in
    let%bind () = text script.kind in
    match script.source with
    | Inline source -> text source
    | Src { path; source_text } ->
      let%bind () = text path in
      text source_text
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
    | Script script when selected -> moderator_script script
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
  let%map () =
    List.fold_result input ~init:() ~f:(fun () item ->
      content ~inline_image:false ~depth:0 item)
  in
  !needs_dynamic_source
;;

let admit_literal ~revision ~parsed ~checkpoint =
  admit_surface
    ~selected:false
    ~ordinary_agent_tools:Denied
    ~revision
    ~parsed
    ~input:[]
    ~checkpoint
  |> Result.map ~f:ignore
;;

let admit_selected ~revision ~parsed ~input ~checkpoint =
  admit_surface
    ~selected:true
    ~ordinary_agent_tools:Denied
    ~revision
    ~parsed
    ~input
    ~checkpoint
  |> Result.map ~f:ignore
;;

let classify_selected ~ordinary_agent_tools ~revision ~parsed ~input ~checkpoint =
  admit_surface ~selected:true ~ordinary_agent_tools ~revision ~parsed ~input ~checkpoint
;;
