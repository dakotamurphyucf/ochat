open Core
open Notty

let chrome_attr = A.(bg (gray 2) ++ fg (gray 15))
let heading_attr = A.(Highlight_styles.fg_hex "#13A3F2" ++ st bold)
let muted_attr = A.(fg (gray 12))
let truncation_attr = A.(Highlight_styles.fg_hex "#FF5370" ++ st bold)

let safe_string attr text =
  match I.string attr (Util.sanitize ~strip:false text) with
  | image -> image
  | exception _ -> I.string attr ""
;;

let row ~width attr text = safe_string attr text |> I.hsnap ~align:`Left (Int.max 0 width)

let output_text = function
  | Openai.Responses.Tool_output.Output.Text text -> text
  | Content parts ->
    List.map parts ~f:(function
      | Openai.Responses.Tool_output.Output_part.Input_text { text } -> text
      | Input_image { image_url; _ } -> Printf.sprintf "<image src=\"%s\" />" image_url)
    |> String.concat ~sep:"\n"
;;

let render_message ~width ~hi_engine ?(tool_output = None) ~role ~text () =
  Renderer_component_message.render_message
    ~width
    ~selected:false
    ~tool_output
    ~role
    ~text
    ~hi_engine
    ()
;;

let role_of_channel = function
  | `Assistant -> "assistant"
  | `Reasoning -> "reasoning"
  | `Stdout -> "tool_output"
  | `Stderr -> "error"
  | `Activity -> "system"
;;

let render_text_entry ~width ~hi_engine (channel, text) =
  render_message ~width ~hi_engine ~role:(role_of_channel channel) ~text ()
;;

let classify_tool_output ~name ~payload =
  let path =
    match Jsonaf.of_string payload with
    | exception _ -> None
    | `Object fields ->
      List.find_map fields ~f:(fun (key, value) ->
        match key, value with
        | ("file" | "path"), `String path -> Some path
        | _ -> None)
    | _ -> None
  in
  match String.lowercase name with
  | "apply_patch" -> Types.Apply_patch
  | "read_file" -> Types.Read_file { path }
  | "read_directory" -> Types.Read_directory { path }
  | name -> Types.Other { name = Some name }
;;

let progress_text progress =
  List.filter_map progress ~f:(fun (channel, text) ->
    match channel with
    | `Stdout | `Stderr | `Activity -> Some text
    | `Assistant | `Reasoning -> None)
  |> String.concat
;;

let terminal_status = function
  | None -> None
  | Some Ochat_function.Trace.Returned -> Some (Highlight_styles.fg_green, "✓ Returned")
  | Some Raised -> Some (Highlight_styles.fg_red, "✗ Raised")
  | Some Cancelled -> Some (Highlight_styles.fg_yellow, "⊘ Cancelled")
;;

let append_status ~width message (attr, text) =
  let message = I.vcrop 0 1 message in
  I.vcat [ message; row ~width attr text; row ~width A.empty "" ]
;;

let render_status_only ~width ~hi_engine status =
  let header =
    Renderer_component_message.render_header_line
      ~width
      ~selected:false
      ~role:"tool_output"
      ~hi_engine
      ()
  in
  let attr, text = status in
  I.vcat
    [ row ~width A.empty ""
    ; header
    ; row ~width A.empty ""
    ; row ~width attr text
    ; row ~width A.empty ""
    ]
;;

let render_nested_tool
      ~width
      ~hi_engine
      (_call_id, name, _kind, payload, progress, outcome, output)
  =
  let invocation =
    render_message
      ~width
      ~hi_engine
      ~role:"tool"
      ~text:(Printf.sprintf "%s(%s)" name payload)
      ()
  in
  let displayed_output =
    match output with
    | Some output -> output_text output
    | None ->
      let progress = progress_text progress in
      if not (String.is_empty progress)
      then progress
      else if Option.is_none outcome
      then "(running…)"
      else ""
  in
  let status = terminal_status outcome in
  let result =
    if String.is_empty (String.strip displayed_output)
    then Option.map status ~f:(render_status_only ~width ~hi_engine)
    else (
      let tool_output = classify_tool_output ~name ~payload in
      let message =
        render_message
          ~width
          ~hi_engine
          ~tool_output:(Some tool_output)
          ~role:"tool_output"
          ~text:displayed_output
          ()
      in
      Some (Option.value_map status ~default:message ~f:(append_status ~width message)))
  in
  match result with
  | None -> invocation
  | Some result -> I.vcat [ invocation; result ]
;;

let render_entry ~width ~hi_engine entry =
  match Model.progress_entry_text_view entry with
  | Some text -> render_text_entry ~width ~hi_engine text
  | None ->
    Model.progress_entry_tool_view entry
    |> Option.value_exn
    |> render_nested_tool ~width ~hi_engine
;;

let render_block ~width ~hi_engine block =
  match Model.agent_render_block_view block with
  | Invocation { name; payload; agent_page_kind } ->
    let role =
      match agent_page_kind with
      | Chat_response.Tool_execution_event.Subagent -> name ^ " Agent"
      | Shell_script -> name
    in
    render_message ~width ~hi_engine ~role ~text:(Printf.sprintf "%s(%s)" name payload) ()
  | Truncation -> row ~width truncation_attr "[earlier output truncated]"
  | Waiting -> row ~width muted_attr "Waiting for progress…"
  | Progress entry -> render_entry ~width ~hi_engine entry
  | Status outcome ->
    (match terminal_status (Some outcome) with
     | None -> I.empty
     | Some (attr, text) -> row ~width attr text)
;;

let get_block_image ~call ~render block =
  match Model.find_agent_render_cache call block with
  | Some (image, _) -> image
  | None ->
    let image = render block in
    Model.set_agent_render_cache call block ~image;
    image
;;

let rebuild_geometry ~call ~blocks ~render =
  let len = List.length blocks in
  let ids = Array.create ~len 0 in
  let revisions = Array.create ~len 0 in
  let heights = Array.create ~len 0 in
  List.iteri blocks ~f:(fun index block ->
    let image = get_block_image ~call ~render block in
    ids.(index) <- Model.agent_render_block_id block;
    revisions.(index) <- Model.agent_render_block_revision block;
    heights.(index) <- I.height image);
  let prefix = Array.create ~len:(len + 1) 0 in
  Array.iteri heights ~f:(fun index height ->
    prefix.(index + 1) <- prefix.(index) + height);
  Model.set_agent_render_geometry call ~block_ids:ids ~revisions ~heights ~prefix
;;

let update_geometry ~call ~blocks ~render =
  let ids = Model.agent_render_block_ids call in
  let revisions = Model.agent_render_block_revisions call in
  let geometry = Model.agent_render_geometry call in
  let len = List.length blocks in
  let shape_matches =
    Array.length ids = len
    && Array.length revisions = len
    && Renderer_virtual_list.Geometry.shape_matches geometry ~length:len
    && List.for_alli blocks ~f:(fun index block ->
      Int.equal ids.(index) (Model.agent_render_block_id block))
  in
  if not shape_matches
  then rebuild_geometry ~call ~blocks ~render
  else
    List.iteri blocks ~f:(fun index block ->
      let revision = Model.agent_render_block_revision block in
      if not (Int.equal revisions.(index) revision)
      then (
        let image = get_block_image ~call ~render block in
        let height = I.height image in
        revisions.(index) <- revision;
        Renderer_virtual_list.Geometry.update_height geometry ~index ~height))
;;

let render_output ~width ~height ~hi_engine ~model = function
  | None -> row ~width muted_attr "No active tool calls."
  | Some call ->
    Model.prepare_agent_render_width call ~width;
    let blocks = Model.agent_call_render_blocks call in
    let render = render_block ~width ~hi_engine in
    Model.prune_agent_render_cache
      call
      ~block_ids:(List.map blocks ~f:Model.agent_render_block_id);
    update_geometry ~call ~blocks ~render;
    let geometry = Model.agent_render_geometry call in
    let scroll_box = Model.agent_scroll_box model in
    let viewport =
      Renderer_virtual_list.Viewport.compute
        ~geometry
        ~requested_scroll:(Notty_scroll_box.scroll scroll_box)
        ~height
        ~follow_bottom:(Model.agent_auto_follow model)
    in
    Renderer_virtual_list.render ~viewport ~width ~image_at_index:(fun index ->
      let block = List.nth_exn blocks index in
      get_block_image ~call ~render block)
;;

module For_testing = struct
  let render_block_ids ~width ~height ~model ~render =
    match Model.selected_agent_call model with
    | None -> []
    | Some call ->
      Model.prepare_agent_render_width call ~width;
      let blocks = Model.agent_call_render_blocks call in
      Model.prune_agent_render_cache
        call
        ~block_ids:(List.map blocks ~f:Model.agent_render_block_id);
      update_geometry ~call ~blocks ~render;
      let geometry = Model.agent_render_geometry call in
      let viewport =
        Renderer_virtual_list.Viewport.compute
          ~geometry
          ~requested_scroll:(Notty_scroll_box.scroll (Model.agent_scroll_box model))
          ~height
          ~follow_bottom:(Model.agent_auto_follow model)
      in
      Renderer_virtual_list.Viewport.visible_indices viewport
      |> List.map ~f:(fun index ->
        let block = List.nth_exn blocks index in
        ignore (get_block_image ~call ~render block : I.t);
        Model.agent_render_block_id block)
  ;;
end

let optional_region height image = if height <= 0 then None else Some image

let render ~size:(width, height) ~model =
  let width = Int.max 0 width in
  let height = Int.max 0 height in
  let layout = Agent_page_layout.compute ~screen_w:width ~screen_h:height ~model in
  let calls = Model.active_agent_calls model in
  let running =
    List.count calls ~f:(fun call -> Option.is_none (Model.agent_call_outcome call))
  in
  let selected = Model.selected_agent_call model in
  let header =
    match running, Model.activity model with
    | 0, _ | _, None | _, Some Model.Compacting ->
      Printf.sprintf "Agent tools — %d running · %d calls" running (List.length calls)
      |> row ~width chrome_attr
    | _, Some (Model.Assistant _) ->
      I.hcat
        [ I.string chrome_attr "Agent tools — "
        ; Renderer_component_loader.render
            ~base_attr:chrome_attr
            ~frame:(Model.animation_frame model)
            "Working"
        ; I.string
            chrome_attr
            (Printf.sprintf " · %d running · %d calls" running (List.length calls))
        ]
      |> I.hsnap ~align:`Left width
  in
  let selector = Agent_page_selector.render ~width model in
  let hi_engine = Renderer_highlight_engine.get () in
  let output =
    render_output ~width ~height:layout.scroll_height ~hi_engine ~model selected
  in
  let scroll_box = Model.agent_scroll_box model in
  Notty_scroll_box.set_content scroll_box output;
  if Model.agent_auto_follow model
  then Notty_scroll_box.scroll_to_bottom scroll_box ~height:layout.scroll_height;
  let viewport = Notty_scroll_box.render scroll_box ~width ~height:layout.scroll_height in
  let footer =
    row ~width chrome_attr "Ctrl-G/Esc chat  j/k select  ↑↓/PgUp/PgDn scroll"
  in
  let image =
    [ optional_region layout.header_height header
    ; optional_region layout.selector_height selector
    ; optional_region layout.scroll_height viewport
    ; optional_region layout.footer_height footer
    ]
    |> List.filter_opt
    |> I.vcat
    |> I.vsnap ~align:`Top height
    |> I.hsnap ~align:`Left width
  in
  image, (0, 0)
;;
