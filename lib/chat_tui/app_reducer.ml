open Core
open Eio.Std
open Types
module Model = Model
module Redraw_throttle = Redraw_throttle
module Controller = Controller
module Cmd = Cmd
module CM = Prompt.Chat_markdown
module Scroll_box = Notty_scroll_box
module Res = Openai.Responses
module Converter = Chat_response.Converter
module Ctx = Chat_response.Ctx
module Cache = Chat_response.Cache
module Req = Res.Request
module Runtime = App_runtime
module Runtime_semantics = Chat_response.Runtime_semantics
module Moderator_session_controller = Moderator_session_controller

type input_event = App_events.input_event
type internal_event = App_events.internal_event

type app_event =
  [ input_event
  | internal_event
  ]

type typeahead_request =
  { generation : int
  ; base_input : string
  ; base_cursor : int
  }

module Context = struct
  type t =
    { runtime : Runtime.t
    ; shared : App_context.Resources.t
    ; submit : App_submit.Context.t
    ; compaction : App_compaction.Context.t
    ; cancelled : exn
    }
end

let finish_startup_progress ~runtime ~size =
  let model = runtime.Runtime.model in
  let publish () =
    let started_at = Time_ns.now () in
    let published = Renderer_page_chat.publish_startup_history ~size ~model in
    let publication_latency = Time_ns.diff (Time_ns.now ()) started_at in
    if not published then failwith "startup history publication failed";
    publication_latency
  in
  if
    Poly.(Model.chat_materialization model = Model.Chat_page_state.Loading)
    && not (Runtime.startup_render_is_warming runtime)
  then (
    Renderer_page_chat.relayout_history_synchronously ~size ~model;
    let publication_latency = publish () in
    Model.set_chat_materialization_warm model;
    Runtime.record_startup_publication runtime ~publication_latency;
    true)
  else false
;;

module For_testing = struct
  let commit_startup_results ~model results =
    List.for_all results ~f:(fun result ->
      let committed = Model.commit_startup_render_result model result in
      if committed then Renderer_component_message.install_highlights result.highlights;
      committed)
  ;;

  let finish_startup_progress = finish_startup_progress
end

module Placeholders = struct
  let add_placeholder_stream_error (model : Model.t) text : unit =
    let patch = Add_placeholder_message { role = "error"; text } in
    ignore (Model.apply_patch model patch)
  ;;
end

module Cancellation_repair = struct
  module Item = Openai.Responses.Item
  module Output = Openai.Responses.Tool_output.Output

  let truncate s ~max_len =
    if String.length s <= max_len then s else String.prefix s max_len ^ "…"
  ;;

  let synthetic_output ~error = function
    | Item.Function_call call ->
      let args = Util.sanitize ~strip:true call.arguments |> truncate ~max_len:200 in
      Some
        (Item.Function_call_output
           { output =
               Output.Text
                 (Printf.sprintf
                    "Tool call did not complete (call_id=%s, name=%s, arguments=%s).\n\n\
                     %s"
                    call.call_id
                    call.name
                    args
                    error)
           ; call_id = call.call_id
           ; _type = "function_call_output"
           ; id = None
           ; status = None
           })
    | Item.Custom_tool_call call ->
      let input = Util.sanitize ~strip:true call.input |> truncate ~max_len:200 in
      Some
        (Item.Custom_tool_call_output
           { output =
               Output.Text
                 (Printf.sprintf
                    "Tool call did not complete (call_id=%s, name=%s, input=%s).\n\n%s"
                    call.call_id
                    call.name
                    input
                    error)
           ; call_id = call.call_id
           ; _type = "custom_tool_call_output"
           ; id = None
           })
    | _ -> None
  ;;

  let repair ~allocator ~error entries =
    let seen_outputs = Hash_set.create (module String) in
    let rec loop entries acc ~dropping_trailing_reasoning =
      match entries with
      | [] -> Ok acc
      | entry :: rest ->
        let item = History_entry.item entry in
        if dropping_trailing_reasoning
        then (
          match item with
          | Item.Reasoning _ -> loop rest acc ~dropping_trailing_reasoning:true
          | _ -> loop entries acc ~dropping_trailing_reasoning:false)
        else (
          let continue acc = loop rest acc ~dropping_trailing_reasoning:false in
          match item with
          | Item.Function_call_output output ->
            if Hash_set.mem seen_outputs output.call_id
            then continue acc
            else (
              Hash_set.add seen_outputs output.call_id;
              continue (entry :: acc))
          | Item.Custom_tool_call_output output ->
            if Hash_set.mem seen_outputs output.call_id
            then continue acc
            else (
              Hash_set.add seen_outputs output.call_id;
              continue (entry :: acc))
          | Item.Function_call call ->
            if Hash_set.mem seen_outputs call.call_id
            then continue (entry :: acc)
            else (
              Hash_set.add seen_outputs call.call_id;
              let open Result.Let_syntax in
              let%bind output =
                synthetic_output ~error item
                |> Option.value_exn
                |> History_entry.create ~allocator
              in
              continue (entry :: output :: acc))
          | Item.Custom_tool_call call ->
            if Hash_set.mem seen_outputs call.call_id
            then continue (entry :: acc)
            else (
              Hash_set.add seen_outputs call.call_id;
              let open Result.Let_syntax in
              let%bind output =
                synthetic_output ~error item
                |> Option.value_exn
                |> History_entry.create ~allocator
              in
              continue (entry :: output :: acc))
          | _ -> continue (entry :: acc))
    in
    loop (List.rev entries) [] ~dropping_trailing_reasoning:true
  ;;
end

module Stream_apply = App_stream_apply

module Controller_actions = struct
  type t =
    { model : Model.t
    ; runtime : Runtime.t
    ; internal_stream : internal_event Eio.Stream.t
    ; throttler : Redraw_throttle.t
    ; handle_cancel_or_quit : unit -> bool
    }

  let handle_controller_result (t : t) (ev : input_event) controller_result =
    match controller_result with
    | Controller.Redraw ->
      Redraw_throttle.request_redraw t.throttler;
      true
    | Controller.Chat_scrolled changed ->
      App_runtime.reprioritize_target_width_batches t.runtime;
      if changed then Redraw_throttle.request_redraw t.throttler;
      true
    | Controller.Prepare_chat_destination destination ->
      Eio.Stream.add t.internal_stream (`Prepare_chat_destination destination);
      true
    | Controller.Shell_approval_response (id, response) ->
      (match Runtime.respond_shell_approval_by_id t.runtime ~id response with
       | Ok () -> ()
       | Error error ->
         Runtime.add_system_notice
           t.runtime
           ("Shell approval response failed: "
            ^ Sexp.to_string_hum ([%sexp_of: Shell_runtime.Approval_broker.error] error)));
      Eio.Stream.add t.internal_stream `Shell_approval_changed;
      Redraw_throttle.request_redraw t.throttler;
      true
    | Controller.Shell_grant_revoke_requested (generation, grant_id) ->
      Model.mark_shell_grant_revoking t.model ~generation ~grant_id;
      Eio.Stream.add
        t.internal_stream
        (`Shell_grant_revoke_requested (generation, grant_id));
      Redraw_throttle.request_redraw t.throttler;
      true
    | Controller.Shell_management_refresh_requested generation ->
      Eio.Stream.add t.internal_stream (`Shell_management_refresh_requested generation);
      Redraw_throttle.request_redraw t.throttler;
      true
    | Controller.Moderator_input_response response ->
      Eio.Stream.add t.internal_stream (`Moderator_input_response response);
      true
    | Controller.Refresh_messages ->
      let damage = Runtime.refresh_messages t.runtime in
      if Model.projection_damage_requires_redraw damage
      then Redraw_throttle.request_redraw t.throttler;
      true
    | Controller.Submit_input ->
      let submit_request = App_submit.capture_request ~model:t.model in
      App_submit.clear_editor ~model:t.model;
      Eio.Stream.add t.internal_stream (`Submit_requested submit_request);
      Redraw_throttle.request_redraw t.throttler;
      true
    | Controller.Cancel_or_quit -> t.handle_cancel_or_quit ()
    | Controller.Compact_context ->
      Eio.Stream.add t.internal_stream `Compact_requested;
      true
    | Controller.Quit -> false
    | Controller.Unhandled ->
      (match ev with
       | `Paste `End ->
         Log.emit `Warn "Unhandled paste event – this is a bug.";
         true
       | `Paste `Start ->
         Log.emit `Warn "Unhandled paste start event – this is a bug.";
         true
       | _ -> true)
  ;;
end

exception Compaction_cancelled
exception Typeahead_cancelled

let rec is_compaction_cancelled = function
  | Compaction_cancelled -> true
  | Eio.Cancel.Cancelled reason -> is_compaction_cancelled reason
  | _ -> false
;;

let run (ctx : Context.t) =
  let runtime = ctx.runtime in
  let shared = ctx.shared in
  let services = shared.services in
  let streams = shared.streams in
  let ui = shared.ui in
  let env = services.env in
  let ui_sw = services.ui_sw in
  let clock = Eio.Stdenv.clock env in
  let term = ui.term in
  let input_stream = streams.input in
  let internal_stream = streams.internal in
  let redraw_stream = streams.redraw in
  let throttler = ui.throttler in
  let redraw_immediate = ui.redraw_immediate in
  let redraw = ui.redraw in
  let cancelled = ctx.cancelled in
  let model = runtime.Runtime.model in
  let initial_terminal_size = ui.size () in
  let last_terminal_size =
    match Model.chat_materialization model, Model.active_history_width model with
    | Loading, Some width -> ref (width, snd initial_terminal_size)
    | Loading, None | (Resizing | Corridor | Warm), _ -> ref initial_terminal_size
  in
  let resize_generation = ref 0 in
  let destination_generation = ref 0 in
  let resize_settle_delay = 0.075 in
  let quit_via_esc = runtime.Runtime.quit_via_esc in
  let add_system_notice text =
    Runtime.add_system_notice runtime text;
    Redraw_throttle.request_redraw throttler
  in
  let add_system_notice_once ~key text =
    if Runtime.add_system_notice_once runtime ~key text
    then Redraw_throttle.request_redraw throttler
  in
  let now_ms () = Eio.Time.now clock *. 1000. |> Int.of_float in
  let chat_viewport_height () =
    let screen_w, screen_h = ui.size () in
    (Chat_page_layout.compute ~screen_w ~screen_h ~model).scroll_height
  in
  let restart_corridor_preparation_after_replacement () =
    match Model.chat_materialization model, Model.active_history_width model with
    | Model.Chat_page_state.Corridor, Some width ->
      destination_generation := Int.max !destination_generation !resize_generation + 1;
      let request_generation = !destination_generation in
      let _, screen_h = ui.size () in
      let size = width, screen_h in
      let layout = Chat_page_layout.compute ~screen_w:width ~screen_h ~model in
      let anchor =
        Model.capture_resize_anchor model ~viewport_height:layout.scroll_height
      in
      Model.start_width_preparation
        model
        ~request_generation
        ~terminal_size:size
        ~layout:
          { Model.Chat_page_state.input_box_height = layout.input_box_height
          ; history_height = layout.history_height
          ; sticky_height = layout.sticky_height
          ; scroll_height = layout.scroll_height
          }
        ~theme_generation:0
        ~grammar_generation:(Highlight_registry.generation ())
        ~anchor;
      Runtime.submit_initial_target_width_batches runtime;
      Runtime.pump_target_width_completion runtime;
      if Renderer_page_chat.promote_width_preparation ~size ~model ~request_generation
      then ()
      else
        ignore (Model.publish_width_preparation_corridor model ~request_generation : bool)
    | (Loading | Resizing | Warm), _ | Corridor, None -> ()
  in
  let finish_startup_synchronously () =
    if finish_startup_progress ~runtime ~size:(ui.size ())
    then (
      ignore (Eio.Stream.take_nonblocking redraw_stream : unit option);
      redraw_immediate ())
  in
  let start_startup_render () =
    match Runtime.begin_startup_render runtime with
    | None -> finish_startup_synchronously ()
    | Some (generation, domain_mgr, config, code_cache_capacity, cancel, snapshot, jobs)
      ->
      (match snapshot with
       | None ->
         Runtime.close_startup_render runtime;
         finish_startup_synchronously ()
       | Some snapshot ->
         Fiber.fork ~sw:ui_sw (fun () ->
           let outcome =
             Chat_startup_render.render
               ~domain_mgr
               ~config
               ~code_cache_capacity
               ~is_cancelled:(fun () -> Atomic.get cancel)
               ~snapshot
               ~jobs
           in
           Eio.Stream.add internal_stream (`Startup_render_finished (generation, outcome))))
  in
  let restart_startup_render ~size =
    Renderer_page_chat.prepare_startup_history ~size ~model;
    redraw_immediate ();
    start_startup_render ()
  in
  let tui_policy =
    { Runtime_semantics.default_policy with honor_request_compaction = true }
  in
  let sync_pending_input () = Runtime.sync_pending_input runtime in
  let apply_moderator_outcome (outcome : Moderator_session_controller.t) =
    let pending_changed = sync_pending_input () in
    if outcome.request_refresh
    then
      Option.iter runtime.Runtime.moderator ~f:(fun moderator ->
        Runtime.note_overlay_revision
          runtime
          (Chat_response.Moderator_manager.overlay_revision moderator.manager));
    let overlay_damage =
      Runtime.refresh_pending_overlay ~viewport_height:(chat_viewport_height ()) runtime
    in
    List.iter outcome.internal_events_to_enqueue ~f:(Eio.Stream.add internal_stream);
    Option.iter outcome.halt_reason ~f:(fun reason ->
      runtime.Runtime.halted_reason <- Some reason);
    let added_notice = ref false in
    List.iter outcome.system_notices ~f:(fun text ->
      if Runtime.add_system_notice_once runtime ~key:("system:" ^ text) text
      then added_notice := true);
    if
      Option.exists overlay_damage ~f:Model.projection_damage_requires_redraw
      || pending_changed
      || !added_notice
    then Redraw_throttle.request_redraw throttler;
    not (List.is_empty outcome.internal_events_to_enqueue)
  in
  let active_turn_policy = { tui_policy with honor_request_turn = false } in
  let handle_runtime_request (request : Chat_response.Moderation.Runtime_request.t) =
    Moderator_session_controller.of_runtime_request
      ~policy:active_turn_policy
      ~turn_request:Ignore
      request
    |> ignore
  in
  let handle_moderator_drain_error msg =
    Runtime.clear_moderator_dirty runtime;
    Placeholders.add_placeholder_stream_error
      model
      (Printf.sprintf "Moderator background drain failed: %s" msg);
    Redraw_throttle.request_redraw throttler
  in
  let handle_pending_input_block () =
    Runtime.mark_moderator_dirty runtime;
    if sync_pending_input () then Redraw_throttle.request_redraw throttler
  in
  let validation_notice_of_pending_input = function
    | Runtime.Ask_text _ -> "Please enter a response before continuing."
    | Runtime.Ask_choice _ ->
      "Please answer with one of the listed choices before continuing."
  in
  let handle_input_resume_error pending_input msg =
    let notice =
      match pending_input, msg with
      | Runtime.Ask_text _, "Approval.ask_text requires a non-empty response."
      | ( Runtime.Ask_choice _
        , "Approval.ask_choice response must match one of the declared choices." ) ->
        validation_notice_of_pending_input pending_input
      | Runtime.Ask_choice _, _ | Runtime.Ask_text _, _ ->
        Printf.sprintf "Approval response failed: %s" msg
    in
    let pending_changed = sync_pending_input () in
    Model.set_moderator_validation_error model (Some notice);
    if pending_changed then Redraw_throttle.request_redraw throttler
  in
  let drain_moderator_if_idle () =
    if not (Runtime.is_moderator_startup_ready runtime)
    then false
    else if not (Runtime.is_idle runtime)
    then false
    else if Runtime.has_pending_input runtime
    then false
    else (
      let policy = Runtime.runtime_policy runtime in
      if Runtime.should_pause_internal_event_drains ~policy
      then (
        add_system_notice_once
          ~key:"budget:pause-internal-event-drains"
          "Automatic moderator idle drains are paused by budget policy.";
        false)
      else (
        match runtime.Runtime.moderator with
        | None ->
          Runtime.clear_moderator_dirty runtime;
          false
        | Some moderator ->
          (match
             Moderator_session_controller.drain_internal_events
               ~moderator
               ~now_ms:(now_ms ())
               ~history:(Model.history_items model)
               ~available_tools:ctx.submit.streaming.tools
               ~turn_request:(Schedule Runtime.Idle_followup)
           with
           | Ok outcome ->
             if outcome.internal_events_remain
             then Runtime.mark_moderator_dirty runtime
             else Runtime.clear_moderator_dirty runtime;
             apply_moderator_outcome outcome
           | Error msg ->
             if String.equal msg "Session is waiting for UI input."
             then handle_pending_input_block ()
             else handle_moderator_drain_error msg;
             false)))
  in
  let handle_idle_safe_point () =
    let drained =
      if Runtime.is_moderator_dirty runtime && Runtime.is_idle runtime
      then drain_moderator_if_idle ()
      else false
    in
    let refreshed =
      Runtime.refresh_pending_overlay ~viewport_height:(chat_viewport_height ()) runtime
    in
    if Option.exists refreshed ~f:Model.projection_damage_requires_redraw
    then Redraw_throttle.request_redraw throttler;
    drained || Option.is_some refreshed
  in
  let max_input_drain_per_iteration = 4 in
  let typeahead_debounce_sw : Switch.t option ref = ref None in
  let typeahead_pending_request : typeahead_request option ref = ref None in
  let typeahead_debounce_s = 0.2 in
  let is_ctrl_space (ev : input_event) =
    match ev with
    | `Key (`ASCII '@', mods) -> List.mem mods `Ctrl ~equal:Poly.equal
    | `Key (`ASCII ' ', mods) -> List.mem mods `Ctrl ~equal:Poly.equal
    | `Key (`ASCII '\000', _mods) -> true
    | _ -> false
  in
  let cancel_typeahead_debounce () =
    match !typeahead_debounce_sw with
    | None -> ()
    | Some sw ->
      typeahead_debounce_sw := None;
      Switch.fail sw Typeahead_cancelled
  in
  let cancel_running_typeahead () =
    match runtime.Runtime.typeahead_op with
    | None -> ()
    | Some (Runtime.Typeahead { sw; id = _ }) ->
      runtime.Runtime.typeahead_op <- None;
      Switch.fail sw Typeahead_cancelled
    | Some (Runtime.Starting_typeahead { id = _ }) ->
      runtime.Runtime.cancel_typeahead_on_start <- true
  in
  let start_typeahead_worker (req : typeahead_request) : unit =
    if String.is_empty (String.strip req.base_input)
    then ()
    else (
      let op_id = Runtime.alloc_op_id runtime in
      runtime.Runtime.typeahead_op <- Some (Runtime.Starting_typeahead { id = op_id });
      runtime.Runtime.cancel_typeahead_on_start <- false;
      Fiber.fork ~sw:ui_sw (fun () ->
        match
          Switch.run
          @@ fun sw ->
          Eio.Stream.add internal_stream (`Typeahead_started (op_id, sw));
          let text =
            Type_ahead_provider.complete_suffix
              ~sw
              ~env
              ~dir:services.cwd
              ~cfg:ctx.submit.streaming.cfg
              ~messages:(Model.messages model)
              ~draft:req.base_input
              ~cursor:req.base_cursor
          in
          Eio.Stream.add
            internal_stream
            (`Typeahead_done
                ( op_id
                , { generation = req.generation
                  ; base_input = req.base_input
                  ; base_cursor = req.base_cursor
                  ; text
                  } ))
        with
        | () -> ()
        | exception Typeahead_cancelled -> ()
        | exception exn -> Eio.Stream.add internal_stream (`Typeahead_error (op_id, exn))))
  in
  let start_typeahead_request (req : typeahead_request) : unit =
    cancel_typeahead_debounce ();
    match runtime.Runtime.typeahead_op with
    | None -> start_typeahead_worker req
    | Some (Runtime.Typeahead { sw; id = _ }) ->
      runtime.Runtime.typeahead_op <- None;
      Switch.fail sw Typeahead_cancelled;
      start_typeahead_worker req
    | Some (Runtime.Starting_typeahead { id = _ }) ->
      runtime.Runtime.cancel_typeahead_on_start <- true;
      typeahead_pending_request := Some req
  in
  let restart_typeahead_debounce (req : typeahead_request) : unit =
    cancel_typeahead_debounce ();
    Fiber.fork ~sw:ui_sw (fun () ->
      match
        Switch.run
        @@ fun sw ->
        typeahead_debounce_sw := Some sw;
        Eio.Time.sleep clock typeahead_debounce_s;
        typeahead_debounce_sw := None;
        start_typeahead_request req
      with
      | () -> ()
      | exception Typeahead_cancelled -> ())
  in
  let start_submit (submit_request : Runtime.submit_request) : unit =
    match Runtime.moderator_startup_state runtime, runtime.Runtime.halted_reason with
    | Runtime.Starting, _ ->
      Queue.enqueue runtime.Runtime.pending (Runtime.Submit submit_request)
    | Runtime.Failed message, _ ->
      add_system_notice
        (Printf.sprintf "Cannot submit: moderator startup failed (%s)." message)
    | Runtime.Ready, None ->
      Runtime.clear_pending_turn_request runtime;
      App_submit.start ctx.submit submit_request
    | Runtime.Ready, Some reason ->
      add_system_notice
        (Printf.sprintf "Cannot submit: session ended by moderator (%s)." reason)
  in
  let start_turn_from_session reason : unit =
    match runtime.Runtime.halted_reason with
    | None -> App_submit.start_from_current_session ctx.submit ~reason
    | Some halted_reason ->
      Runtime.clear_pending_turn_request runtime;
      add_system_notice
        (Printf.sprintf
           "Cannot start follow-up turn: session ended by moderator (%s)."
           halted_reason)
  in
  let maybe_start_pending_turn () : unit =
    if
      (not (Runtime.is_moderator_startup_ready runtime))
      || Runtime.has_active_op runtime
      || Runtime.has_pending_input runtime
    then ()
    else (
      match Runtime.dequeue_pending_turn_request runtime with
      | None -> ()
      | Some reason ->
        if Option.is_none runtime.Runtime.halted_reason
        then start_turn_from_session reason)
  in
  let start_compaction () : unit = App_compaction.start ctx.compaction in
  let maybe_start_next_pending () : unit =
    match Runtime.moderator_startup_state runtime, runtime.Runtime.op with
    | (Runtime.Starting | Failed _), _ -> ()
    | Runtime.Ready, Some _ -> ()
    | Runtime.Ready, None when Runtime.has_pending_input runtime -> ()
    | Runtime.Ready, None ->
      (match Queue.dequeue runtime.Runtime.pending with
       | None -> ()
       | Some (Runtime.Submit submit_request) -> start_submit submit_request
       | Some Runtime.Compact -> start_compaction ());
      maybe_start_pending_turn ()
  in
  let fail_moderator_startup message =
    Runtime.fail_moderator_startup runtime message;
    runtime.Runtime.halted_reason <- Some ("moderator startup failed: " ^ message);
    Queue.clear runtime.Runtime.pending;
    Runtime.clear_pending_turn_request runtime;
    add_system_notice ("Moderator startup failed: " ^ message)
  in
  let complete_moderator_startup outcomes =
    let outcome =
      Moderator_session_controller.of_outcomes
        ~policy:tui_policy
        ~turn_request:(Schedule Runtime.Idle_followup)
        outcomes
    in
    ignore (apply_moderator_outcome outcome : bool);
    Runtime.complete_moderator_startup runtime;
    if Option.is_some runtime.Runtime.halted_reason
    then (
      Queue.clear runtime.Runtime.pending;
      Runtime.clear_pending_turn_request runtime)
    else maybe_start_next_pending ()
  in
  let resume_pending_input (pending_input : Runtime.moderator_approval) ~response =
    match Runtime.resume_pending_input runtime ~response with
    | Error msg ->
      handle_input_resume_error pending_input msg;
      true
    | Ok outcomes ->
      let controller_outcome =
        Moderator_session_controller.of_outcomes
          ~policy:tui_policy
          ~turn_request:(Schedule Runtime.Moderator_request)
          outcomes
      in
      if not (apply_moderator_outcome controller_outcome) then maybe_start_next_pending ();
      true
  in
  let shell_approval_response text =
    let text = String.strip text in
    if String.equal text "once"
    then Ok Shell_runtime.Approval_broker.Approve_once
    else if String.equal text "session"
    then Ok Approve_exact_session
    else if String.equal text "deny"
    then Ok (Deny "denied by user")
    else (
      match String.chop_prefix text ~prefix:"deny " with
      | Some reason when not (String.is_empty (String.strip reason)) ->
        Ok (Deny (String.strip reason))
      | Some _ | None -> Error "Reply with once, session, deny, or deny REASON.")
  in
  let respond_shell_approval request submit_request =
    match shell_approval_response submit_request.Runtime.text with
    | Error message ->
      add_system_notice message;
      Redraw_throttle.request_redraw throttler;
      true
    | Ok response ->
      (match Runtime.respond_shell_approval runtime request response with
       | Error error ->
         add_system_notice
           ("Shell approval response failed: "
            ^ Sexp.to_string_hum ([%sexp_of: Shell_runtime.Approval_broker.error] error))
       | Ok () ->
         ignore (sync_pending_input () : bool);
         Redraw_throttle.request_redraw throttler;
         maybe_start_next_pending ());
      true
  in
  let rec handle_cancel_or_quit () : bool =
    match Runtime.pending_input runtime with
    | Some (Runtime.Shell _) ->
      Runtime.cancel_pending_input runtime;
      ignore (sync_pending_input () : bool);
      Redraw_throttle.request_redraw throttler;
      true
    | Some (Runtime.Moderator _) | None ->
      (match runtime.Runtime.op with
       | Some (Runtime.Streaming { sw; id = _ }) ->
         let cancel () = Switch.fail sw cancelled in
         Cmd.run (Cancel_streaming cancel);
         true
       | Some (Runtime.Starting_streaming { id = _ }) ->
         runtime.Runtime.cancel_streaming_on_start <- true;
         true
       | Some (Runtime.Compacting { sw; id = _ }) ->
         Switch.fail sw Compaction_cancelled;
         true
       | Some (Runtime.Starting_compaction { id = _ }) ->
         runtime.Runtime.cancel_compaction_on_start <- true;
         true
       | None ->
         quit_via_esc := true;
         false)
  and handle_key (ev : input_event) : bool =
    if not (Model.normal_input_is_enabled model)
    then (
      match ev with
      | `Key (`Escape, []) -> handle_cancel_or_quit ()
      | `Key _ | `Mouse _ | `Paste _ -> true)
    else handle_enabled_key ev
  and handle_enabled_key (ev : input_event) : bool =
    let pre_input_line = Model.input_line model in
    let pre_cursor_pos = Model.cursor_pos model in
    let pre_mode = Model.mode model in
    let pre_generation = Model.typeahead_generation model in
    let controller_result =
      if
        Controller.is_ctrl_g ev
        && Poly.(Model.active_page model = Model.Page_id.Chat)
        && List.is_empty (Model.active_agent_calls model)
        && Runtime.toggle_pending_agent runtime
      then Controller.Redraw
      else Controller.handle_key ~model ~term ev
    in
    (match controller_result with
     | Controller.Redraw
     | Controller.Refresh_messages
     | Controller.Submit_input
     | Controller.Cancel_or_quit
     | Controller.Compact_context
     | Controller.Quit
     | Controller.Chat_scrolled _
     | Controller.Prepare_chat_destination _
     | Controller.Shell_approval_response _
     | Controller.Shell_grant_revoke_requested _
     | Controller.Shell_management_refresh_requested _
     | Controller.Moderator_input_response _
     | Controller.Unhandled -> ());
    let controller_actions =
      Controller_actions.
        { model; runtime; internal_stream; throttler; handle_cancel_or_quit }
    in
    let keep_going =
      Controller_actions.handle_controller_result controller_actions ev controller_result
    in
    if keep_going
    then (
      let post_input_line = Model.input_line model in
      let post_cursor_pos = Model.cursor_pos model in
      let post_mode = Model.mode model in
      let post_preview_open = Model.typeahead_preview_open model in
      let post_generation = Model.typeahead_generation model in
      let generation_changed = not (Int.equal pre_generation post_generation) in
      if generation_changed then cancel_running_typeahead ();
      if
        Poly.(Model.active_page model = Model.Page_id.Chat)
        && is_ctrl_space ev
        && Poly.(post_mode = Model.Insert)
        && not (Model.typeahead_is_relevant model)
      then (
        let now_open = if post_preview_open then false else true in
        Model.set_typeahead_preview_open model now_open;
        if now_open
        then (
          Model.set_typeahead_preview_scroll model 0;
          let generation =
            if generation_changed
            then post_generation
            else Model.bump_typeahead_generation model
          in
          start_typeahead_request
            { generation; base_input = post_input_line; base_cursor = post_cursor_pos };
          Redraw_throttle.request_redraw throttler)
        else (
          cancel_running_typeahead ();
          Redraw_throttle.request_redraw throttler))
      else (
        let input_changed = not (String.equal pre_input_line post_input_line) in
        let cursor_changed = not (Int.equal pre_cursor_pos post_cursor_pos) in
        let mode_changed = Poly.(pre_mode <> post_mode) in
        if mode_changed && Poly.(post_mode <> Model.Insert)
        then (
          cancel_typeahead_debounce ();
          cancel_running_typeahead ();
          Model.clear_typeahead model)
        else if Poly.(post_mode = Model.Insert) && (input_changed || cursor_changed)
        then (
          cancel_typeahead_debounce ();
          if cursor_changed && not input_changed
          then (
            let generation =
              if generation_changed
              then post_generation
              else Model.bump_typeahead_generation model
            in
            cancel_running_typeahead ();
            Model.clear_typeahead model;
            if
              (not (Model.typeahead_is_relevant model))
              && not (String.is_empty (String.strip post_input_line))
            then
              restart_typeahead_debounce
                { generation
                ; base_input = post_input_line
                ; base_cursor = post_cursor_pos
                };
            Redraw_throttle.request_redraw throttler)
          else if Model.typeahead_is_relevant model
          then ()
          else if String.is_empty (String.strip post_input_line)
          then ()
          else (
            let generation =
              if generation_changed
              then post_generation
              else Model.bump_typeahead_generation model
            in
            cancel_running_typeahead ();
            restart_typeahead_debounce
              { generation; base_input = post_input_line; base_cursor = post_cursor_pos }))));
    keep_going
  and handle_app_event (ev : app_event) : bool =
    match ev with
    | #Notty.Unescape.event as ev -> handle_key ev
    | `Prepare_chat_destination destination ->
      let viewport_height = chat_viewport_height () in
      let immediate =
        match Model.chat_materialization model, destination with
        | Model.Chat_page_state.Warm, Earlier_conversation ->
          Model.set_auto_follow model false;
          Notty_scroll_box.scroll_to_top (Model.scroll_box model);
          true
        | Warm, Latest_conversation ->
          Model.follow_chat_bottom model ~viewport_height;
          true
        | Warm, Search_result id ->
          Model.request_projected_reveal model ~id;
          true
        | Corridor, _ | (Loading | Resizing), _ -> false
      in
      if immediate
      then Redraw_throttle.request_redraw throttler
      else (
        let row_count = Array.length (Model.render_messages model) in
        let id, reason, placement =
          match destination with
          | Controller_types.Earlier_conversation ->
            ( Option.bind
                (if row_count = 0 then None else Some 0)
                ~f:(fun idx -> Model.render_row_identity model ~idx |> Option.map ~f:fst)
            , Model.Chat_page_state.Destination.Earlier_conversation
            , Model.Chat_page_state.Destination.Top )
          | Search_result id -> Some id, Search_result, Center
          | Latest_conversation ->
            ( Option.bind
                (if row_count = 0 then None else Some (row_count - 1))
                ~f:(fun idx -> Model.render_row_identity model ~idx |> Option.map ~f:fst)
            , Latest_conversation
            , Bottom )
        in
        Option.iter id ~f:(fun id ->
          Runtime.cancel_current_target_width_preparation runtime;
          destination_generation := Int.max !destination_generation !resize_generation + 1;
          let generation = !destination_generation in
          let size = ui.size () in
          let screen_w, screen_h = size in
          let layout = Chat_page_layout.compute ~screen_w ~screen_h ~model in
          let anchor =
            Model.capture_resize_anchor model ~viewport_height:layout.scroll_height
          in
          Model.start_width_preparation
            model
            ~request_generation:generation
            ~terminal_size:size
            ~layout:
              { Model.Chat_page_state.input_box_height = layout.input_box_height
              ; history_height = layout.history_height
              ; sticky_height = layout.sticky_height
              ; scroll_height = layout.scroll_height
              }
            ~theme_generation:0
            ~grammar_generation:(Highlight_registry.generation ())
            ~anchor;
          let revision =
            Model.render_index_by_id model ~id
            |> Option.bind ~f:(fun idx -> Model.message_revision model ~idx)
            |> Option.value_exn
          in
          ignore
            (Model.set_width_preparation_destination
               model
               ~request_generation:generation
               (Some { Model.Chat_page_state.Destination.id; revision; reason; placement })
             : bool);
          Model.set_chat_materialization_resizing model;
          ui.render_current_with_layout ~size ~layout;
          Runtime.submit_destination_target_width_batches runtime;
          Runtime.pump_target_width_completion runtime;
          if Model.publish_width_preparation_corridor model ~request_generation:generation
          then ui.render_current_with_layout ~size ~layout
          else Redraw_throttle.request_redraw throttler));
      true
    | `Resize ->
      Int.incr resize_generation;
      let generation = !resize_generation in
      Live_scroll_trace.emit
        ~phase:"resize_observed"
        [ "generation", `Number (Int.to_string generation) ];
      Fiber.fork ~sw:ui_sw (fun () ->
        Eio.Time.sleep clock resize_settle_delay;
        Eio.Stream.add internal_stream (`Resize_settled generation));
      true
    | `Resize_settled generation ->
      let is_current = Int.equal generation !resize_generation in
      if not is_current
      then (
        Live_scroll_trace.emit
          ~phase:"resize_settled"
          [ "generation", `Number (Int.to_string generation); "current", `String "false" ];
        true)
      else (
        let size = ui.size () in
        let screen_w, screen_h = size in
        let size_changed = not ([%equal: int * int] size !last_terminal_size) in
        Live_scroll_trace.emit
          ~phase:"resize_settled"
          [ "generation", `Number (Int.to_string generation)
          ; "current", `String "true"
          ; "width", `Number (Int.to_string screen_w)
          ; "height", `Number (Int.to_string screen_h)
          ; "size_changed", `String (Bool.to_string size_changed)
          ];
        let startup_width_changed =
          Poly.(Model.chat_materialization model = Model.Chat_page_state.Loading)
          && not
               (Option.equal Int.equal (Model.active_history_width model) (Some screen_w))
        in
        if size_changed || startup_width_changed
        then (
          last_terminal_size := size;
          match Model.chat_materialization model with
          | Loading ->
            if startup_width_changed
            then restart_startup_render ~size
            else redraw_immediate ()
          | Resizing | Corridor ->
            Runtime.cancel_current_target_width_preparation runtime;
            let layout = Chat_page_layout.compute ~screen_w ~screen_h ~model in
            let anchor =
              Model.capture_resize_anchor model ~viewport_height:layout.scroll_height
            in
            Model.start_width_preparation
              model
              ~request_generation:generation
              ~terminal_size:size
              ~layout:
                { Model.Chat_page_state.input_box_height = layout.input_box_height
                ; history_height = layout.history_height
                ; sticky_height = layout.sticky_height
                ; scroll_height = layout.scroll_height
                }
              ~theme_generation:0
              ~grammar_generation:(Highlight_registry.generation ())
              ~anchor;
            Model.clear_corridor_history_cache model;
            Model.set_chat_materialization_resizing model;
            ui.render_current_with_layout ~size ~layout;
            Runtime.submit_initial_target_width_batches runtime;
            Runtime.pump_target_width_completion runtime;
            Redraw_throttle.request_redraw throttler
          | Warm ->
            let layout = Chat_page_layout.compute ~screen_w ~screen_h ~model in
            let anchor =
              Model.capture_resize_anchor model ~viewport_height:layout.scroll_height
            in
            Model.remember_current_width model;
            if Model.restore_width model ~width:screen_w
            then (
              ignore
                (Model.restore_resize_anchor
                   model
                   ~viewport_height:layout.scroll_height
                   anchor
                 : Model.Resize_anchor.resolution);
              ui.render_current_with_layout ~size ~layout)
            else (
              Model.start_width_preparation
                model
                ~request_generation:generation
                ~terminal_size:size
                ~layout:
                  { Model.Chat_page_state.input_box_height = layout.input_box_height
                  ; history_height = layout.history_height
                  ; sticky_height = layout.sticky_height
                  ; scroll_height = layout.scroll_height
                  }
                ~theme_generation:0
                ~grammar_generation:(Highlight_registry.generation ())
                ~anchor;
              Model.set_chat_materialization_resizing model;
              ui.render_current_with_layout ~size ~layout;
              Runtime.submit_initial_target_width_batches runtime;
              Runtime.pump_target_width_completion runtime;
              if
                Model.publish_width_preparation_corridor
                  model
                  ~request_generation:generation
              then ui.render_current_with_layout ~size ~layout
              else Redraw_throttle.request_redraw throttler));
        true)
    | `Redraw ->
      Redraw_throttle.on_redraw_handled throttler;
      if
        (match Model.chat_materialization model with
         | Loading | Resizing -> true
         | Corridor | Warm -> false)
        || (Option.is_some (Model.activity model) && Model.auto_follow model)
      then Model.advance_animation_frame model;
      redraw ();
      if
        match Model.chat_materialization model with
        | Loading | Resizing -> true
        | Corridor | Warm ->
          Option.is_some (Model.activity model) && Model.auto_follow model
      then Redraw_throttle.request_redraw throttler;
      true
    | `Ui_frame_presented (generation, capability) ->
      let is_latest = Int.equal generation (ui.latest_frame_generation ()) in
      let is_current =
        match capability, Model.active_page model with
        | App_events.Disabled, _ -> false
        | App_events.Normal, Model.Page_id.Agent -> true
        | App_events.Normal, Model.Page_id.Shell_security -> true
        | App_events.Normal, Chat ->
          (match Model.chat_materialization model with
           | Model.Chat_page_state.Corridor | Warm -> true
           | Loading | Resizing -> false)
        | App_events.Interaction id, _ ->
          Option.exists (Model.shell_interaction_id model) ~f:(String.equal id)
      in
      if is_latest && is_current && not (Model.normal_input_is_enabled model)
      then (
        Model.set_normal_input_enabled model true;
        Live_scroll_trace.emit ~phase:"input_enabled" []);
      true
    | `Width_rendered result ->
      (match runtime.Runtime.chat_render_worker with
       | None -> ()
       | Some worker ->
         if Chat_render_worker.accepts_result worker result
         then (
           let destination_is_current =
             Option.for_all (Model.width_preparation model) ~f:(fun preparation ->
               Model.width_preparation_destination_is_current model preparation)
           in
           if not destination_is_current
           then (
             Option.iter (Model.width_preparation model) ~f:(fun preparation ->
               ignore
                 (Model.cancel_width_preparation
                    model
                    ~request_generation:
                      (Model.width_preparation_request_generation preparation)
                  : bool));
             Model.set_chat_materialization_corridor model;
             let size = ui.size () in
             let screen_w, screen_h = size in
             let layout = Chat_page_layout.compute ~screen_w ~screen_h ~model in
             ui.render_current_with_layout ~size ~layout)
           else (
             let accepted =
               Model.commit_width_preparation_result
                 model
                 ~theme_generation:0
                 ~grammar_generation:(Highlight_registry.generation ())
                 result
             in
             if accepted
             then (
               Renderer_component_message.install_highlights result.highlights;
               let request_generation = result.request_generation in
               let promoted =
                 Renderer_page_chat.promote_width_preparation
                   ~size:(ui.size ())
                   ~model
                   ~request_generation
               in
               if promoted
               then (
                 let size = ui.size () in
                 let screen_w, screen_h = size in
                 let layout = Chat_page_layout.compute ~screen_w ~screen_h ~model in
                 ui.render_current_with_layout ~size ~layout)
               else if Model.publish_width_preparation_corridor model ~request_generation
               then (
                 let size = ui.size () in
                 let screen_w, screen_h = size in
                 let layout = Chat_page_layout.compute ~screen_w ~screen_h ~model in
                 ui.render_current_with_layout ~size ~layout);
               Runtime.pump_target_width_completion runtime))));
      true
    | `Width_render_failed (job, exn) ->
      (match runtime.Runtime.chat_render_worker with
       | None -> ()
       | Some worker ->
         Chat_render_worker.record_failure worker;
         (match Chat_render_worker.retry_failed worker job with
          | Retried ->
            Live_scroll_trace.emit
              ~phase:"resize_worker_retry"
              [ "request_generation", `Number (Int.to_string job.request_generation)
              ; "row_id", `String (Projected_message.Id.to_string job.key.row_id)
              ]
          | Stale -> ()
          | Exhausted ->
            Live_scroll_trace.emit
              ~phase:"resize_synchronous_fallback"
              [ "reason", `String (Exn.to_string exn)
              ; "request_generation", `Number (Int.to_string job.request_generation)
              ];
            Chat_render_worker.record_synchronous_fallback worker;
            Runtime.cancel_current_target_width_preparation runtime;
            Model.set_chat_materialization_resizing model;
            let size = ui.size () in
            let screen_w, screen_h = size in
            let layout = Chat_page_layout.compute ~screen_w ~screen_h ~model in
            ui.render_current_with_layout ~size ~layout;
            ui.resize_and_redraw ~size ~layout));
      true
    | `Load_discovered_grammars custom_grammars ->
      Renderer_component_message.clear_code_cache ();
      Option.iter runtime.Runtime.chat_render_worker ~f:(fun worker ->
        Chat_render_worker.update_config
          worker
          (Chat_render_worker_runtime.Config.create
             ~custom_grammars
             ~theme_generation:0
             ~grammar_generation:(Highlight_registry.generation ())));
      if Runtime.startup_render_is_warming runtime
      then (
        Runtime.update_startup_render_config
          runtime
          (Chat_render_worker_runtime.Config.create
             ~custom_grammars
             ~theme_generation:0
             ~grammar_generation:1);
        Model.clear_img_caches_preserving_heights model;
        restart_startup_render ~size:(ui.size ()));
      true
    | `Startup_render_finished (generation, outcome) ->
      (match outcome with
       | Chat_startup_render.Completed completion
         when Runtime.startup_render_accepts runtime ~generation completion.snapshot
              && Int.equal completion.snapshot.width (fst !last_terminal_size)
              && List.length completion.jobs = List.length completion.results
              && List.for_all2_exn
                   completion.results
                   completion.jobs
                   ~f:(fun result job ->
                     Chat_message_render_job.result_matches result job) ->
         let publication_size = !last_terminal_size in
         let committed =
           List.for_all completion.results ~f:(fun result ->
             Model.commit_startup_render_result model result)
         in
         if committed && Int.equal completion.snapshot.width (fst publication_size)
         then (
           List.iter completion.results ~f:(fun result ->
             Renderer_component_message.install_highlights result.highlights);
           let started_at = Time_ns.now () in
           let published =
             Renderer_page_chat.publish_startup_history ~size:publication_size ~model
           in
           if published
           then (
             let publication_latency = Time_ns.diff (Time_ns.now ()) started_at in
             Runtime.complete_startup_render runtime;
             Runtime.record_startup_publication runtime ~publication_latency;
             ignore (Eio.Stream.take_nonblocking redraw_stream : unit option);
             redraw_immediate ())
           else restart_startup_render ~size:!last_terminal_size)
         else restart_startup_render ~size:!last_terminal_size
       | Completed _ when Runtime.startup_render_generation_is_current runtime generation
         -> restart_startup_render ~size:!last_terminal_size
       | Failed _ when Runtime.startup_render_generation_is_current runtime generation ->
         Runtime.close_startup_render runtime;
         finish_startup_synchronously ()
       | Cancelled when Runtime.startup_render_generation_is_current runtime generation ->
         restart_startup_render ~size:!last_terminal_size
       | Completed _ | Failed _ | Cancelled -> ());
      true
    | `Moderator_wakeup ->
      Runtime.mark_moderator_dirty runtime;
      ignore (drain_moderator_if_idle () : bool);
      true
    | `Moderator_input_response response ->
      (match Runtime.pending_input runtime with
       | Some (Runtime.Moderator pending_input) ->
         resume_pending_input pending_input ~response
       | Some (Runtime.Shell _) | None -> true)
    | `Shell_approval_changed ->
      let changed = sync_pending_input () in
      if changed then Redraw_throttle.request_redraw throttler;
      true
    | `Shell_management_refresh_requested generation ->
      (match runtime.Runtime.session_state with
       | None ->
         Eio.Stream.add
           internal_stream
           (`Shell_management_loaded
               (generation, "", Error "Audit replay requires an active session."))
       | Some session_state ->
         let session_id = !session_state.Session.id in
         Fiber.fork ~sw:ui_sw (fun () ->
           let result = Shell_management_service.load_audit ~env ~session_id in
           Eio.Stream.add
             internal_stream
             (`Shell_management_loaded (generation, session_id, result))));
      true
    | `Shell_management_loaded (generation, session_id, result) ->
      let session_is_current =
        match runtime.Runtime.session_state with
        | None -> String.is_empty session_id
        | Some state -> String.equal !state.Session.id session_id
      in
      if session_is_current
      then (
        let changed =
          match result with
          | Ok page -> Model.finish_shell_management_load model ~generation page
          | Error message ->
            Model.fail_shell_management_load
              model
              ~generation
              (Util.sanitize ~strip:true message)
        in
        if changed then Redraw_throttle.request_redraw throttler);
      true
    | `Shell_grant_revoke_requested (generation, grant_id) ->
      let shell_snapshot = Model.shell_security_snapshot model in
      let grant =
        List.find shell_snapshot.Model.Shell_security_page_state.grants ~f:(fun grant ->
          String.equal grant.Session.Shell_state.Approval_grant.grant_id grant_id)
      in
      (match runtime.Runtime.approval_store, grant with
       | None, _ | _, None ->
         Model.fail_shell_grant_revoke
           model
           ~generation
           ~grant_id
           "The authoritative approval store is unavailable.";
         Redraw_throttle.request_redraw throttler
       | Some store, Some grant ->
         Fiber.fork ~sw:ui_sw (fun () ->
           let now =
             Eio.Time.now clock |> Time_ns.Span.of_sec |> Time_ns.of_span_since_epoch
           in
           let revoke_error =
             match
               Shell_runtime.Approval_store.revoke
                 store
                 ~now
                 ~grant_id
                 ~reason:(Some "revoked from Shell Security")
             with
             | Ok () -> None
             | Error error -> Some (error.code ^ ": " ^ error.message)
           in
           let audit_sequence, audit_error =
             match revoke_error, runtime.Runtime.session_state with
             | Some _, _ -> None, None
             | None, None ->
               None, Some "Grant was revoked, but no session audit destination exists."
             | None, Some session_state ->
               let session_id = !session_state.Session.id in
               let path =
                 Filename.concat
                   (Session_store.rel_path session_id)
                   ".chatmd/shell-audit.jsonl"
               in
               (match
                  Shell_runtime.Audit_sink.append_management_event
                    ~env
                    ~path
                    ~session_id:(Some session_id)
                    ~runtime_id:grant.runtime_id
                    ~manifest_sha256:grant.manifest_sha256
                    ~request_id:("grant-revoke:" ^ grant_id)
                    ~event:"grant_revoked"
                    ~fields:
                      [ "grant_id", `String grant_id
                      ; ( "scope"
                        , `String
                            (Sexp.to_string_mach
                               (Session.Shell_state.Approval_scope.sexp_of_t grant.scope))
                        )
                      ; "reason", `String "revoked from Shell Security"
                      ]
                with
                | Error error -> None, Some (error.code ^ ": " ^ error.message)
                | Ok sequence ->
                  let shell_state =
                    { !session_state.shell_state with
                      last_audit_sequence = Some sequence
                    }
                  in
                  let updated = { !session_state with shell_state } in
                  (match
                     Or_error.try_with (fun () -> Session_store.save ~env updated)
                   with
                   | Error error ->
                     ( None
                     , Some
                         ("The grant was revoked and audited, but the session sequence \
                           could not be persisted: "
                          ^ Error.to_string_hum error) )
                   | Ok () ->
                     session_state := updated;
                     Some sequence, None))
           in
           let grants, list_error =
             match Shell_runtime.Approval_store.list store with
             | Ok grants -> grants, None
             | Error error ->
               shell_snapshot.grants, Some (error.code ^ ": " ^ error.message)
           in
           let error =
             List.filter_opt [ revoke_error; audit_error; list_error ]
             |> function
             | [] -> None
             | errors -> Some (String.concat ~sep:"; " errors)
           in
           Eio.Stream.add
             internal_stream
             (`Shell_grant_revoke_finished
                 (generation, grant_id, { grants; audit_sequence; error }))));
      true
    | `Shell_grant_revoke_finished (generation, grant_id, outcome) ->
      (match Model.shell_grant_revoke_modal model with
       | Some modal
         when Int.equal modal.generation generation
              && String.equal modal.grant_id grant_id ->
         let snapshot = Model.shell_security_snapshot model in
         Model.set_shell_security_snapshot model { snapshot with grants = outcome.grants };
         (match outcome.error with
          | None -> Model.close_shell_grant_revoke_modal model
          | Some message ->
            Model.fail_shell_grant_revoke
              model
              ~generation
              ~grant_id
              (Util.sanitize ~strip:true message));
         Redraw_throttle.request_redraw throttler
       | None | Some _ -> ());
      true
    | `Moderator_startup_completed result ->
      (match result with
       | Error message -> fail_moderator_startup message
       | Ok outcomes -> complete_moderator_startup outcomes);
      true
    | `Moderator_overlay_changed change ->
      Runtime.note_overlay_revision
        runtime
        (Chat_response.Moderation.Overlay_change.revision change);
      let refreshed =
        Runtime.refresh_pending_overlay ~viewport_height:(chat_viewport_height ()) runtime
      in
      if Option.exists refreshed ~f:Model.projection_damage_requires_redraw
      then Redraw_throttle.request_redraw throttler;
      true
    | `Start_turn reason ->
      let policy = Runtime.runtime_policy runtime in
      (match
         Runtime.decide_automatic_turn
           ~policy
           ~followup_turns_started_since_user_submit:
             runtime.Runtime.session_controller.started_followup_turns_since_user_submit
           ~started_followup_turn_timestamps_ms:
             runtime.Runtime.session_controller.started_followup_turn_timestamps_ms
           ~now_ms:(now_ms ())
           ~reason
       with
       | Runtime.Allow_automatic_turn -> Runtime.request_turn_start runtime reason
       | Runtime.Suppress_automatic_turn { notice_key; notice_text } ->
         add_system_notice_once ~key:notice_key notice_text);
      maybe_start_next_pending ();
      true
    | `Streaming_started (op_id, sw) ->
      (match runtime.Runtime.op with
       | Some (Runtime.Starting_streaming { id }) when Int.equal id op_id ->
         runtime.Runtime.op <- Some (Runtime.Streaming { sw; id });
         if runtime.Runtime.cancel_streaming_on_start
         then (
           runtime.Runtime.cancel_streaming_on_start <- false;
           Switch.fail sw cancelled);
         true
       | _ -> true)
    | `Stream (op_id, ev) ->
      (match runtime.Runtime.op with
       | Some (Runtime.Streaming { id; sw = _ }) when Int.equal id op_id ->
         Stream_apply.apply_stream_event
           runtime
           throttler
           ~viewport_height:(chat_viewport_height ())
           ev;
         true
       | _ -> true)
    | `Stream_batch (op_id, items) ->
      (match runtime.Runtime.op with
       | Some (Runtime.Streaming { id; sw = _ }) when Int.equal id op_id ->
         Stream_apply.apply_stream_batch
           runtime
           throttler
           ~viewport_height:(chat_viewport_height ())
           items;
         true
       | _ -> true)
    | `Sourced_stream (op_id, event) ->
      (match runtime.Runtime.op with
       | Some (Runtime.Streaming { id; sw = _ }) when Int.equal id op_id ->
         Stream_apply.apply_sourced_stream_event
           runtime
           throttler
           ~viewport_height:(chat_viewport_height ())
           event;
         true
       | _ -> true)
    | `Sourced_stream_batch (op_id, events) ->
      (match runtime.Runtime.op with
       | Some (Runtime.Streaming { id; sw = _ }) when Int.equal id op_id ->
         Stream_apply.apply_sourced_stream_batch
           runtime
           throttler
           ~viewport_height:(chat_viewport_height ())
           events;
         true
       | _ -> true)
    | `History_stream (op_id, event) ->
      (match runtime.Runtime.op with
       | Some (Runtime.Streaming { id; sw = _ }) when Int.equal id op_id ->
         Stream_apply.apply_history_stream_event runtime event;
         true
       | _ -> true)
    | `History_stream_batch (op_id, events) ->
      (match runtime.Runtime.op with
       | Some (Runtime.Streaming { id; sw = _ }) when Int.equal id op_id ->
         Stream_apply.apply_history_stream_batch runtime events;
         true
       | _ -> true)
    | `Tool_execution (op_id, event) ->
      (match runtime.Runtime.op with
       | Some (Runtime.Streaming { id; sw = _ }) when Int.equal id op_id ->
         let accepted =
           match event with
           | Chat_response.Tool_execution_event.Started { call_id; name; kind; payload }
             ->
             (match Runtime.agent_page_kind runtime ~name with
              | None -> true
              | Some agent_page_kind ->
                let accepted =
                  Model.agent_call_started
                    model
                    ~call_id
                    ~name
                    ~kind
                    ~payload
                    ~agent_page_kind
                in
                if accepted
                then Model.set_activity model (Some (Model.Assistant Model.Working));
                if accepted && Runtime.consume_pending_agent runtime ~op_id
                then (
                  Controller_normal.cancel_pending ();
                  Model.set_active_page model Model.Page_id.Agent);
                accepted)
           | Progress { call_id; progress } ->
             Model.agent_call_progress model ~call_id progress
           | Trace { call_id; trace } -> Model.agent_call_trace model ~call_id trace
           | Finished { call_id; outcome; output } ->
             let retained = Model.agent_call_finished model ~call_id ~outcome ~output in
             let chat_changed = Model.mark_tool_call_finished model ~call_id ~outcome in
             let has_running_tool =
               Model.active_agent_calls model
               |> List.exists ~f:(fun call ->
                 Option.is_none (Model.agent_call_outcome call))
             in
             if retained && not has_running_tool
             then Model.set_activity model (Some (Model.Assistant Model.Thinking));
             retained || chat_changed
         in
         if accepted
         then Redraw_throttle.request_redraw throttler
         else Log.emit `Debug "Ignored duplicate or unknown tool execution event.";
         true
       | _ -> true)
    | `Tool_output (op_id, item) ->
      (match runtime.Runtime.op with
       | Some (Runtime.Streaming { id; sw = _ }) when Int.equal id op_id ->
         Stream_apply.apply_tool_output runtime throttler item;
         true
       | _ -> true)
    | `Moderator_runtime_request (op_id, request) ->
      (match runtime.Runtime.op with
       | Some (Runtime.Streaming { id; sw = _ }) when Int.equal id op_id ->
         handle_runtime_request request;
         true
       | _ -> true)
    | `Typeahead_started (op_id, sw) ->
      (match runtime.Runtime.typeahead_op with
       | Some (Runtime.Starting_typeahead { id }) when Int.equal id op_id ->
         if runtime.Runtime.cancel_typeahead_on_start
         then (
           runtime.Runtime.cancel_typeahead_on_start <- false;
           runtime.Runtime.typeahead_op <- None;
           Switch.fail sw Typeahead_cancelled;
           match !typeahead_pending_request with
           | None -> ()
           | Some req ->
             typeahead_pending_request := None;
             start_typeahead_request req)
         else runtime.Runtime.typeahead_op <- Some (Runtime.Typeahead { sw; id });
         true
       | _ -> true)
    | `Typeahead_done (op_id, completion) ->
      let is_current =
        match runtime.Runtime.typeahead_op with
        | Some (Runtime.Typeahead { id; sw = _ }) -> Int.equal id op_id
        | Some (Runtime.Starting_typeahead { id }) -> Int.equal id op_id
        | None -> false
      in
      if not is_current
      then true
      else (
        runtime.Runtime.typeahead_op <- None;
        let text = Util.sanitize ~strip:false completion.text in
        let is_still_applicable =
          Int.equal completion.generation (Model.typeahead_generation model)
          && Poly.(Model.mode model = Model.Insert)
          && String.equal completion.base_input (Model.input_line model)
          && Int.equal completion.base_cursor (Model.cursor_pos model)
        in
        if is_still_applicable && not (String.is_empty text)
        then (
          Model.set_typeahead_completion
            model
            (Some
               { text
               ; base_input = completion.base_input
               ; base_cursor = completion.base_cursor
               ; generation = completion.generation
               });
          Redraw_throttle.request_redraw throttler);
        true)
    | `Typeahead_error (op_id, exn) ->
      let is_current =
        match runtime.Runtime.typeahead_op with
        | Some (Runtime.Typeahead { id; sw = _ }) -> Int.equal id op_id
        | Some (Runtime.Starting_typeahead { id }) -> Int.equal id op_id
        | None -> false
      in
      if not is_current
      then true
      else (
        runtime.Runtime.typeahead_op <- None;
        (match exn with
         | Typeahead_cancelled -> ()
         | _ -> Log.emit `Warn (sprintf "Type-ahead error: %s" (Exn.to_string exn)));
        true)
    | `Submit_requested submit_request ->
      (match Runtime.pending_input runtime with
       | Some (Runtime.Moderator pending_input) ->
         resume_pending_input pending_input ~response:submit_request.Runtime.text
       | Some (Runtime.Shell request) -> respond_shell_approval request submit_request
       | None when Runtime.is_moderator_starting runtime ->
         Queue.enqueue runtime.Runtime.pending (Runtime.Submit submit_request);
         true
       | None ->
         (match runtime.Runtime.op with
          | Some (Runtime.Streaming _ | Runtime.Starting_streaming _) ->
            (match Runtime.enqueue_deferred_user_note runtime submit_request with
             | Ok true ->
               Log.emit `Debug "Queued canonical user entry for the next model turn."
             | Ok false -> ()
             | Error message -> Runtime.add_system_notice runtime message);
            Redraw_throttle.request_redraw throttler;
            true
          | Some _ ->
            Queue.enqueue runtime.Runtime.pending (Runtime.Submit submit_request);
            maybe_start_next_pending ();
            true
          | None ->
            start_submit submit_request;
            Redraw_throttle.request_redraw throttler;
            true))
    | `Compact_requested ->
      (match Runtime.moderator_startup_state runtime with
       | Runtime.Starting -> Queue.enqueue runtime.Runtime.pending Runtime.Compact
       | Runtime.Failed message ->
         add_system_notice
           (Printf.sprintf "Cannot compact: moderator startup failed (%s)." message)
       | Runtime.Ready ->
         Queue.enqueue runtime.Runtime.pending Runtime.Compact;
         maybe_start_next_pending ());
      true
    | `Compaction_started (op_id, sw) ->
      (match runtime.Runtime.op with
       | Some (Runtime.Starting_compaction { id }) when Int.equal id op_id ->
         runtime.Runtime.op <- Some (Runtime.Compacting { sw; id });
         if runtime.Runtime.cancel_compaction_on_start
         then (
           runtime.Runtime.cancel_compaction_on_start <- false;
           Switch.fail sw Compaction_cancelled);
         true
       | _ -> true)
    | `Compaction_done (op_id, history') ->
      let is_current =
        match runtime.Runtime.op with
        | Some (Runtime.Compacting { id; sw = _ }) -> Int.equal id op_id
        | Some (Runtime.Starting_compaction { id }) -> Int.equal id op_id
        | _ -> false
      in
      if not is_current
      then true
      else (
        runtime.Runtime.op <- None;
        Model.set_activity model None;
        Runtime.cancel_current_target_width_preparation runtime;
        Model.set_history_items model history';
        ignore (App_runtime.refresh_messages runtime : Model.projection_damage);
        Runtime.note_current_overlay_projected runtime;
        Model.select_projected model None;
        Model.set_auto_follow model true;
        restart_corridor_preparation_after_replacement ();
        Redraw_throttle.request_redraw throttler;
        if not (handle_idle_safe_point ()) then maybe_start_next_pending ();
        true)
    | `Compaction_error (op_id, exn) ->
      let is_current =
        match runtime.Runtime.op with
        | Some (Runtime.Compacting { id; sw = _ }) -> Int.equal id op_id
        | Some (Runtime.Starting_compaction { id }) -> Int.equal id op_id
        | _ -> false
      in
      if not is_current
      then true
      else (
        runtime.Runtime.op <- None;
        Model.set_activity model None;
        ignore (App_runtime.refresh_messages runtime : Model.projection_damage);
        Runtime.note_current_overlay_projected runtime;
        Placeholders.add_placeholder_stream_error
          model
          (if is_compaction_cancelled exn
           then "Compaction cancelled."
           else "Compaction failed.");
        Redraw_throttle.request_redraw throttler;
        if not (handle_idle_safe_point ()) then maybe_start_next_pending ();
        true)
    | `Streaming_done (op_id, items) ->
      let is_current =
        match runtime.Runtime.op with
        | Some (Runtime.Streaming { id; sw = _ }) -> Int.equal id op_id
        | Some (Runtime.Starting_streaming { id }) -> Int.equal id op_id
        | _ -> false
      in
      if not is_current
      then true
      else (
        Runtime.clear_pending_agent runtime ~op_id;
        runtime.Runtime.op <- None;
        Model.set_activity model None;
        runtime.Runtime.cancel_streaming_on_start <- false;
        Runtime.clear_active_turn_start_reason runtime;
        Model.clear_agent_calls model;
        let late_entries =
          Runtime.dequeue_deferred_user_notes runtime
          |> List.map ~f:(fun note -> note.entry)
        in
        let moderation_outcome =
          match runtime.Runtime.moderator, late_entries with
          | _, [] | None, _ -> None
          | Some moderator, entries ->
            (match
               Moderator_session_controller.handle_appended_entries
                 ~moderator
                 ~now_ms:(now_ms ())
                 ~history:items
                 ~entries
                 ~available_tools:ctx.submit.streaming.tools
                 ~turn_request:Moderator_session_controller.Ignore
             with
             | Ok outcome -> Some outcome
             | Error message ->
               add_system_notice
                 (Printf.sprintf "Queued user-message moderation failed: %s" message);
               None)
        in
        let items = items @ late_entries in
        Stream_apply.replace_history runtime redraw_immediate items;
        Runtime.note_current_overlay_projected runtime;
        Option.iter moderation_outcome ~f:(fun outcome ->
          ignore (apply_moderator_outcome outcome : bool));
        let handled_safe_point = handle_idle_safe_point () in
        if not (List.is_empty late_entries)
        then Eio.Stream.add internal_stream (`Start_turn Runtime.User_submit)
        else if not handled_safe_point
        then maybe_start_next_pending ();
        true)
    | `Streaming_error (op_id, exn) ->
      let is_current =
        match runtime.Runtime.op with
        | Some (Runtime.Streaming { id; sw = _ }) -> Int.equal id op_id
        | Some (Runtime.Starting_streaming { id }) -> Int.equal id op_id
        | _ -> false
      in
      if not is_current
      then true
      else (
        Runtime.clear_pending_agent runtime ~op_id;
        runtime.Runtime.op <- None;
        Model.set_activity model None;
        runtime.Runtime.cancel_streaming_on_start <- false;
        Runtime.clear_active_turn_start_reason runtime;
        Model.clear_agent_calls model;
        let error_msg = Printf.sprintf "Error during streaming: %s" (Exn.to_string exn) in
        let old_entries = Model.history_items model in
        let pruned =
          Cancellation_repair.repair
            ~allocator:runtime.history_allocator
            ~error:error_msg
            old_entries
          |> Result.ok_or_failwith
        in
        Runtime.cancel_current_target_width_preparation runtime;
        Model.set_history_items model pruned;
        ignore (App_runtime.refresh_messages runtime : Model.projection_damage);
        Runtime.note_current_overlay_projected runtime;
        Placeholders.add_placeholder_stream_error model error_msg;
        restart_corridor_preparation_after_replacement ();
        Redraw_throttle.request_redraw throttler;
        if not (handle_idle_safe_point ()) then maybe_start_next_pending ();
        true)
  and drain_input_events acc remaining =
    if remaining = 0
    then List.rev acc
    else (
      match Eio.Stream.take_nonblocking input_stream with
      | None -> List.rev acc
      | Some ev -> drain_input_events (ev :: acc) (remaining - 1))
  and main_loop () : unit =
    let startup_event =
      if Model.normal_input_is_enabled model
      then None
      else Eio.Stream.take_nonblocking internal_stream
    in
    match startup_event with
    | Some ev -> if handle_app_event (ev :> app_event) then main_loop () else ()
    | None ->
      let input_batch = drain_input_events [] max_input_drain_per_iteration in
      if not (List.is_empty input_batch)
      then
        Live_scroll_trace.emit
          ~phase:"reducer_input_batch"
          [ "size", `Number (Int.to_string (List.length input_batch))
          ; ( "events"
            , `Array
                (List.map input_batch ~f:(fun event ->
                   `String (Live_scroll_trace.event_name event))) )
          ];
      if not (List.for_all input_batch ~f:(fun ev -> handle_app_event (ev :> app_event)))
      then ()
      else (
        match Eio.Stream.take_nonblocking redraw_stream with
        | Some () -> if handle_app_event `Redraw then main_loop () else ()
        | None ->
          (match Eio.Stream.take_nonblocking internal_stream with
           | Some ev -> if handle_app_event (ev :> app_event) then main_loop () else ()
           | None ->
             if not (Eio.Stream.is_empty input_stream)
             then (
               Fiber.yield ();
               main_loop ())
             else (
               let ready : app_event list =
                 Fiber.n_any
                   [ (fun () -> (Eio.Stream.take input_stream : input_event :> app_event))
                   ; (fun () ->
                       (Eio.Stream.take internal_stream : internal_event :> app_event))
                   ; (fun () ->
                       Eio.Stream.take redraw_stream;
                       `Redraw)
                   ]
               in
               if List.for_all ready ~f:handle_app_event then main_loop () else ())))
  in
  main_loop ();
  !quit_via_esc
;;
