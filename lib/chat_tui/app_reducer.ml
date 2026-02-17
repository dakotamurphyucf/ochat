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

module Placeholders = struct
  let add_placeholder_stream_error (model : Model.t) text : unit =
    let patch = Add_placeholder_message { role = "error"; text } in
    ignore (Model.apply_patch model patch)
  ;;
end

module Stream_apply = App_stream_apply

module Controller_actions = struct
  type t =
    { model : Model.t
    ; internal_stream : internal_event Eio.Stream.t
    ; throttler : Redraw_throttle.t
    ; handle_cancel_or_quit : unit -> bool
    }

  let handle_controller_result (t : t) (ev : input_event) controller_result =
    match controller_result with
    | Controller.Redraw ->
      Redraw_throttle.request_redraw t.throttler;
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
  let system_event = streams.system in
  let throttler = ui.throttler in
  let redraw_immediate = ui.redraw_immediate in
  let redraw = ui.redraw in
  let cancelled = ctx.cancelled in
  let model = runtime.Runtime.model in
  let quit_via_esc = runtime.Runtime.quit_via_esc in
  let max_input_drain_per_iteration = 64 in
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
              ~history_items:(Model.history_items model)
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
    App_submit.start ctx.submit submit_request
  in
  let start_compaction () : unit = App_compaction.start ctx.compaction in
  let maybe_start_next_pending () : unit =
    match runtime.Runtime.op with
    | Some _ -> ()
    | None ->
      (match Queue.dequeue runtime.Runtime.pending with
       | None -> ()
       | Some (Runtime.Submit submit_request) -> start_submit submit_request
       | Some Runtime.Compact -> start_compaction ())
  in
  let rec handle_cancel_or_quit () : bool =
    match runtime.Runtime.op with
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
      false
  and handle_key (ev : input_event) : bool =
    let pre_input_line = Model.input_line model in
    let pre_cursor_pos = Model.cursor_pos model in
    let pre_mode = Model.mode model in
    let pre_generation = Model.typeahead_generation model in
    let controller_result = Controller.handle_key ~model ~term ev in
    let controller_actions =
      Controller_actions.{ model; internal_stream; throttler; handle_cancel_or_quit }
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
        is_ctrl_space ev
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
    | `Resize ->
      redraw_immediate ();
      true
    | `Redraw ->
      Redraw_throttle.on_redraw_handled throttler;
      redraw ();
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
         Stream_apply.apply_stream_event model throttler ev;
         true
       | _ -> true)
    | `Stream_batch (op_id, items) ->
      (match runtime.Runtime.op with
       | Some (Runtime.Streaming { id; sw = _ }) when Int.equal id op_id ->
         Stream_apply.apply_stream_batch model throttler items;
         true
       | _ -> true)
    | `Tool_output (op_id, item) ->
      (match runtime.Runtime.op with
       | Some (Runtime.Streaming { id; sw = _ }) when Int.equal id op_id ->
         Stream_apply.apply_tool_output model throttler item;
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
      (match runtime.Runtime.op with
       | Some (Runtime.Streaming _ | Runtime.Starting_streaming _) ->
         let user_msg = String.strip submit_request.Runtime.text in
         let msg = sprintf "This is a Note From the User:\n%s" user_msg in
         Eio.Stream.add system_event msg;
         Redraw_throttle.request_redraw throttler;
         true
       | Some _ ->
         Queue.enqueue runtime.Runtime.pending (Runtime.Submit submit_request);
         maybe_start_next_pending ();
         true
       | None ->
         start_submit submit_request;
         true)
    | `Compact_requested ->
      Queue.enqueue runtime.Runtime.pending Runtime.Compact;
      maybe_start_next_pending ();
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
        Model.set_history_items model history';
        Model.set_messages model (Conversation.of_history history');
        Model.rebuild_tool_output_index model;
        Model.select_message model None;
        Model.set_auto_follow model true;
        Redraw_throttle.request_redraw throttler;
        maybe_start_next_pending ();
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
        (match exn with
         | Compaction_cancelled ->
           Placeholders.add_placeholder_stream_error model "Compaction cancelled."
         | _ -> Placeholders.add_placeholder_stream_error model "Compaction failed.");
        Redraw_throttle.request_redraw throttler;
        maybe_start_next_pending ();
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
        runtime.Runtime.op <- None;
        Stream_apply.replace_history model redraw_immediate items;
        maybe_start_next_pending ();
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
        runtime.Runtime.op <- None;
        (match Model.fork_start_index model with
         | Some idx ->
           let hist_prefix = List.take (Model.history_items model) idx in
           Model.set_history_items model hist_prefix;
           Model.set_messages model (Conversation.of_history hist_prefix);
           Model.set_active_fork model None;
           Model.set_fork_start_index model None
         | None -> ());
        let error_msg = Printf.sprintf "Error during streaming: %s" (Exn.to_string exn) in
        let prune_trailing ~error history =
          let module Item = Openai.Responses.Item in
          let module Fco = Openai.Responses.Function_call_output in
          let module Cco = Openai.Responses.Custom_tool_call_output in
          let seen_outputs = Hash_set.create (module String) in
          let truncate s ~max_len =
            if String.length s <= max_len then s else String.prefix s max_len ^ "…"
          in
          let synthetic_output_for_call (fc : Openai.Responses.Function_call.t) : Item.t =
            let args_preview =
              Util.sanitize ~strip:true fc.arguments |> truncate ~max_len:200
            in
            let output =
              Printf.sprintf
                "Tool call did not complete (call_id=%s, name=%s, arguments=%s).\n\n%s"
                fc.call_id
                fc.name
                args_preview
                error
            in
            Item.Function_call_output
              { Fco.output = Openai.Responses.Tool_output.Output.Text output
              ; call_id = fc.call_id
              ; _type = "function_call_output"
              ; id = None
              ; status = None
              }
          in
          let synthetic_output_for_custom_tool_call
                (ct : Openai.Responses.Custom_tool_call.t)
            : Item.t
            =
            let input_preview =
              Util.sanitize ~strip:true ct.input |> truncate ~max_len:200
            in
            let output =
              Printf.sprintf
                "Tool call did not complete (call_id=%s, name=%s, input=%s).\n\n%s"
                ct.call_id
                ct.name
                input_preview
                error
            in
            Item.Custom_tool_call_output
              { Cco.output = Openai.Responses.Tool_output.Output.Text output
              ; call_id = ct.call_id
              ; _type = "custom_tool_call_output"
              ; id = None
              }
          in
          let rec loop rev_items acc ~dropping_trailing_reasoning =
            match rev_items with
            | [] -> acc
            | item :: rest ->
              if dropping_trailing_reasoning
              then (
                match item with
                | Item.Reasoning _ -> loop rest acc ~dropping_trailing_reasoning:true
                | _ -> loop rev_items acc ~dropping_trailing_reasoning:false)
              else (
                match item with
                | Item.Function_call_output fco ->
                  if Hash_set.mem seen_outputs fco.call_id
                  then loop rest acc ~dropping_trailing_reasoning:false
                  else (
                    Hash_set.add seen_outputs fco.call_id;
                    loop rest (item :: acc) ~dropping_trailing_reasoning:false)
                | Item.Custom_tool_call_output cco ->
                  if Hash_set.mem seen_outputs cco.call_id
                  then loop rest acc ~dropping_trailing_reasoning:false
                  else (
                    Hash_set.add seen_outputs cco.call_id;
                    loop rest (item :: acc) ~dropping_trailing_reasoning:false)
                | Item.Function_call fc ->
                  if Hash_set.mem seen_outputs fc.call_id
                  then loop rest (item :: acc) ~dropping_trailing_reasoning:false
                  else (
                    Hash_set.add seen_outputs fc.call_id;
                    let out = synthetic_output_for_call fc in
                    loop
                      rest
                      (Item.Function_call fc :: out :: acc)
                      ~dropping_trailing_reasoning:false)
                | Item.Custom_tool_call ct ->
                  if Hash_set.mem seen_outputs ct.call_id
                  then loop rest (item :: acc) ~dropping_trailing_reasoning:false
                  else (
                    Hash_set.add seen_outputs ct.call_id;
                    let out = synthetic_output_for_custom_tool_call ct in
                    loop
                      rest
                      (Item.Custom_tool_call ct :: out :: acc)
                      ~dropping_trailing_reasoning:false)
                | _ -> loop rest (item :: acc) ~dropping_trailing_reasoning:false)
          in
          loop (List.rev history) [] ~dropping_trailing_reasoning:true
        in
        let pruned = prune_trailing ~error:error_msg (Model.history_items model) in
        Model.set_history_items model pruned;
        Model.set_messages model (Conversation.of_history pruned);
        Model.rebuild_tool_output_index model;
        Model.clear_all_img_caches model;
        Placeholders.add_placeholder_stream_error model error_msg;
        Redraw_throttle.request_redraw throttler;
        maybe_start_next_pending ();
        true)
  and drain_input_events acc remaining =
    if remaining = 0
    then List.rev acc
    else (
      match Eio.Stream.take_nonblocking input_stream with
      | None -> List.rev acc
      | Some ev -> drain_input_events (ev :: acc) (remaining - 1))
  and main_loop () : unit =
    let input_batch = drain_input_events [] max_input_drain_per_iteration in
    if not (List.for_all input_batch ~f:(fun ev -> handle_app_event (ev :> app_event)))
    then ()
    else (
      match Eio.Stream.take_nonblocking internal_stream with
      | Some ev -> if handle_app_event (ev :> app_event) then main_loop () else ()
      | None ->
        if not (List.is_empty input_batch)
        then main_loop ()
        else (
          let ready : app_event list =
            Fiber.n_any
              [ (fun () -> (Eio.Stream.take input_stream : input_event :> app_event))
              ; (fun () ->
                  (Eio.Stream.take internal_stream : internal_event :> app_event))
              ]
          in
          let inputs, internals =
            List.partition_tf ready ~f:(function
              | #Notty.Unescape.event -> true
              | _ -> false)
          in
          if List.for_all (inputs @ internals) ~f:handle_app_event
          then main_loop ()
          else ()))
  in
  main_loop ();
  !quit_via_esc
;;
