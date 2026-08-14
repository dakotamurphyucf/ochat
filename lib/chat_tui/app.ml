(** Terminal chat application – event-loop, streaming, export, and persistence.

    {!Chat_tui.App} glues together the building blocks of the terminal UI and
    runs the main event-loop.

    Responsibilities:
    {ul
    {- run a full-screen {!Notty_eio.Term} and render frames via {!Chat_tui.Renderer}}
    {- interpret keystrokes via {!Chat_tui.Controller}}
    {- maintain a mutable {!Chat_tui.Model.t} (editor state, scroll position, caches)}
    {- stream assistant replies and tool calls via
       {!Chat_response.In_memory_stream.run_completion_stream_in_memory_entries}}
    {- perform user-triggered history compaction via
       {!Context_compaction.Compactor.compact_entries}}
    {- export the conversation as ChatMarkdown and optionally persist a session
       snapshot on exit}}

    The main entry point is {!run_chat}.  The remaining values are exposed to
    support white-box tests of the event-loop.
*)

open Core
open Eio.Std
open Types
module Renderer = Renderer
module Redraw_throttle = Redraw_throttle
module Stream_handler = Stream
module CM = Prompt.Chat_markdown
module Scroll_box = Notty_scroll_box
module Res = Openai.Responses
module Res_stream = Res.Response_stream
module Res_item = Res.Item
module Converter = Chat_response.Converter
module Ctx = Chat_response.Ctx
module Cache = Chat_response.Cache
module Agent_runtime = Chat_response.Agent_runtime
module Config = Chat_response.Config
module Moderation = Chat_response.Moderation
module Moderator_manager = Chat_response.Moderator_manager
module Stream_moderator = Chat_response.In_memory_stream
module Req = Res.Request
module Runtime_semantics = Chat_response.Runtime_semantics
module Moderator_session_controller = Moderator_session_controller
module Chatml_builtin_spec = Chatml.Chatml_builtin_spec
module Chatml_builtin_surface = Chatml.Chatml_builtin_surface
module Chatml_debug_log = Chatml.Chatml_debug_log
module Chatml_runtime = Chatml_moderator_runtime

(** Runtime artefacts derived from the chat prompt. *)
type prompt_context =
  { cfg : Config.t (** Behavioural settings such as temperature, model, … *)
  ; tools : Req.Tool.t list (** Tools exposed to the assistant at runtime. *)
  ; tool_tbl : (string, Ochat_function.runner) Hashtbl.t
    (** Mapping [tool_name -> implementation].

        The assistant returns a JSON payload that is looked up in this table and
        then executed. *)
  ; moderator : Stream_moderator.moderator option
  }

type input_event = App_events.input_event

type input_capability = App_events.input_capability =
  | Disabled
  | Normal
  | Interaction of string

module Runtime = App_runtime

type internal_event = App_events.internal_event

module Session_persist = struct
  (** [persist_snapshot env session model] copies the live [model] back into
       [session] and persists it to disk.

       The helper updates the canonical history, task list and key/value store
       fields of the supplied {!Session.t} and then delegates the actual
       serialisation to {!Session_store.save}.  It is used from all quit
       branches so that conversation state is not lost even when the user
       skips ChatMarkdown export. *)
  let persist_snapshot env session runtime =
    let session =
      Option.first_some
        (Option.map runtime.Runtime.session_state ~f:(fun state -> !state))
        session
    in
    match session with
    | None -> ()
    | Some (s : Session.t) ->
      let moderator_snapshot =
        match Runtime.moderator_snapshot runtime with
        | Ok moderator_snapshot -> moderator_snapshot
        | Error msg ->
          Log.emit `Error (Printf.sprintf "Failed to snapshot moderator state: %s" msg);
          s.moderator_state.legacy_snapshot
      in
      let updated_session =
        let history = Model.history_items runtime.Runtime.model in
        let next_history_sequence =
          History_entry.Allocator.next_sequence runtime.Runtime.history_allocator
        in
        Session.
          { s with
            history
          ; next_history_sequence
          ; tasks = Model.tasks runtime.Runtime.model
          ; moderator_state =
              { s.moderator_state with legacy_snapshot = moderator_snapshot }
          ; kv_store = Hashtbl.to_alist (Model.kv_store runtime.Runtime.model)
          }
      in
      Session_store.save ~env updated_session
  ;;
end

module Setup = struct
  let now_ms ~env = Eio.Time.now (Eio.Stdenv.clock env) *. 1000. |> Int.of_float

  let init_datadir ~env ~cwd ~session : _ Eio.Path.t =
    let open Eio.Path in
    match session with
    | Some (s : Session.t) ->
      let session_dir = Session_store.path ~env s.id in
      let chatmd_dir = session_dir / ".chatmd" in
      (match is_directory chatmd_dir with
       | true -> ()
       | false -> mkdirs ~perm:0o700 chatmd_dir);
      chatmd_dir
    | None -> Io.ensure_chatmd_dir ~cwd
  ;;

  let load_cache ~datadir =
    let cache_file = Eio.Path.(datadir / "cache.bin") in
    Cache.load ~file:cache_file ~max_size:1000 ()
  ;;

  let resolve_prompt_dir ~env ~cwd ~prompt_file : _ Eio.Path.t =
    let dirname = Filename.dirname prompt_file in
    if Filename.is_relative dirname
    then Eio.Path.(cwd / dirname)
    else Eio.Path.(Eio.Stdenv.fs env / dirname)
  ;;

  let load_prompt_xml ~env ~prompt_file =
    match Io.load_doc ~dir:(Eio.Stdenv.fs env) prompt_file with
    | s -> s
    | exception e ->
      raise
        (Failure
           (Printf.sprintf
              "Failed to load prompt file %s: %s"
              prompt_file
              (Exn.to_string e)))
  ;;

  let parse_prompt_elements ~source ~dir ~prompt_xml =
    CM.parse_chat_inputs ~source ~dir prompt_xml
  ;;

  let cfg_of_elements prompt_elements = Config.of_elements prompt_elements

  let build_ctx ~env ~prompt_dir ~tool_dir ~cache =
    Ctx.create ~env ~dir:prompt_dir ~tool_dir ~cache
  ;;

  let agent_runtime_host ~ctx ~response_dir ~session_id ~prompt_elements =
    Agent_runtime.host
      ~env:(Ctx.env ctx)
      ~workspace:(Ctx.tool_dir ctx)
      ~tool_dir:(Ctx.tool_dir ctx)
      ~prompt_dir:(Ctx.dir ctx)
      ~session_dir:response_dir
      ~cache_dir:response_dir
      ~home:(Agent_runtime.default_home (Ctx.env ctx))
      ~session_id
      ~resource_runner:(Sys.getenv "OCHAT_SHELL_RESOURCE_RUNNER")
      ~prompt_elements
    |> Result.map_error ~f:(fun diagnostics ->
      List.map diagnostics ~f:Agent_runtime.diagnostic_to_string
      |> String.concat ~sep:"\n")
    |> Result.ok_or_failwith
  ;;

  let build_agent_runtime
        ~sw
        ~ctx
        ~response_dir
        ~session_id
        ~prompt_elements
        ~manifest_authorizer
        ~approval_provider
        ~approval_store
        ~extension_snapshots
        ?persist_extension_snapshots
        ()
    =
    let host = agent_runtime_host ~ctx ~response_dir ~session_id ~prompt_elements in
    Agent_runtime.create
      ~sw
      ~ctx
      ~host
      ~platform:(Agent_runtime.platform ())
      ~prompt_elements
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~extension_snapshots
      ?persist_extension_snapshots
      ~run_agent:(fun ?prompt_dir ?session_id ?observer ~source ~ctx prompt items ->
        Chat_response.Driver.run_agent
          ~history_compaction:false
          ?prompt_dir
          ?session_id
          ?observer
          ~source
          ~response_dir
          ~shell_manifest_authorizer:manifest_authorizer
          ~shell_approval_provider:approval_provider
          ~ctx
          prompt
          items)
      ()
    |> Result.map_error ~f:(fun diagnostics ->
      List.map diagnostics ~f:Agent_runtime.diagnostic_to_string
      |> String.concat ~sep:"\n")
    |> Result.ok_or_failwith
  ;;

  let history_items_from_prompt ~ctx ~response_dir ~prompt_elements =
    Converter.to_items
      ~ctx
      ~run_agent:(fun ?prompt_dir ?session_id ~ctx prompt items ->
        Chat_response.Driver.run_agent
          ~history_compaction:false
          ?prompt_dir
          ?session_id
          ~response_dir
          ~ctx
          prompt
          items)
      prompt_elements
  ;;

  let choose_initial_history ~session ~history_items_prompt =
    match session with
    | Some s when not (List.is_empty (s : Session.t).history) -> (s : Session.t).history
    | _ -> history_items_prompt
  ;;

  let initial_msg_count ~history_items_prompt = List.length history_items_prompt

  let moderator_session_id ~session ~prompt_file =
    match session with
    | Some (session : Session.t) -> session.id
    | None -> prompt_file
  ;;

  let create_moderator
        ?(capabilities = Moderation.Capabilities.default)
        ?(runtime_policy = Chat_response.Runtime_semantics.default_policy)
        ?on_wakeup
        ?on_process_run
        ~model_executor
        ~env
        ~prompt_file
        ~session
        ~allocator
        ~prompt_elements
        ~history_entries
        ~tools
        ()
    =
    let open Result.Let_syntax in
    let%bind _, artifact =
      Moderator_manager.Registry.of_elements
        ~surface:Chatml_builtin_surface.ui_moderator_surface
        Moderator_manager.Registry.empty
        prompt_elements
    in
    match artifact with
    | None -> Ok (None, None)
    | Some artifact ->
      let legacy_snapshot =
        Option.bind session ~f:(fun (session : Session.t) ->
          session.moderator_state.legacy_snapshot)
      in
      let identity_snapshot =
        Option.bind session ~f:(fun (session : Session.t) ->
          session.moderator_state.identity_snapshot)
      in
      let session_id = moderator_session_id ~session ~prompt_file in
      let capabilities =
        { capabilities with
          model_recipes =
            Map.of_alist_exn
              (module String)
              [ ( Chat_response.Model_executor.agent_prompt_v1_name
                , Chat_response.Model_executor.recipe_agent_prompt_v1
                    model_executor
                    ~session_id )
              ]
        }
      in
      let%bind manager =
        Moderator_manager.create_entries
          ~artifact
          ~capabilities
          ~allocator
          ?on_process_run
          ?snapshot:identity_snapshot
          ()
      in
      Chat_response.Model_executor.register_session
        ?on_wakeup
        model_executor
        ~session_id
        ~manager;
      let moderator =
        Stream_moderator.{ manager; session_id; session_meta = `Null; runtime_policy }
      in
      let startup_event =
        match identity_snapshot, legacy_snapshot with
        | Some _, _ | None, Some _ -> Moderation.Event.Session_resume
        | None, None -> Session_start
      in
      Ok (Some moderator, Some startup_event)
  ;;

  let run_moderator_startup ~env ~moderator ~history_entries ~tools ~event =
    let open Result.Let_syntax in
    let%bind outcome =
      Moderator_manager.handle_event_entries
        moderator.Stream_moderator.manager
        ~session_id:moderator.session_id
        ~now_ms:(now_ms ~env)
        ~history:history_entries
        ~available_tools:tools
        ~session_meta:moderator.session_meta
        ~event
    in
    if
      Option.is_some
        (Chat_response.Runtime_semantics.should_end_session outcome.runtime_requests)
    then Ok [ outcome ]
    else
      Moderator_manager.drain_internal_events_entries
        moderator.manager
        ~session_id:moderator.session_id
        ~now_ms:(now_ms ~env)
        ~history:history_entries
        ~available_tools:tools
        ~session_meta:moderator.session_meta
      |> Result.map ~f:(fun drained -> outcome :: drained)
  ;;

  let init_model ~(session : Session.t option) ~history_items : Model.t =
    Model.create
      ~history_items
      ~messages:[]
      ~input_line:""
      ~auto_follow:true
      ~msg_buffers:(Hashtbl.create (module String))
      ~function_name_by_id:(Hashtbl.create (module String))
      ~reasoning_idx_by_id:(Hashtbl.create (module String))
      ~tool_output_by_index:(Hashtbl.create (module Int))
      ~tasks:
        (match session with
         | Some s -> (s : Session.t).tasks
         | None -> [])
      ~kv_store:
        (let tbl = Hashtbl.create (module String) in
         (match session with
          | Some s ->
            List.iter (s : Session.t).kv_store ~f:(fun (k, v) ->
              Hashtbl.set tbl ~key:k ~data:v)
          | None -> ());
         tbl)
      ~fetch_sw:None
      ~scroll_box:(Scroll_box.create Notty.I.empty)
      ~cursor_pos:0
      ~selection_anchor:None
      ~mode:Insert
      ~draft_mode:Plain
      ~selected_msg:None
      ~undo_stack:[]
      ~redo_stack:[]
      ~cmdline:""
      ~cmdline_cursor:0
  ;;
end

module Ui = struct
  type frame_origin =
    | Normal
    | Resize

  type frame =
    { size : int * int
    ; visible_fingerprint : string
    ; cursor : (int * int) option
    }

  type pending_frame =
    { generation : int
    ; frame : frame
    ; image : Notty.I.t
    ; input_capability : input_capability
    ; origin : frame_origin
    }

  type presenter =
    { changed : Eio.Condition.t
    ; mutable latest : pending_frame option
    ; mutable previous : frame option
    ; mutable next_generation : int
    ; mutable latest_generation : int
    }

  let frame_equal left right =
    [%equal: int * int] left.size right.size
    && String.equal left.visible_fingerprint right.visible_fingerprint
    && [%equal: (int * int) option] left.cursor right.cursor
  ;;

  let create_presenter () =
    { changed = Eio.Condition.create ()
    ; latest = None
    ; previous = None
    ; next_generation = 0
    ; latest_generation = 0
    }
  ;;

  let cursor_for_frame ~model cursor =
    match Model.shell_interaction_id model, Model.active_page model with
    | Some _, _ when Model.shell_interaction_uses_cursor model -> Some cursor
    | Some _, _ -> None
    | None, (Model.Page_id.Agent | Shell_security) -> None
    | None, Chat ->
      (match Model.chat_materialization model with
       | Model.Chat_page_state.Loading | Resizing -> None
       | Corridor | Warm -> Some cursor)
  ;;

  let input_capability_for_frame ~model : input_capability =
    match Model.shell_interaction_id model, Model.active_page model with
    | Some id, _ -> App_events.Interaction id
    | None, (Model.Page_id.Agent | Shell_security) -> App_events.Normal
    | None, Chat ->
      (match Model.chat_materialization model with
       | Model.Chat_page_state.Loading | Resizing -> App_events.Disabled
       | Corridor | Warm -> App_events.Normal)
  ;;

  let submit_frame presenter ~origin ~size ~image ~cursor ~model =
    let visible_fingerprint = Renderer.visible_fingerprint ~size image in
    let cursor = cursor_for_frame ~model cursor in
    let frame = { size; visible_fingerprint; cursor } in
    presenter.next_generation <- presenter.next_generation + 1;
    let generation = presenter.next_generation in
    presenter.latest_generation <- generation;
    let input_capability = input_capability_for_frame ~model in
    let origin =
      match origin, presenter.latest with
      | Resize, _ | Normal, Some { origin = Resize; _ } -> Resize
      | Normal, None | Normal, Some { origin = Normal; _ } -> Normal
    in
    (match origin with
     | Normal -> ()
     | Resize ->
       Live_scroll_trace.emit
         ~phase:"resize_frame_submitted"
         [ "width", `Number (Int.to_string (fst size))
         ; "height", `Number (Int.to_string (snd size))
         ]);
    presenter.latest <- Some { generation; frame; image; input_capability; origin };
    Eio.Condition.broadcast presenter.changed
  ;;

  let spawn_presenter ~sw ~term ~input_stream ~on_presented presenter =
    Fiber.fork_daemon ~sw (fun () ->
      let rec discard_startup_input () =
        match Eio.Stream.take_nonblocking input_stream with
        | None -> ()
        | Some (`Key (`Escape, [])) -> Eio.Stream.add input_stream (`Key (`Escape, []))
        | Some (`Key _ | `Mouse _ | `Paste _) -> discard_startup_input ()
      in
      let rec loop () =
        match presenter.latest with
        | None ->
          Eio.Condition.await_no_mutex presenter.changed;
          loop ()
        | Some pending ->
          presenter.latest <- None;
          if
            (match pending.input_capability with
             | Disabled -> false
             | Normal | Interaction _ -> true)
            || not (Option.exists presenter.previous ~f:(frame_equal pending.frame))
          then (
            (match pending.origin with
             | Normal -> ()
             | Resize ->
               Live_scroll_trace.emit
                 ~phase:"resize_frame_present_begin"
                 [ "width", `Number (Int.to_string (fst pending.frame.size))
                 ; "height", `Number (Int.to_string (snd pending.frame.size))
                 ]);
            Live_scroll_trace.emit
              ~phase:"present_begin"
              [ ( "input_capability"
                , `String
                    (Sexp.to_string
                       (App_events.sexp_of_input_capability pending.input_capability)) )
              ];
            Notty_eio.Term.present term ~image:pending.image ~cursor:pending.frame.cursor;
            Live_scroll_trace.emit
              ~phase:"present_end"
              [ ( "input_capability"
                , `String
                    (Sexp.to_string
                       (App_events.sexp_of_input_capability pending.input_capability)) )
              ];
            (match pending.origin with
             | Normal -> ()
             | Resize ->
               Live_scroll_trace.emit
                 ~phase:"resize_frame_present_end"
                 [ "width", `Number (Int.to_string (fst pending.frame.size))
                 ; "height", `Number (Int.to_string (snd pending.frame.size))
                 ]);
            presenter.previous <- Some pending.frame;
            (match pending.input_capability with
             | Normal -> discard_startup_input ()
             | Disabled | Interaction _ -> ());
            on_presented pending.generation pending.input_capability);
          loop ()
      in
      loop ())
  ;;

  let latest_generation presenter = presenter.latest_generation

  let should_warm_history_before_redraw ~runtime ~model =
    let is_writing =
      match Model.activity model with
      | Some (Model.Assistant Model.Writing) -> true
      | None | Some (Assistant (Thinking | Working) | Compacting) -> false
    in
    Poly.(Model.active_page model = Model.Page_id.Chat)
    && Poly.(Model.chat_materialization model = Model.Chat_page_state.Warm)
    && not (Runtime.startup_render_is_warming runtime || is_writing)
  ;;

  let make_redraws ~term ~presenter ~runtime ~model =
    let redraw () =
      let size = Notty_eio.Term.size term in
      if should_warm_history_before_redraw ~runtime ~model
      then Renderer_page_chat.warm_history_synchronously ~size ~model;
      let image, cursor = Renderer.render_full ~size ~model in
      submit_frame presenter ~origin:Normal ~size ~image ~cursor ~model
    in
    let render_current_with_layout ~size ~layout =
      let image, cursor =
        match Model.active_page model with
        | Model.Page_id.Chat ->
          let image, cursor =
            Renderer_page_chat.render_with_layout ~size ~layout ~model
          in
          Renderer.decorate ~size ~model image, cursor
        | Agent | Shell_security -> Renderer.render_full ~size ~model
      in
      submit_frame presenter ~origin:Resize ~size ~image ~cursor ~model
    in
    let resize_and_redraw ~size:((width, _) as size) ~layout =
      Renderer_page_chat.relayout_history_with_layout_synchronously ~width ~layout ~model;
      Renderer_page_chat.warm_history_synchronously ~size ~model;
      render_current_with_layout ~size ~layout
    in
    redraw, resize_and_redraw, render_current_with_layout
  ;;

  let read_fps_env () =
    match Sys.getenv "OCHAT_TUI_FPS" with
    | Some s ->
      (try Float.max 1. (Float.of_string s) with
       | _ -> 30.)
    | None -> 30.
  ;;

  let init_throttler ~fps ~enqueue_redraw = Redraw_throttle.create ~fps ~enqueue_redraw

  let spawn_throttler ~env ~sw ~throttler =
    Redraw_throttle.spawn throttler ~sw ~sleep:(fun dt ->
      Eio.Time.sleep (Eio.Stdenv.clock env) dt)
  ;;

  let spawn_startup_animator ~env ~sw ~fps ~model ~redraw_stream =
    Fiber.fork_daemon ~sw (fun () ->
      let rec loop () =
        Eio.Time.sleep (Eio.Stdenv.clock env) (1. /. fps);
        match Model.chat_materialization model with
        | Model.Chat_page_state.Corridor | Warm -> `Stop_daemon
        | Loading | Resizing ->
          Eio.Stream.add redraw_stream ();
          loop ()
      in
      loop ())
  ;;
end

module For_testing = struct
  let should_warm_history_before_redraw = Ui.should_warm_history_before_redraw
  let cursor_for_frame = Ui.cursor_for_frame
end

(* ────────────────────────────────────────────────────────────────────────── *)
(*  Main event handler for submitting the draft to the assistant             *)
(* ────────────────────────────────────────────────────────────────────────── *)

module Streaming_submit = App_streaming

module Shutdown = struct
  let shutdown
        ~env
        ~term
        ~quit_via_esc
        ~prompt_file
        ~export_file
        ~persist_mode
        ~session
        ~runtime
        ~model
        ~cfg
        ~initial_msg_count
        ()
    =
    (* Helper: export conversation to ChatMarkdown at [target_path]. *)
    let do_export ~target_path () =
      let moderator_snapshot =
        match App_runtime.moderator_snapshot runtime with
        | Ok moderator_snapshot -> moderator_snapshot
        | Error msg ->
          Log.emit
            `Error
            (Printf.sprintf "Failed to snapshot moderator state for export: %s" msg);
          Option.bind session ~f:(fun (session : Session.t) ->
            session.moderator_state.legacy_snapshot)
      in
      Export.archive
        ~env
        ~model
        ~prompt_file
        ~target_path
        ~initial_msg_count
        ~moderator_snapshot
        ~session
    in
    (* ------------------------------------------------------------------ *)
    (* Shutdown & persistence                                              *)
    (* ------------------------------------------------------------------ *)

    (* Decide whether to export the conversation as ChatMarkdown.  When the
        user quits via ESC we prompt for confirmation; otherwise we keep the
        previous automatic export behaviour.  Snapshot persistence happens
        independently via [persist_snapshot]. *)
    let export_session ~target_path () =
      let cmd : Types.cmd = Persist_session (fun () -> do_export ~target_path ()) in
      Cmd.run cmd
    in
    (* Release the Notty terminal before printing any messages to stdout so
        that they appear correctly in the user’s shell.  We do this once and
        only once – further calls are benign but avoided for clarity. *)
    Notty_eio.Term.release term;
    (match quit_via_esc with
     | false ->
       (* Quit triggered via other means (Ctrl-C / q) – export automatically. *)
       let target = Option.value export_file ~default:prompt_file in
       export_session ~target_path:target ()
     | true ->
       Out_channel.(output_string stdout "Export conversation to promptmd file? [y/N] ");
       Out_channel.flush stdout;
       (match In_channel.input_line In_channel.stdin with
        | Some ans
          when List.mem
                 [ "y"; "yes" ]
                 (String.lowercase (String.strip ans))
                 ~equal:String.equal ->
          (* Determine target path *)
          let target_path =
            match export_file with
            | Some p -> p
            | None ->
              (* Prompt for filename *)
              Out_channel.output_string
                stdout
                (Printf.sprintf "Enter output file path [default: %s]: " prompt_file);
              Out_channel.flush stdout;
              (match In_channel.input_line In_channel.stdin with
               | Some line when not (String.is_empty (String.strip line)) ->
                 String.strip line
               | _ -> prompt_file)
          in
          export_session ~target_path ()
        | _ -> ()));
    (* Decide whether to persist the snapshot. *)
    let should_persist =
      match persist_mode with
      | `Always -> true
      | `Never -> false
      | `Ask ->
        (* Ask the user – default to yes. *)
        (match session with
         | None -> false
         | Some _ ->
           Out_channel.output_string stdout "Save session snapshot? [Y/n] ";
           Out_channel.flush stdout;
           (match In_channel.input_line In_channel.stdin with
            | Some ans ->
              let ans = String.lowercase (String.strip ans) in
              not (List.mem [ "n"; "no" ] ans ~equal:String.equal)
            | None -> true))
    in
    if should_persist
    then Session_persist.persist_snapshot env session runtime
    else Log.emit `Info "Skipping session persistence as per user request."
  ;;
end

let fetch_prompt ~ctx ~prompt ~is_local =
  try
    let xml = Chat_response.Fetch.get ~ctx prompt ~is_local in
    let prompt_dir =
      if is_local then Chat_response.Fetch.resolve_local_dir ~ctx prompt else None
    in
    Ok (xml, prompt_dir)
  with
  | exn -> Error (Exn.to_string exn)
;;

(* ------------------------------------------------------------------------ *)
(*  Main entry-point                                                         *)
(* ------------------------------------------------------------------------ *)

(** [run_chat ~env ~prompt_file ()] is the {b only} public entry-point of
    the module.  Call it from your executable to start an interactive chat
    session.  The function never returns – it blocks until the user quits
    the TUI.

    Parameters:
    {ul
    {- [env] – the standard environment passed by {!Eio_main.run}.}
    {- [prompt_file] – path to a *.chatmd* document that seeds the history,
       declares tools and provides default settings.}
    {- [textmate_grammar_files] – explicit custom TextMate grammar files loaded
       before terminal initialization.}}

    Typical usage:
    {[
      let () =
        Eio_main.run @@ fun env ->
        Chat_tui.App.run_chat ~env ~prompt_file:"prompt.chatmd" ()
    ]} *)
type persist_mode =
  [ `Always
  | `Never
  | `Ask
  ]

let startup_timing_enabled () =
  Sys.getenv "OCHAT_TUI_STARTUP_TIMING"
  |> Option.exists ~f:(fun value ->
    (not (String.is_empty value)) && not (String.equal value "0"))
;;

let render_metrics_enabled () =
  Sys.getenv "OCHAT_TUI_RENDER_METRICS"
  |> Option.exists ~f:(fun value ->
    (not (String.is_empty value)) && not (String.equal value "0"))
;;

let time_startup_phase label f =
  if not (startup_timing_enabled ())
  then f ()
  else (
    let started_at = Time_ns.now () in
    let result = f () in
    let elapsed = Time_ns.diff (Time_ns.now ()) started_at in
    eprintf "[chat-tui startup] %s: %.3f ms\n%!" label (Time_ns.Span.to_ms elapsed);
    result)
;;

let run_chat
      ~env
      ~prompt_file
      ?session
      ?export_file
      ?(persist_mode = `Ask)
      ?(parallel_tool_calls = true)
      ?(textmate_grammar_files = [])
      ?(shell_manifest_authorizer = Shell_runtime.Manifest_authorizer.deny)
      ?shell_approval_provider
      ()
  =
  let fs = Eio.Stdenv.fs env in
  let cwd = Eio.Stdenv.cwd env in
  let grammar_registry = time_startup_phase "bundled grammars" Highlight_registry.get in
  time_startup_phase "explicit grammars" (fun () ->
    Highlight_grammar_discovery.load_explicit
      ~fs
      ~cwd
      ~registry:grammar_registry
      textmate_grammar_files)
  |> Or_error.ok_exn;
  Switch.run
  @@ fun ui_sw ->
  (* Two event queues: terminal input events must not be backpressured by internal traffic. *)
  let input_stream : input_event Eio.Stream.t = Eio.Stream.create 4096 in
  let internal_stream : internal_event Eio.Stream.t = Eio.Stream.create 1024 in
  let redraw_stream = Eio.Stream.create 1 in
  let internal_shell_approval_broker =
    Shell_runtime.Approval_broker.create
      ~on_pending:(fun _ -> Eio.Stream.add internal_stream `Shell_approval_changed)
      ()
  in
  let shell_approval_provider, shell_approval_broker =
    match shell_approval_provider with
    | None ->
      ( Shell_runtime.Approval_broker.Callback internal_shell_approval_broker
      , Some internal_shell_approval_broker )
    | Some (Shell_runtime.Approval_broker.Callback broker as provider) ->
      provider, Some broker
    | Some
        ((Shell_runtime.Approval_broker.None_available | Auto_deny | Assume_approved) as
         provider) -> provider, None
  in
  let streams : App_context.Streams.t =
    { input = input_stream; internal = internal_stream; redraw = redraw_stream }
  in
  (* Load the chat prompt and initialise the model. *)
  (* Determine the directory used to store runtime artefacts (cache,
     tool outputs, etc.).  When running inside a session we place the
     hidden [.chatmd] folder {i inside} the session directory so that
     each session has an isolated cache.  Falling back to the process
     [cwd] preserves the previous behaviour for ad-hoc one-off chats
     where no session is active. *)
  let datadir = Setup.init_datadir ~env ~cwd ~session in
  let session_state =
    Option.map session ~f:(fun session ->
      match Shell_runtime.Interrupted_store.refresh ~env ~session with
      | Error message ->
        eprintf "[shell security] interrupted-request refresh failed: %s\n%!" message;
        ref session
      | Ok refreshed ->
        if
          not
            (Sexp.equal
               (Session.Shell_state.sexp_of_t session.shell_state)
               (Session.Shell_state.sexp_of_t refreshed.shell_state))
        then Session_store.save ~env refreshed;
        ref refreshed)
  in
  let approval_store =
    let bindings =
      Shell_runtime.Approval_store.
        { user_id = Sys.getenv "USER"; host_id = Some (Core_unix.gethostname ()) }
    in
    match session_state with
    | None -> Shell_runtime.Approval_store.memory ~bindings ()
    | Some session ->
      Shell_runtime.Approval_store.session ~session ~bindings ~persist:(fun updated ->
        Or_error.try_with (fun () -> Session_store.save ~env updated)
        |> Result.map_error ~f:Error.to_string_hum)
  in
  let executor_approval_store =
    Shell_runtime.Approval_store.executor_store approval_store
  in
  Live_scroll_trace.install ~datadir;
  let chatml_log_file = "chatml-runtime.log" in
  let strip_leading_space text =
    match String.chop_prefix text ~prefix:" " with
    | Some stripped -> stripped
    | None -> text
  in
  let classify_chatml_log_line line =
    let parse_levelled prefix component =
      match String.chop_prefix line ~prefix:(prefix ^ "[") with
      | None -> None
      | Some rest ->
        (match String.lsplit2 rest ~on:']' with
         | None -> None
         | Some (level, message) ->
           Some (component, Some level, strip_leading_space message))
    in
    match String.chop_prefix line ~prefix:"[chat_tui] " with
    | Some message -> "chat_tui", None, message
    | None ->
      (match parse_levelled "[script-log]" "script_log" with
       | Some parsed -> parsed
       | None ->
         (match parse_levelled "[chatml-log]" "chatml_log" with
          | Some parsed -> parsed
          | None ->
            (match String.chop_prefix line ~prefix:"[chatml-runtime] " with
             | Some message -> "chatml_runtime", None, message
             | None ->
               (match String.chop_prefix line ~prefix:"[moderator-manager] " with
                | Some message -> "moderator_manager", None, message
                | None ->
                  (match String.chop_prefix line ~prefix:"[print] " with
                   | Some message -> "print", None, message
                   | None -> "chatml", None, line)))))
  in
  let append_chatml_log line =
    let timestamp = Time_ns.to_string_utc (Time_ns.now ()) in
    let component, level, message = classify_chatml_log_line line in
    let fields =
      [ Some ("timestamp", `String timestamp)
      ; Some ("component", `String component)
      ; Some ("message", `String message)
      ; Some ("raw", `String line)
      ; Option.map level ~f:(fun level -> "level", `String level)
      ]
      |> List.filter_map ~f:Fn.id
    in
    Io.log ~dir:datadir ~file:chatml_log_file (Jsonaf.to_string (`Object fields) ^ "\n")
  in
  Chatml_builtin_spec.set_print_sink (fun text ->
    append_chatml_log (Printf.sprintf "[print] %s" text));
  Chatml_debug_log.set_sink append_chatml_log;
  append_chatml_log
    (Printf.sprintf
       "[chat_tui] runtime_log_started prompt=%s session=%s"
       prompt_file
       (Option.value_map session ~default:"<none>" ~f:(fun (s : Session.t) -> s.id)));
  let cache = Setup.load_cache ~datadir in
  let services : App_context.Services.t = { env; ui_sw; cwd; cache; datadir; session } in
  (* Base directory of the prompt file – used for resolving relative paths in
     <import/> and <doc src="…"> tags. *)
  let prompt_dir = Setup.resolve_prompt_dir ~env ~cwd ~prompt_file in
  (* Load the prompt file and parse it into a list of elements. *)
  let prompt_xml = Setup.load_prompt_xml ~env ~prompt_file in
  let prompt_elements =
    Setup.parse_prompt_elements ~source:prompt_file ~dir:prompt_dir ~prompt_xml
  in
  let shell_manifest_authorizer =
    match session_state with
    | None -> shell_manifest_authorizer
    | Some session ->
      Shell_runtime.Manifest_grant_store.session_authorizer
        ~session
        ~persist:(fun updated ->
          Or_error.try_with (fun () -> Session_store.save ~env updated)
          |> Result.map_error ~f:Error.to_string_hum)
        ~source:
          { canonical_source_root = Eio.Path.native_exn prompt_dir
          ; source_sha256 = Digestif.SHA256.(to_hex (digest_string prompt_xml))
          ; repository_identity = Sys.getenv "OCHAT_SHELL_REPOSITORY_IDENTITY"
          }
        ~bindings:
          { user_id = Sys.getenv "USER"; host_id = Some (Core_unix.gethostname ()) }
        ~fallback:shell_manifest_authorizer
  in
  let cfg = Setup.cfg_of_elements prompt_elements in
  let ctx = Setup.build_ctx ~env ~prompt_dir ~tool_dir:cwd ~cache in
  let moderator_session_id = Setup.moderator_session_id ~session ~prompt_file in
  let exec_context : Chat_response.Model_executor.exec_context =
    { ctx
    ; run_agent =
        (fun ?history_compaction ?prompt_dir ?session_id ~ctx prompt items ->
          Chat_response.Driver.run_agent
            ?history_compaction
            ?prompt_dir
            ?session_id
            ~response_dir:datadir
            ~shell_manifest_authorizer
            ~shell_approval_provider
            ~ctx
            prompt
            items)
    ; fetch_prompt
    }
  in
  let model_executor = Chat_response.Model_executor.create ~sw:ui_sw ~exec_context () in
  let wakeup_is_active = ref true in
  let on_moderator_wakeup () =
    if !wakeup_is_active then Eio.Stream.add internal_stream `Moderator_wakeup
  in
  let moderator_capabilities =
    { Moderation.Capabilities.default with
      on_log =
        (fun ~level ~message ->
          Chatml_debug_log.emitf
            "[script-log][%s] %s"
            (Chatml_runtime.string_of_log_level level)
            message;
          Ok ())
    }
  in
  let agent_runtime =
    let extension_snapshots =
      Option.value_map session_state ~default:[] ~f:(fun state ->
        !state.Session.shell_state.extension_snapshots)
    in
    let persist_extension_snapshots =
      Option.map session_state ~f:(fun state extension_snapshots ->
        let shell_state = { !state.Session.shell_state with extension_snapshots } in
        let updated = { !state with shell_state } in
        Or_error.try_with (fun () -> Session_store.save ~env updated)
        |> Result.map_error ~f:Error.to_string_hum
        |> Result.map ~f:(fun () -> state := updated))
    in
    Setup.build_agent_runtime
      ~sw:ui_sw
      ~ctx
      ~response_dir:datadir
      ~session_id:moderator_session_id
      ~prompt_elements
      ~manifest_authorizer:shell_manifest_authorizer
      ~approval_provider:shell_approval_provider
      ~approval_store:executor_approval_store
      ~extension_snapshots
      ?persist_extension_snapshots
      ()
  in
  let agent_page_classifications = agent_runtime.classifications in
  let comp_tools, tool_tbl = Ochat_function.functions agent_runtime.functions in
  let tools = Chat_response.Tool.convert_tools comp_tools in
  (* Convert prompt → initial history items extracted from the static prompt. *)
  let history_items_prompt =
    Setup.history_items_from_prompt ~ctx ~response_dir:datadir ~prompt_elements
  in
  let history_allocator =
    match session with
    | Some session -> Session.allocator session |> Result.ok_or_failwith
    | None ->
      History_entry.Allocator.create ~namespace:prompt_file ~next_sequence:0
      |> Result.ok_or_failwith
  in
  let history_items_prompt =
    List.map history_items_prompt ~f:(History_entry.create ~allocator:history_allocator)
    |> Result.all
    |> Result.ok_or_failwith
  in
  (* If a persisted [session] was passed in, prefer its history – otherwise
     start from the prompt defaults. *)
  let history_items = Setup.choose_initial_history ~session ~history_items_prompt in
  let moderator, moderator_startup_event =
    let on_process_run = Agent_runtime.moderator_process_handler agent_runtime in
    Setup.create_moderator
      ~capabilities:moderator_capabilities
      ~model_executor
      ~env
      ~prompt_file
      ~session
      ~allocator:history_allocator
      ~prompt_elements
      ~history_entries:history_items
      ~tools
      ~on_wakeup:on_moderator_wakeup
      ?on_process_run
      ()
    |> Result.ok_or_failwith
  in
  let moderator_change_subscription = ref None in
  let subscription =
    Option.map moderator ~f:(fun moderator ->
      Chat_response.Moderator_manager.subscribe_committed_changes
        moderator.manager
        ~on_wakeup:(fun () ->
          if !wakeup_is_active
          then (
            let changes =
              Chat_response.Moderator_manager.drain_committed_changes
                (Option.value_exn !moderator_change_subscription)
            in
            List.iter changes ~f:(fun change ->
              Eio.Stream.add internal_stream (`Moderator_overlay_changed change)))))
  in
  moderator_change_subscription := subscription;
  let prompt_ctx : prompt_context = { cfg; tools; tool_tbl; moderator } in
  (* Number of history items contributed by the static prompt.  This is
     forwarded to export logic. *)
  let initial_msg_count = Setup.initial_msg_count ~history_items_prompt in
  (* Load persisted draft, if exists, now that [cursor_pos] is available *)
  let model = Setup.init_model ~session ~history_items in
  Model.set_shell_security_snapshot
    model
    (Shell_security_snapshot.create
       ~agent_runtime
       ~session:(Option.map session_state ~f:(fun state -> !state)));
  let explicit_grammar_sources =
    Highlight_grammar_discovery.load_explicit_sources ~fs ~cwd textmate_grammar_files
    |> Or_error.ok_exn
  in
  let startup_render_config =
    Chat_render_worker_runtime.Config.create
      ~custom_grammars:explicit_grammar_sources
      ~theme_generation:0
      ~grammar_generation:0
  in
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let chat_render_worker =
    Chat_render_worker.create
      ~sw:ui_sw
      ~domain_mgr
      ~config:startup_render_config
      ~worker_count:2
      ~queue_capacity:64
      ~code_cache_capacity:128
      ~on_result:(fun result -> Eio.Stream.add internal_stream (`Width_rendered result))
      ~on_error:(fun job exn ->
        Eio.Stream.add internal_stream (`Width_render_failed (job, exn)))
      ~now:Mtime_clock.now
      ()
  in
  let runtime =
    let moderator_startup_state =
      Option.value_map moderator_startup_event ~default:Runtime.Ready ~f:(fun _ ->
        Runtime.Starting)
    in
    Runtime.create
      ?moderator
      ?shell_approval_broker
      ~approval_store
      ?session_state
      ~moderator_startup_state
      ~chat_render_worker
      ~history_allocator
      ~agent_page_classifications
      ~model
      ()
  in
  time_startup_phase "transcript projection" (fun () ->
    ignore (App_runtime.refresh_messages runtime : Model.projection_damage));
  Runtime.arm_startup_render
    runtime
    ~domain_mgr
    ~config:startup_render_config
    ~code_cache_capacity:128;
  (* Start the Notty terminal – its [on_event] callback just pushes events
        into [ev_stream] so the UI stays single-threaded. *)
  Notty_eio.Term.run ~input:env#stdin ~output:env#stdout ~mouse:false ~on_event:(fun ev ->
    match ev with
    | #Notty.Unescape.event as key_ev ->
      Live_scroll_trace.emit
        ~phase:"terminal_decoded"
        [ "event", `String (Live_scroll_trace.event_name key_ev) ];
      Eio.Stream.add input_stream key_ev
    | `Resize -> Eio.Stream.add internal_stream `Resize)
  @@ fun term ->
  let presenter = Ui.create_presenter () in
  Ui.spawn_presenter
    ~sw:ui_sw
    ~term
    ~input_stream
    ~on_presented:(fun generation capability ->
      Eio.Stream.add internal_stream (`Ui_frame_presented (generation, capability)))
    presenter;
  let redraw, resize_and_redraw, render_current_with_layout =
    Ui.make_redraws ~term ~presenter ~runtime ~model
  in
  let fps = Ui.read_fps_env () in
  let throttler =
    Ui.init_throttler ~fps ~enqueue_redraw:(fun () -> Eio.Stream.add redraw_stream ())
  in
  let redraw_immediate () = Redraw_throttle.redraw_immediate throttler ~draw:redraw in
  if Runtime.startup_render_is_warming runtime
  then Renderer_page_chat.prepare_startup_history ~size:(Notty_eio.Term.size term) ~model;
  time_startup_phase "first render" redraw;
  (* Start the periodic scheduler to coalesce frequent updates. *)
  Ui.spawn_throttler ~env ~sw:ui_sw ~throttler;
  Ui.spawn_startup_animator ~env ~sw:ui_sw ~fps ~model ~redraw_stream;
  Fiber.fork ~sw:ui_sw (fun () ->
    let loaded =
      time_startup_phase "discovered grammars" (fun () ->
        Highlight_grammar_discovery.read_discovered_sources
          ~fs
          ~cwd
          ~warn:(fun message -> eprintf "[chat-tui] warning: %s\n%!" message)
          ())
    in
    let discovered, warnings, registry =
      Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
        let discovered, warnings =
          Highlight_grammar_discovery.parse_discovered_sources loaded
        in
        let registry =
          Highlight_registry.create_with_sources (explicit_grammar_sources @ discovered)
        in
        discovered, warnings, registry)
    in
    List.iter warnings ~f:(fun message -> eprintf "[chat-tui] warning: %s\n%!" message);
    (match registry with
     | Ok registry -> Highlight_registry.replace registry
     | Error error ->
       eprintf
         "[chat-tui] warning: Failed to prepare discovered TextMate grammars: %s\n%!"
         (Error.to_string_hum error));
    Eio.Stream.add
      internal_stream
      (`Load_discovered_grammars (explicit_grammar_sources @ discovered)));
  let ui : App_context.Ui.t =
    { term
    ; size = (fun () -> Notty_eio.Term.size term)
    ; throttler
    ; redraw
    ; redraw_immediate
    ; latest_frame_generation = (fun () -> Ui.latest_generation presenter)
    ; resize_and_redraw
    ; render_current_with_layout
    }
  in
  let shared : App_context.Resources.t = { services; streams; ui } in
  let streaming : Streaming_submit.Context.t =
    { shared
    ; allocator = runtime.Runtime.history_allocator
    ; cfg = prompt_ctx.cfg
    ; tools = prompt_ctx.tools
    ; tool_tbl = prompt_ctx.tool_tbl
    ; moderator = prompt_ctx.moderator
    ; safe_point_input = Some (Runtime.safe_point_input_source runtime)
    ; parallel_tool_calls
    ; history_compaction = false
    }
  in
  let start_streaming ~history ~op_id =
    Fiber.fork ~sw:ui_sw (fun () -> App_streaming.start streaming ~history ~op_id)
  in
  let submit : App_submit.Context.t = { runtime; streaming; start_streaming } in
  let compaction : App_compaction.Context.t = { shared; runtime } in
  let reducer_ctx : App_reducer.Context.t =
    { runtime; shared; submit; compaction; cancelled = Streaming_submit.Cancelled }
  in
  (match moderator, moderator_startup_event with
   | Some moderator, Some event ->
     Fiber.fork ~sw:ui_sw (fun () ->
       let result =
         Setup.run_moderator_startup
           ~env
           ~moderator
           ~history_entries:history_items
           ~tools
           ~event
       in
       Eio.Stream.add internal_stream (`Moderator_startup_completed result))
   | None, None -> ()
   | Some _, None | None, Some _ -> assert false);
  let unregister_moderator_wakeup () =
    wakeup_is_active := false;
    Option.iter moderator ~f:(fun _ ->
      Chat_response.Model_executor.unregister_session_wakeup
        model_executor
        ~session_id:moderator_session_id)
  in
  let quit_via_esc =
    Fun.protect
      ~finally:(fun () ->
        Runtime.cancel_current_target_width_preparation runtime;
        Chat_render_worker.close chat_render_worker;
        Runtime.close_startup_render runtime;
        if render_metrics_enabled ()
        then
          Option.iter (Runtime.startup_render_metrics runtime) ~f:(fun metrics ->
            eprintf "[chat-tui render] %s\n%!" (Jsonaf.to_string metrics));
        Option.iter
          !moderator_change_subscription
          ~f:Chat_response.Moderator_manager.unsubscribe;
        Option.iter shell_approval_broker ~f:Shell_runtime.Approval_broker.close;
        unregister_moderator_wakeup ())
      (fun () -> App_reducer.run reducer_ctx)
  in
  Shutdown.shutdown
    ~env
    ~term
    ~quit_via_esc
    ~prompt_file
    ~export_file
    ~persist_mode
    ~session
    ~runtime
    ~model
    ~cfg
    ~initial_msg_count
    ();
  append_chatml_log "[chat_tui] runtime_log_finished";
  Live_scroll_trace.flush ();
  Chatml_builtin_spec.clear_print_sink ();
  Chatml_debug_log.clear_sink ()
;;
(* Exit the program. *)
