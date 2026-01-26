(** Terminal chat application – event-loop, streaming, export, and persistence.

    {!Chat_tui.App} glues together the building blocks of the terminal UI and
    runs the main event-loop.

    Responsibilities:
    {ul
    {- run a full-screen {!Notty_eio.Term} and render frames via {!Chat_tui.Renderer}}
    {- interpret keystrokes via {!Chat_tui.Controller}}
    {- maintain a mutable {!Chat_tui.Model.t} (editor state, scroll position, caches)}
    {- stream assistant replies and tool calls via
       {!Chat_response.Driver.run_completion_stream_in_memory_v1}}
    {- perform user-triggered history compaction via
       {!Context_compaction.Compactor.compact_history}}
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
module Tool = Chat_response.Tool
module Config = Chat_response.Config
module Req = Res.Request

(** Runtime artefacts derived from the chat prompt. *)
type prompt_context =
  { cfg : Config.t (** Behavioural settings such as temperature, model, … *)
  ; tools : Req.Tool.t list (** Tools exposed to the assistant at runtime. *)
  ; tool_tbl : (string, string -> Openai.Responses.Tool_output.Output.t) Hashtbl.t
    (** Mapping [tool_name -> implementation].

        The assistant returns a JSON payload that is looked up in this table and
        then executed. *)
  }

type input_event = App_events.input_event

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
  let persist_snapshot env session model =
    match session with
    | None -> ()
    | Some s ->
      let updated_session =
        Session.
          { s with
            history = Model.history_items model
          ; tasks = Model.tasks model
          ; kv_store = Hashtbl.to_alist (Model.kv_store model)
          }
      in
      Session_store.save ~env updated_session
  ;;
end

module Setup = struct
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

  let parse_prompt_elements ~dir ~prompt_xml = CM.parse_chat_inputs ~dir prompt_xml
  let cfg_of_elements prompt_elements = Config.of_elements prompt_elements

  let declared_tools_of_elements prompt_elements =
    List.filter_map prompt_elements ~f:(function
      | CM.Tool t -> Some t
      | _ -> None)
  ;;

  let build_ctx ~env ~prompt_dir ~tool_dir ~cache =
    Ctx.create ~env ~dir:prompt_dir ~tool_dir ~cache
  ;;

  let build_tools_runtime ~sw ~ctx ~declared_tools =
    (* Tools should execute relative to user’s current working directory *)
    let user_fns =
      List.concat_map declared_tools ~f:(fun decl ->
        Tool.of_declaration
          ~sw
          ~ctx
          ~run_agent:(Chat_response.Driver.run_agent ~history_compaction:true)
          decl)
    in
    let comp_tools, tbl = Ochat_function.functions user_fns in
    Tool.convert_tools comp_tools, tbl
  ;;

  let history_items_from_prompt ~ctx ~prompt_elements =
    Converter.to_items
      ~ctx
      ~run_agent:(Chat_response.Driver.run_agent ~history_compaction:true)
      prompt_elements
  ;;

  let choose_initial_history ~session ~history_items_prompt =
    match session with
    | Some s when not (List.is_empty (s : Session.t).history) -> (s : Session.t).history
    | _ -> history_items_prompt
  ;;

  let initial_messages_of_history history_items =
    let initial = Conversation.of_history history_items in
    initial
  ;;

  let initial_msg_count ~history_items_prompt = List.length history_items_prompt

  let init_model ~(session : Session.t option) ~history_items ~messages : Model.t =
    Model.create
      ~history_items
      ~messages
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
  let make_redraw ~term ~model () =
    let size = Notty_eio.Term.size term in
    let img, (cx, cy) = Renderer.render_full ~size ~model in
    Notty_eio.Term.image term img;
    Notty_eio.Term.cursor term (Some (cx, cy))
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
        ~model
        ~cfg
        ~initial_msg_count
        ()
    =
    (* Helper: export conversation to ChatMarkdown at [target_path]. *)
    let do_export ~target_path () =
      Export.archive
        ~env
        ~model
        ~prompt_file
        ~target_path
        ~cfg
        ~initial_msg_count
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
    then Session_persist.persist_snapshot env session model
    else Log.emit `Info "Skipping session persistence as per user request."
  ;;
end

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
       declares tools and provides default settings.}}

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

let run_chat
      ~env
      ~prompt_file
      ?session
      ?export_file
      ?(persist_mode = `Ask)
      ?(parallel_tool_calls = true)
      ()
  =
  Switch.run
  @@ fun ui_sw ->
  (* Two event queues: terminal input events must not be backpressured by internal traffic. *)
  let input_stream : input_event Eio.Stream.t = Eio.Stream.create 4096 in
  let internal_stream : internal_event Eio.Stream.t = Eio.Stream.create 1024 in
  let system_event = Eio.Stream.create 10 in
  let streams : App_context.Streams.t =
    { input = input_stream; internal = internal_stream; system = system_event }
  in
  (* Load the chat prompt and initialise the model. *)
  let cwd = Eio.Stdenv.cwd env in
  (* Determine the directory used to store runtime artefacts (cache,
     tool outputs, etc.).  When running inside a session we place the
     hidden [.chatmd] folder {i inside} the session directory so that
     each session has an isolated cache.  Falling back to the process
     [cwd] preserves the previous behaviour for ad-hoc one-off chats
     where no session is active. *)
  let datadir = Setup.init_datadir ~env ~cwd ~session in
  let cache = Setup.load_cache ~datadir in
  let services : App_context.Services.t = { env; ui_sw; cwd; cache; datadir; session } in
  (* Base directory of the prompt file – used for resolving relative paths in
     <import/> and <doc src="…"> tags. *)
  let prompt_dir = Setup.resolve_prompt_dir ~env ~cwd ~prompt_file in
  (* Load the prompt file and parse it into a list of elements. *)
  let prompt_xml = Setup.load_prompt_xml ~env ~prompt_file in
  let prompt_elements = Setup.parse_prompt_elements ~dir:prompt_dir ~prompt_xml in
  let cfg = Setup.cfg_of_elements prompt_elements in
  let ctx = Setup.build_ctx ~env ~prompt_dir ~tool_dir:cwd ~cache in
  let declared_tools = Setup.declared_tools_of_elements prompt_elements in
  let tools, tool_tbl = Setup.build_tools_runtime ~sw:ui_sw ~ctx ~declared_tools in
  let prompt_ctx : prompt_context = { cfg; tools; tool_tbl } in
  (* Convert prompt → initial history items extracted from the static prompt. *)
  let history_items_prompt = Setup.history_items_from_prompt ~ctx ~prompt_elements in
  (* If a persisted [session] was passed in, prefer its history – otherwise
     start from the prompt defaults. *)
  let history_items = Setup.choose_initial_history ~session ~history_items_prompt in
  let messages = Setup.initial_messages_of_history history_items in
  (* Number of history items contributed by the static prompt.  This is
     forwarded to export logic. *)
  let initial_msg_count = Setup.initial_msg_count ~history_items_prompt in
  (* Load persisted draft, if exists, now that [cursor_pos] is available *)
  let model = Setup.init_model ~session ~history_items ~messages in
  let runtime = Runtime.create ~model in
  Model.rebuild_tool_output_index model;
  (* Start the Notty terminal – its [on_event] callback just pushes events
        into [ev_stream] so the UI stays single-threaded. *)
  Notty_eio.Term.run ~input:env#stdin ~output:env#stdout ~mouse:false ~on_event:(fun ev ->
    match ev with
    | #Notty.Unescape.event as key_ev -> Eio.Stream.add input_stream key_ev
    | `Resize -> Eio.Stream.add internal_stream `Resize)
  @@ fun term ->
  let redraw = Ui.make_redraw ~term ~model in
  let fps = Ui.read_fps_env () in
  let throttler =
    Ui.init_throttler ~fps ~enqueue_redraw:(fun () ->
      Eio.Stream.add internal_stream `Redraw)
  in
  let redraw_immediate () = Redraw_throttle.redraw_immediate throttler ~draw:redraw in
  redraw ();
  (* Start the periodic scheduler to coalesce frequent updates. *)
  Ui.spawn_throttler ~env ~sw:ui_sw ~throttler;
  let ui : App_context.Ui.t = { term; throttler; redraw; redraw_immediate } in
  let shared : App_context.Resources.t = { services; streams; ui } in
  let streaming : Streaming_submit.Context.t =
    { shared
    ; cfg = prompt_ctx.cfg
    ; tools = prompt_ctx.tools
    ; tool_tbl = prompt_ctx.tool_tbl
    ; parallel_tool_calls
    ; history_compaction = true
    }
  in
  let submit : App_submit.Context.t = { runtime; streaming } in
  let compaction : App_compaction.Context.t = { shared; runtime } in
  let reducer_ctx : App_reducer.Context.t =
    { runtime; shared; submit; compaction; cancelled = Streaming_submit.Cancelled }
  in
  let quit_via_esc = App_reducer.run reducer_ctx in
  Shutdown.shutdown
    ~env
    ~term
    ~quit_via_esc
    ~prompt_file
    ~export_file
    ~persist_mode
    ~session
    ~model
    ~cfg
    ~initial_msg_count
    ()
;;
(* Exit the program. *)
