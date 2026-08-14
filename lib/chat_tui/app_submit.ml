open Core
open Eio.Std
open Types
module Model = Model
module Redraw_throttle = Redraw_throttle
module CM = Prompt.Chat_markdown
module Scroll_box = Notty_scroll_box
module Res_item = Openai.Responses.Item
module Converter = Chat_response.Converter
module Ctx = Chat_response.Ctx
module Cache = Chat_response.Cache
module Runtime = App_runtime

type request = Runtime.submit_request

module Context = struct
  type t =
    { runtime : Runtime.t
    ; streaming : App_streaming.Context.t
    ; start_streaming : history:History_entry.t list -> op_id:int -> unit
    }
end

let capture_request ~model : request =
  let text = Model.input_line model in
  let draft_mode = Model.draft_mode model in
  { Runtime.text; Runtime.draft_mode }
;;

let clear_editor ~model : unit =
  ignore (Model.bump_typeahead_generation model : int);
  Model.clear_typeahead model;
  Model.set_input_line model "";
  Model.set_cursor_pos model 0;
  Model.set_draft_mode model Model.Plain
;;

let get_user_message_item text =
  let open Openai.Responses in
  Item.Input_message
    { Input_message.role = Input_message.User
    ; content = [ Input_message.Text { text; _type = "input_text" } ]
    ; _type = "message"
    }
;;

let apply_user_submit_effects
      ~cwd
      ~env
      ~cache
      ~response_dir
      ~allocator
      ~model
      ~(submit_request : request)
  =
  let user_msg = String.strip submit_request.Runtime.text in
  if not (String.is_empty user_msg)
  then (
    match submit_request.Runtime.draft_mode with
    | Model.Plain ->
      ignore (Model.apply_patch model (Add_user_message { text = user_msg }));
      let entry =
        History_entry.create ~allocator (get_user_message_item user_msg)
        |> Result.ok_or_failwith
      in
      ignore @@ Model.add_history_item model entry
    | Model.Raw_xml ->
      let xml =
        if String.is_prefix ~prefix:"<" user_msg
        then user_msg
        else Printf.sprintf "<user>\n%s\n</user>" user_msg
      in
      let elements =
        try CM.parse_chat_inputs ~dir:cwd xml with
        | exn ->
          Log.emit `Error (Printf.sprintf "XML parse error: %s" (Exn.to_string exn));
          []
      in
      Log.emit `Debug (Printf.sprintf "Parsed %d XML elements" (List.length elements));
      let user_msg =
        List.find_map_exn elements ~f:(function
          | CM.User m ->
            let ctx = Ctx.create ~env ~dir:cwd ~cache ~tool_dir:cwd in
            Some
              (Converter.convert_user_msg
                 ~ctx
                 ~run_agent:(fun ?prompt_dir ?session_id ~ctx prompt items ->
                   Chat_response.Driver.run_agent
                     ~history_compaction:true
                     ?prompt_dir
                     ?session_id
                     ~response_dir
                     ~ctx
                     prompt
                     items)
                 m)
          | _ -> None)
      in
      let user_msg_txt =
        match user_msg with
        | Openai.Responses.Item.Input_message msg ->
          List.fold msg.content ~init:None ~f:(fun acc user_msg ->
            match acc with
            | Some txt ->
              (match user_msg with
               | Openai.Responses.Input_message.Text t -> Some (txt ^ "\n" ^ t.text)
               | Openai.Responses.Input_message.Image img ->
                 Some (txt ^ "\n" ^ Printf.sprintf "<image src=\"%s\"/>" img.image_url))
            | None -> None)
        | _ ->
          failwith
          @@ Printf.sprintf
               "Expected user message, got: %s"
               (Res_item.jsonaf_of_t user_msg |> Jsonaf.to_string)
      in
      let txt = Option.value user_msg_txt ~default:(Util.sanitize xml) in
      ignore (Model.apply_patch model (Add_user_message { text = txt }));
      let entry = History_entry.create ~allocator user_msg |> Result.ok_or_failwith in
      ignore (Model.add_history_item model entry))
;;

let apply_turn_start_effects ~model ~screen_size ~throttler =
  Model.set_activity model (Some (Model.Assistant Model.Thinking));
  let screen_w, screen_h = screen_size in
  let layout = Chat_page_layout.compute ~screen_w ~screen_h ~model in
  Model.follow_chat_bottom model ~viewport_height:layout.scroll_height;
  Redraw_throttle.request_redraw throttler
;;

let begin_streaming_turn
      ~(runtime : Runtime.t)
      ~(now_ms : int)
      ~reason
      ~history
      ~start_streaming
  =
  let op_id = Runtime.alloc_op_id runtime in
  runtime.Runtime.op <- Some (Runtime.Starting_streaming { id = op_id });
  runtime.Runtime.cancel_streaming_on_start <- false;
  Runtime.note_started_turn runtime ~now_ms ~reason;
  Runtime.set_active_turn_start_reason runtime reason;
  Log.emit
    `Debug
    (Printf.sprintf
       "Starting turn reason=%s history_items=%d"
       (Runtime.string_of_turn_start_reason reason)
       (List.length history));
  start_streaming ~history ~op_id
;;

let start_from_current_session_with_screen_size
      (ctx : Context.t)
      ~now_ms
      ~screen_size
      ~reason
  =
  let runtime = ctx.runtime in
  let throttler = ctx.streaming.shared.ui.throttler in
  apply_turn_start_effects ~model:runtime.Runtime.model ~screen_size ~throttler;
  let history = Model.history_items runtime.Runtime.model in
  begin_streaming_turn
    ~runtime
    ~now_ms
    ~reason
    ~history
    ~start_streaming:ctx.start_streaming
;;

let start_from_current_session (ctx : Context.t) ~reason =
  let screen_size = ctx.streaming.shared.ui.size () in
  let now_ms =
    Eio.Time.now (Eio.Stdenv.clock ctx.streaming.shared.services.env) *. 1000.
    |> Int.of_float
  in
  start_from_current_session_with_screen_size ctx ~now_ms ~screen_size ~reason
;;

let start (ctx : Context.t) (submit_request : request) =
  let shared = ctx.streaming.shared in
  let services = shared.services in
  let env = services.env in
  let cwd = services.cwd in
  let cache = services.cache in
  let response_dir = services.datadir in
  let runtime = ctx.runtime in
  apply_user_submit_effects
    ~cwd
    ~env
    ~cache
    ~response_dir
    ~allocator:runtime.Runtime.history_allocator
    ~model:runtime.Runtime.model
    ~submit_request;
  start_from_current_session ctx ~reason:Runtime.User_submit
;;

let model_of_history history =
  Model.create
    ~history_items:history
    ~messages:(Conversation.of_history (History_entry.items history))
    ~input_line:""
    ~auto_follow:true
    ~msg_buffers:(Hashtbl.create (module String))
    ~function_name_by_id:(Hashtbl.create (module String))
    ~reasoning_idx_by_id:(Hashtbl.create (module String))
    ~tool_output_by_index:(Hashtbl.create (module Int))
    ~tasks:[]
    ~kv_store:(Hashtbl.create (module String))
    ~fetch_sw:None
    ~scroll_box:(Notty_scroll_box.create Notty.I.empty)
    ~cursor_pos:0
    ~selection_anchor:None
    ~mode:Model.Insert
    ~draft_mode:Model.Plain
    ~selected_msg:None
    ~undo_stack:[]
    ~redo_stack:[]
    ~cmdline:""
    ~cmdline_cursor:0
;;

let start_streaming_stub started_turns ~history ~op_id =
  started_turns := Some (op_id, history)
;;

let context_for_tests runtime started_turns =
  let shared : App_context.Resources.t =
    { services =
        { env = Obj.magic 0
        ; ui_sw = Obj.magic 0
        ; cwd = Obj.magic 0
        ; cache = Chat_response.Cache.create ~max_size:1 ()
        ; datadir = Obj.magic 0
        ; session = None
        }
    ; streams = { input = Obj.magic 0; internal = Obj.magic 0; redraw = Obj.magic 0 }
    ; ui =
        { term = Obj.magic 0
        ; size = (fun () -> 80, 24)
        ; throttler = Redraw_throttle.create ~fps:60. ~enqueue_redraw:(fun () -> ())
        ; redraw = (fun () -> ())
        ; redraw_immediate = (fun () -> ())
        ; latest_frame_generation = (fun () -> 0)
        ; resize_and_redraw = (fun ~size:_ ~layout:_ -> ())
        ; render_current_with_layout = (fun ~size:_ ~layout:_ -> ())
        }
    }
  in
  let streaming : App_streaming.Context.t =
    { shared
    ; allocator = runtime.Runtime.history_allocator
    ; cfg = Chat_response.Config.default
    ; tools = []
    ; tool_tbl = Hashtbl.create (module String)
    ; moderator = None
    ; safe_point_input = Some (Runtime.safe_point_input_source runtime)
    ; parallel_tool_calls = true
    ; history_compaction = false
    }
  in
  { Context.runtime; streaming; start_streaming = start_streaming_stub started_turns }
;;

let%test_unit "start_from_current_session preserves canonical history" =
  let allocator =
    History_entry.Allocator.create ~namespace:"submit-test" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let history =
    [ History_entry.create ~allocator (get_user_message_item "Hello")
      |> Result.ok_or_failwith
    ]
  in
  let model = model_of_history history in
  let runtime = Runtime.create ~history_allocator:allocator ~model () in
  let started_turns = ref None in
  let ctx = context_for_tests runtime started_turns in
  start_from_current_session_with_screen_size
    ctx
    ~now_ms:0
    ~screen_size:(80, 24)
    ~reason:Runtime.Idle_followup;
  [%test_result: int] (List.length (Model.history_items model)) ~expect:1;
  [%test_result: string option]
    (Option.map
       (Runtime.active_turn_start_reason runtime)
       ~f:Runtime.string_of_turn_start_reason)
    ~expect:(Some "idle_followup");
  [%test_result: bool]
    (match runtime.Runtime.op with
     | Some (Runtime.Starting_streaming _) -> true
     | Some (Runtime.Streaming _ | Runtime.Compacting _ | Runtime.Starting_compaction _)
     | None -> false)
    ~expect:true;
  [%test_result: int]
    (Option.value_map !started_turns ~default:0 ~f:(fun (_, history) ->
       List.length history))
    ~expect:1
;;

let%test_unit "start preserves submit append semantics" =
  let model = model_of_history [] in
  let runtime = Runtime.create ~model () in
  let started_turns = ref None in
  let ctx = context_for_tests runtime started_turns in
  apply_user_submit_effects
    ~cwd:(Obj.magic 0)
    ~env:(Obj.magic 0)
    ~cache:(Chat_response.Cache.create ~max_size:1 ())
    ~response_dir:(Obj.magic 0)
    ~allocator:runtime.Runtime.history_allocator
    ~model
    ~submit_request:{ Runtime.text = "Hello"; draft_mode = Model.Plain };
  start_from_current_session_with_screen_size
    ctx
    ~now_ms:0
    ~screen_size:(80, 24)
    ~reason:Runtime.User_submit;
  [%test_result: int] (List.length (Model.history_items model)) ~expect:1;
  [%test_result: string option]
    (Option.map
       (Runtime.active_turn_start_reason runtime)
       ~f:Runtime.string_of_turn_start_reason)
    ~expect:(Some "user_submit");
  [%test_result: int] (List.length (Model.messages model)) ~expect:1;
  [%test_result: bool]
    (match Model.activity model with
     | Some (Model.Assistant Model.Thinking) -> true
     | Some (Model.Assistant Model.Writing)
     | Some (Model.Assistant Model.Working)
     | Some Model.Compacting
     | None -> false)
    ~expect:true;
  [%test_result: int]
    (Option.value_map !started_turns ~default:0 ~f:(fun (_, history) ->
       List.length history))
    ~expect:1
;;
