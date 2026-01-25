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

type handle_submit =
  env:Eio_unix.Stdenv.base
  -> history:Openai.Responses.Item.t list
  -> internal_stream:App_events.internal_event Eio.Stream.t
  -> system_event:string Eio.Stream.t
  -> datadir:Eio.Fs.dir_ty Eio.Path.t
  -> parallel_tool_calls:bool
  -> history_compaction:bool
  -> op_id:int
  -> unit

let capture_request ~model : request =
  let text = Model.input_line model in
  let draft_mode = Model.draft_mode model in
  { Runtime.text; Runtime.draft_mode }
;;

let clear_editor ~model : unit =
  Model.set_input_line model "";
  Model.set_cursor_pos model 0;
  Model.set_draft_mode model Model.Plain
;;

let add_placeholder_thinking_message (model : Model.t) : unit =
  let patch = Add_placeholder_message { role = "assistant"; text = "(thinking…)" } in
  ignore (Model.apply_patch model patch)
;;

let get_user_message_item text =
  let open Openai.Responses in
  Item.Input_message
    { Input_message.role = Input_message.User
    ; content = [ Input_message.Text { text; _type = "input_text" } ]
    ; _type = "message"
    }
;;

let apply_start_effects
      ~cwd
      ~env
      ~cache
      ~model
      ~term
      ~throttler
      ~(submit_request : request)
  =
  let user_msg = String.strip submit_request.Runtime.text in
  if not (String.is_empty user_msg)
  then (
    match submit_request.Runtime.draft_mode with
    | Model.Plain ->
      ignore (Model.apply_patch model (Add_user_message { text = user_msg }));
      ignore @@ Model.add_history_item model (get_user_message_item user_msg)
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
                 ~run_agent:(Chat_response.Driver.run_agent ~history_compaction:true)
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
      ignore (Model.add_history_item model user_msg));
  Model.set_auto_follow model true;
  let _, h = Notty_eio.Term.size term in
  let input_h =
    match String.split_lines (Model.input_line model) with
    | [] -> 1
    | ls -> List.length ls
  in
  Scroll_box.scroll_to_bottom (Model.scroll_box model) ~height:(h - input_h);
  add_placeholder_thinking_message model;
  Redraw_throttle.request_redraw throttler
;;

let start
      ~env
      ~ui_sw
      ~cwd
      ~cache
      ~datadir
      ~term
      ~runtime
      ~internal_stream
      ~system_event
      ~throttler
      ~handle_submit
      ~parallel_tool_calls
      (submit_request : request)
  =
  apply_start_effects
    ~cwd
    ~env
    ~cache
    ~model:runtime.Runtime.model
    ~term
    ~throttler
    ~submit_request;
  let op_id = Runtime.alloc_op_id runtime in
  runtime.Runtime.op <- Some (Runtime.Starting_streaming { id = op_id });
  runtime.Runtime.cancel_streaming_on_start <- false;
  let history = Model.history_items runtime.Runtime.model in
  Fiber.fork ~sw:ui_sw (fun () ->
    handle_submit
      ~env
      ~history
      ~internal_stream
      ~system_event
      ~datadir
      ~parallel_tool_calls
      ~history_compaction:true
      ~op_id)
;;
