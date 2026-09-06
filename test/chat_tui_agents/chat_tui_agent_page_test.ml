open Core
module Execution = Chat_response.Tool_execution_event
module Model = Chat_tui.Model

let make_model ?(history_items = []) () =
  let history_items =
    let allocator =
      History_entry.Allocator.create ~namespace:"agent-page-test" ~next_sequence:0
      |> Result.ok_or_failwith
    in
    List.map history_items ~f:(History_entry.create ~allocator)
    |> Result.all
    |> Result.ok_or_failwith
  in
  Model.create
    ~history_items
    ~messages:(Chat_tui.Conversation.of_history (History_entry.items history_items))
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

let page_name model =
  match Model.active_page model with
  | Chat -> "Chat"
  | Agent -> "Agent"
  | Shell_security -> "Shell_security"
;;

let call_ids model = Model.active_agent_calls model |> List.map ~f:Model.agent_call_id

let selected_id model =
  Model.selected_agent_call model |> Option.map ~f:Model.agent_call_id
;;

let progress_text call =
  Model.agent_call_progress_entries call
  |> List.map ~f:Model.progress_entry_text
  |> String.concat
;;

let channel_name = function
  | `Assistant -> "Assistant"
  | `Reasoning -> "Reasoning"
  | `Stdout -> "Stdout"
  | `Stderr -> "Stderr"
  | `Activity -> "Activity"
;;

let start ?(payload = "{}") ?(agent_page_kind = Execution.Subagent) model call_id name =
  Model.agent_call_started model ~call_id ~name ~kind:`Function ~payload ~agent_page_kind
;;

let progress model call_id channel update =
  Model.agent_call_progress model ~call_id { channel; update }
;;

let render_to_string ~w ~h model =
  let image, _ = Chat_tui.Renderer.render_full ~size:(w, h) ~model in
  let buffer = Buffer.create 256 in
  Notty.Render.to_buffer buffer Notty.Cap.dumb (0, 0) (w, h) image;
  Buffer.contents buffer
;;

let%expect_test "lifecycle is deterministic, selected completion returns to Chat" =
  let model = make_model () in
  print_s
    [%sexp
      (page_name model : string)
    , (call_ids model : string list)
    , (selected_id model : string option)];
  ignore (start model "call-1" "one" : bool);
  ignore (start model "call-2" "two" : bool);
  ignore (progress model "call-2" `Stdout (Append "two") : bool);
  ignore (progress model "call-1" `Assistant (Append "one") : bool);
  let history_before = Model.history_items model in
  print_s
    [%sexp
      (page_name model : string)
    , (call_ids model : string list)
    , (selected_id model : string option)
    , (List.map (Model.active_agent_calls model) ~f:progress_text : string list)
    , (Poly.equal history_before (Model.history_items model) : bool)];
  Model.set_active_page model Agent;
  ignore
    (Model.agent_call_finished model ~outcome:Returned ~output:None ~call_id:"call-1"
     : bool);
  print_s
    [%sexp
      (page_name model : string)
    , (call_ids model : string list)
    , (selected_id model : string option)];
  Model.set_active_page model Agent;
  ignore
    (Model.agent_call_finished model ~outcome:Returned ~output:None ~call_id:"call-2"
     : bool);
  print_s
    [%sexp
      (page_name model : string)
    , (call_ids model : string list)
    , (selected_id model : string option)];
  [%expect
    {|
    (Chat () ())
    (Chat (call-1 call-2) (call-1) (one two) true)
    (Agent (call-1 call-2) (call-1))
    (Agent (call-1 call-2) (call-1))
    |}]
;;

let%expect_test "unselected completion and all terminal outcomes remove calls" =
  let outcomes = [ Execution.Returned; Raised; Cancelled ] in
  List.iteri outcomes ~f:(fun index outcome ->
    let model = make_model () in
    ignore (start model "selected" "selected" : bool);
    ignore (start model "other" "other" : bool);
    Model.set_active_page model Agent;
    let call_id = if index = 0 then "other" else "selected" in
    let event = Execution.Finished { call_id; outcome; output = None } in
    (match event with
     | Finished { call_id; _ } ->
       ignore (Model.agent_call_finished model ~outcome ~output:None ~call_id : bool)
     | _ -> assert false);
    print_s
      [%sexp
        (page_name model : string)
      , (call_ids model : string list)
      , (selected_id model : string option)]);
  [%expect
    {|
    (Agent (selected other) (selected))
    (Agent (selected other) (selected))
    (Agent (selected other) (selected))
    |}]
;;

let%expect_test "duplicate and late events are idempotent" =
  let model = make_model () in
  let first = start model "call" "tool" in
  let duplicate_start = start model "call" "changed-name" in
  let first_finish =
    Model.agent_call_finished model ~outcome:Returned ~output:None ~call_id:"call"
  in
  let duplicate_finish =
    Model.agent_call_finished model ~outcome:Returned ~output:None ~call_id:"call"
  in
  let late_progress = progress model "call" `Activity (Append "late") in
  let restart = start model "call" "tool" in
  print_s
    [%sexp
      ((first, duplicate_start, first_finish, duplicate_finish, late_progress, restart)
       : bool * bool * bool * bool * bool * bool)];
  [%expect {| (true true true false false false) |}]
;;

let%expect_test "Replace updates the latest replaceable entry by channel" =
  let model = make_model () in
  ignore (start model "call" "tool" : bool);
  ignore (progress model "call" `Activity (Replace "first") : bool);
  ignore (progress model "call" `Stdout (Append "output") : bool);
  ignore (progress model "call" `Activity (Replace "second") : bool);
  let call = Model.selected_agent_call model |> Option.value_exn in
  Model.agent_call_progress_entries call
  |> List.iter ~f:(fun entry ->
    match Model.progress_entry_text_view entry with
    | Some (channel, text) ->
      print_s [%sexp (channel_name channel : string), (text : string)]
    | None -> ());
  [%expect
    {|
    (Activity second)
    (Stdout output)
    |}]
;;

let%expect_test "per-call and global progress limits retain bounded suffixes" =
  let model = make_model () in
  ignore (start model "oversized" "oversized" : bool);
  let oversized_text =
    String.make 10 'a' ^ String.make 999_990 'x' ^ String.make 10 'z'
  in
  ignore (progress model "oversized" `Stdout (Append oversized_text) : bool);
  let oversized = Model.selected_agent_call model |> Option.value_exn in
  printf
    "single bytes=%d prefix=%S suffix=%S truncated=%b\n"
    (String.length (progress_text oversized))
    (String.prefix (progress_text oversized) 10)
    (String.suffix (progress_text oversized) 10)
    (Model.agent_call_is_truncated oversized);
  for index = 0 to 16 do
    let call_id = "global-" ^ Int.to_string index in
    ignore (start model call_id call_id : bool);
    ignore (progress model call_id `Stdout (Append (String.make 1_000_000 'y')) : bool);
    ignore (progress model call_id `Activity (Append "z") : bool)
  done;
  let retained =
    Model.active_agent_calls model
    |> List.sum (module Int) ~f:(fun call -> String.length (progress_text call))
  in
  let truncated_count =
    Model.active_agent_calls model |> List.count ~f:Model.agent_call_is_truncated
  in
  printf "global bytes=%d truncated-calls=%d\n" retained truncated_count;
  [%expect
    {|
    single bytes=1000000 prefix="xxxxxxxxxx" suffix="zzzzzzzzzz" truncated=true
    global bytes=1000017 truncated-calls=18
    |}]
;;

let%expect_test "Agent rendering covers one, many, and narrow terminals" =
  let one = make_model () in
  ignore
    (start
       ~payload:{|{"query":"ChatML chat template message roles serialization"}|}
       one
       "call-123456789"
       "odoc_search"
     : bool);
  ignore (progress one "call-123456789" `Stdout (Append "hello") : bool);
  Model.set_active_page one Agent;
  print_string (render_to_string ~w:64 ~h:14 one);
  [%expect
    {|
    Agent tools — 1 running · 1 calls
    > odoc_search:23456789

    🛠  Odoc_search Agent

    odoc_search({"query":"ChatML chat template message roles seriali
    zation"})


    📬 Tool_output

    hello

    Ctrl-G/Esc chat  j/k select  ↑↓/PgUp/PgDn scroll
    |}];
  let many = make_model () in
  ignore (start many "first" "one" : bool);
  ignore (start many "second" "two" : bool);
  Model.set_active_page many Agent;
  print_string (render_to_string ~w:32 ~h:5 many);
  [%expect
    {|
    Agent tools — 2 running · 2 call
    > one:first    two:second

    Waiting for progress…
    Ctrl-G/Esc chat  j/k select  ↑↓/
    |}];
  print_string (render_to_string ~w:8 ~h:3 many);
  [%expect
    {|
    Agent to
    > one:fi
      two:se
    |}]
;;

let%expect_test "Agent output follows the bottom until the user scrolls up" =
  let model = make_model () in
  ignore (start ~payload:"prompt" model "call" "worker" : bool);
  ignore
    (progress
       model
       "call"
       `Stdout
       (Append (String.concat ~sep:"\n" (List.init 20 ~f:Int.to_string)))
     : bool);
  Model.set_active_page model Agent;
  ignore (Chat_tui.Renderer.render_full ~size:(30, 8) ~model);
  let box = Model.agent_scroll_box model in
  let height =
    (Chat_tui.Agent_page_layout.compute ~screen_w:30 ~screen_h:8 ~model).scroll_height
  in
  printf
    "initial scroll=%d max=%d follow=%b\n"
    (Notty_scroll_box.scroll box)
    (Notty_scroll_box.max_scroll box ~height)
    (Model.agent_auto_follow model);
  ignore
    (Chat_tui.Controller_agent.For_testing.handle_key
       ~model
       ~size:(fun () -> 30, 8)
       (`Key (`Arrow `Up, []))
     : Chat_tui.Controller.reaction);
  let scrolled_up = Notty_scroll_box.scroll box in
  ignore (progress model "call" `Stdout (Append "\nnew") : bool);
  ignore (Chat_tui.Renderer.render_full ~size:(30, 8) ~model);
  printf
    "updated scroll=%d preserved=%b max=%d follow=%b\n"
    (Notty_scroll_box.scroll box)
    (Int.equal scrolled_up (Notty_scroll_box.scroll box))
    (Notty_scroll_box.max_scroll box ~height)
    (Model.agent_auto_follow model);
  ignore
    (Chat_tui.Controller_agent.For_testing.handle_key
       ~model
       ~size:(fun () -> 30, 8)
       (`Key (`End, []))
     : Chat_tui.Controller.reaction);
  ignore (progress model "call" `Stdout (Append "\nnewest") : bool);
  ignore (Chat_tui.Renderer.render_full ~size:(30, 8) ~model);
  printf
    "resumed scroll=%d max=%d follow=%b\n"
    (Notty_scroll_box.scroll box)
    (Notty_scroll_box.max_scroll box ~height)
    (Model.agent_auto_follow model);
  [%expect
    {|
    initial scroll=24 max=24 follow=true
    updated scroll=23 preserved=true max=25 follow=false
    resumed scroll=26 max=26 follow=true
    |}]
;;

let%expect_test "nested calls keep custom stdout and final output with their tool" =
  let model = make_model () in
  ignore (start ~payload:{|{"input":"inspect"}|} model "outer" "research" : bool);
  ignore
    (Model.agent_call_trace
       model
       ~call_id:"outer"
       (Ochat_function.Trace.Tool_started
          { call_id = "nested"; name = "shell"; kind = `Custom; payload = "printf hello" })
     : bool);
  ignore
    (Model.agent_call_trace
       model
       ~call_id:"outer"
       (Tool_progress
          { call_id = "nested"
          ; progress = { channel = `Stdout; update = Append "hello" }
          })
     : bool);
  ignore
    (Model.agent_call_trace
       model
       ~call_id:"outer"
       (Tool_finished
          { call_id = "nested"
          ; outcome = Returned
          ; output = Some (Openai.Responses.Tool_output.Output.Text "hello")
          })
     : bool);
  let call = Model.selected_agent_call model |> Option.value_exn in
  let nested =
    Model.agent_call_progress_entries call
    |> List.filter_map ~f:Model.progress_entry_tool_view
    |> List.hd_exn
  in
  let call_id, name, _, payload, progress, outcome, output = nested in
  let progress =
    List.map progress ~f:(fun (channel, text) -> channel_name channel, text)
  in
  let output =
    match output with
    | Some (Openai.Responses.Tool_output.Output.Text text) -> text
    | Some (Content _) | None -> ""
  in
  print_s
    [%sexp
      (call_id : string)
    , (name : string)
    , (payload : string)
    , (progress : (string * string) list)
    , (Option.is_some outcome : bool)
    , (output : string)];
  let persisted =
    Chat_tui.Persistence.history_entries_as_chatmd
      ~moderator_snapshot:None
      ~history:(Model.history_items model)
  in
  print_s
    [%sexp
      (not (String.is_substring persisted ~substring:"printf hello") : bool)
    , (not (String.is_substring persisted ~substring:"hello") : bool)];
  Model.set_active_page model Agent;
  render_to_string ~w:50 ~h:30 model
  |> String.split_lines
  |> List.filter ~f:(fun line ->
    List.exists [ "Tool"; "shell("; "hello"; "Returned" ] ~f:(fun text ->
      String.is_substring line ~substring:text))
  |> List.iter ~f:(fun line -> print_endline (String.rstrip line));
  [%expect
    {|
    (nested shell "printf hello" ((Stdout hello)) true hello)
    (true true)
    🛠  Tool
    shell(printf hello)
    📬 Tool_output
    hello
    ✓ Returned
    |}]
;;

let%expect_test "nested terminal outcomes remain visible beside existing progress" =
  let render_outcome outcome =
    let model = make_model () in
    ignore (start model "outer" "research" : bool);
    ignore
      (Model.agent_call_trace
         model
         ~call_id:"outer"
         (Ochat_function.Trace.Tool_started
            { call_id = "nested"; name = "worker"; kind = `Function; payload = "{}" })
       : bool);
    ignore
      (Model.agent_call_trace
         model
         ~call_id:"outer"
         (Tool_progress
            { call_id = "nested"
            ; progress = { channel = `Stdout; update = Append "existing output" }
            })
       : bool);
    ignore
      (Model.agent_call_trace
         model
         ~call_id:"outer"
         (Tool_finished { call_id = "nested"; outcome; output = None })
       : bool);
    Model.set_active_page model Agent;
    render_to_string ~w:50 ~h:30 model
    |> String.split_lines
    |> List.find_exn ~f:(fun line ->
      List.exists [ "Returned"; "Raised"; "Cancelled" ] ~f:(fun text ->
        String.is_substring line ~substring:text))
    |> String.strip
    |> print_endline
  in
  List.iter [ Ochat_function.Trace.Returned; Raised; Cancelled ] ~f:render_outcome;
  [%expect
    {|
    ✓ Returned
    ✗ Raised
    ⊘ Cancelled
    |}]
;;

let%expect_test "three top-level Agents retain immediate terminal states" =
  let model = make_model () in
  List.iter [ "one"; "two"; "three" ] ~f:(fun call_id ->
    ignore (start model call_id "research" : bool));
  Model.set_active_page model Agent;
  List.iter [ "three"; "one"; "two" ] ~f:(fun call_id ->
    ignore
      (Model.agent_call_finished model ~call_id ~outcome:Returned ~output:None : bool));
  print_s
    [%sexp
      (page_name model : string)
    , (List.map (Model.active_agent_calls model) ~f:(fun call ->
         Model.agent_call_id call, Option.is_some (Model.agent_call_outcome call))
       : (string * bool) list)
    , (selected_id model : string option)];
  render_to_string ~w:70 ~h:12 model
  |> String.split_lines
  |> List.filter ~f:(fun line ->
    String.is_substring line ~substring:"research:"
    || String.is_substring line ~substring:"Returned")
  |> List.iter ~f:(fun line -> print_endline (String.rstrip line));
  [%expect
    {|
    (Agent ((one true) (two true) (three true)) (one))
    > research:one ✓    research:two ✓    research:three ✓
    ✓ Returned
    |}]
;;

let%expect_test "shell-script invocation header omits Agent" =
  let model = make_model () in
  ignore
    (start
       ~agent_page_kind:Execution.Shell_script
       ~payload:{|{"arguments":["hello"]}|}
       model
       "shell-call"
       "run_shell"
     : bool);
  Model.set_active_page model Agent;
  render_to_string ~w:48 ~h:9 model
  |> String.split_lines
  |> List.filter ~f:(fun line ->
    String.is_substring line ~substring:"Run_shell"
    || String.is_substring line ~substring:"run_shell(")
  |> List.iter ~f:(fun line -> print_endline (String.rstrip line));
  [%expect
    {|
    Run_shell
    run_shell({"arguments":["hello"]})
    |}]
;;

let%expect_test "selector fills each row before wrapping Agent and script tabs" =
  let model = make_model () in
  List.iter
    [ "fMdlRk6k", Execution.Subagent
    ; "5S7HpkZy", Execution.Shell_script
    ; "68Jxciuh", Execution.Subagent
    ; "v2Kq9sNt", Execution.Shell_script
    ]
    ~f:(fun (call_id, agent_page_kind) ->
      ignore (start ~agent_page_kind model call_id "research" : bool));
  Model.set_active_page model Agent;
  let rendered = render_to_string ~w:42 ~h:10 model in
  List.iter [ "fMdlRk6k"; "5S7HpkZy"; "68Jxciuh"; "v2Kq9sNt" ] ~f:(fun call_id ->
    printf "%s=%b\n" call_id (String.is_substring rendered ~substring:call_id));
  printf
    "selector-height=%d\n"
    (Chat_tui.Agent_page_layout.compute ~screen_w:42 ~screen_h:10 ~model).selector_height;
  [%expect
    {|
    fMdlRk6k=true
    5S7HpkZy=true
    68Jxciuh=true
    v2Kq9sNt=true
    selector-height=2
    |}]
;;

let%expect_test "transient Agent state does not affect persisted transcript" =
  let history =
    [ Openai.Responses.Item.Input_message
        { role = Openai.Responses.Input_message.User
        ; content = [ Text { text = "persistent"; _type = "input_text" } ]
        ; _type = "message"
        }
    ]
  in
  let model = make_model ~history_items:history () in
  let persisted () =
    Chat_tui.Persistence.history_entries_as_chatmd
      ~moderator_snapshot:None
      ~history:(Model.history_items model)
  in
  let before = persisted () in
  ignore (start ~payload:"INPUT-ONLY" model "call" "worker" : bool);
  ignore (progress model "call" `Stdout (Append "transient") : bool);
  Model.set_active_page model Agent;
  print_s
    [%sexp
      (Poly.equal history (Model.history_items model |> History_entry.items) : bool)
    , (String.equal before (persisted ()) : bool)
    , (not (String.is_substring (persisted ()) ~substring:"transient") : bool)
    , (not (String.is_substring (persisted ()) ~substring:"INPUT-ONLY") : bool)
    , (page_name model : string)
    , (List.length (Model.active_agent_calls model) : int)];
  [%expect {| (true true true true Agent 1) |}]
;;

let%expect_test "canonical final output persists while progress does not" =
  let model = make_model () in
  ignore (start model "call" "worker" : bool);
  ignore (progress model "call" `Stdout (Append "LIVE-ONLY") : bool);
  let output =
    Openai.Responses.Item.Function_call_output
      { output = Openai.Responses.Tool_output.Output.Text "FINAL-RESULT"
      ; call_id = "call"
      ; _type = "function_call_output"
      ; id = None
      ; status = None
      }
  in
  let entry =
    History_entry.create
      ~allocator:
        (History_entry.Allocator.create ~namespace:"agent-page-output" ~next_sequence:0
         |> Result.ok_or_failwith)
      output
    |> Result.ok_or_failwith
  in
  ignore (Model.add_history_item model entry : Model.t);
  let persisted =
    Chat_tui.Persistence.history_entries_as_chatmd
      ~moderator_snapshot:None
      ~history:(Model.history_items model)
  in
  print_s
    [%sexp
      (String.is_substring persisted ~substring:"FINAL-RESULT" : bool)
    , (not (String.is_substring persisted ~substring:"LIVE-ONLY") : bool)];
  [%expect {| (true true) |}]
;;

let%expect_test "Agent rendering caches blocks and virtualizes warm scrolling" =
  let model = make_model () in
  ignore (start model "call" "worker" : bool);
  List.iteri (List.init 12 ~f:Fn.id) ~f:(fun index _ ->
    let channel = if index mod 2 = 0 then `Assistant else `Reasoning in
    ignore (progress model "call" channel (Append (Int.to_string index)) : bool));
  Model.set_active_page model Agent;
  Model.set_agent_auto_follow model false;
  let rendered = ref [] in
  let render block =
    let id = Model.agent_render_block_id block in
    rendered := id :: !rendered;
    Notty.I.void 20 1
  in
  let visible () =
    Chat_tui.Renderer_page_agent.For_testing.render_block_ids
      ~width:20
      ~height:3
      ~model
      ~render
  in
  let first_visible = visible () in
  let cold_count = List.length !rendered in
  rendered := [];
  let warm_visible = visible () in
  let warm_count = List.length !rendered in
  Notty_scroll_box.scroll_to (Model.agent_scroll_box model) 7;
  rendered := [];
  let scrolled_visible = visible () in
  let scroll_render_count = List.length !rendered in
  ignore (progress model "call" `Reasoning (Append " changed") : bool);
  rendered := [];
  ignore (visible () : int list);
  let append_render_count = List.length !rendered in
  rendered := [];
  ignore
    (Chat_tui.Renderer_page_agent.For_testing.render_block_ids
       ~width:19
       ~height:3
       ~model
       ~render
     : int list);
  let resize_render_count = List.length !rendered in
  print_s
    [%sexp
      (( (first_visible : int list)
       , (warm_visible : int list)
       , (scrolled_visible : int list)
       , cold_count
       , warm_count
       , scroll_render_count
       , append_render_count
       , resize_render_count )
       : int list * int list * int list * int * int * int * int * int)];
  [%expect {| ((-1 0 1) (-1 0 1) (6 7 8) 13 0 0 1 13) |}]
;;

let%expect_test "Agent rendering invalidates nested traces and terminal chrome only" =
  let model = make_model () in
  ignore (start model "outer" "research" : bool);
  ignore
    (Model.agent_call_trace
       model
       ~call_id:"outer"
       (Ochat_function.Trace.Tool_started
          { call_id = "nested"; name = "worker"; kind = `Function; payload = "{}" })
     : bool);
  Model.set_active_page model Agent;
  let rendered = ref [] in
  let render block =
    rendered := Model.agent_render_block_id block :: !rendered;
    Notty.I.void 30 1
  in
  let render_all () =
    Chat_tui.Renderer_page_agent.For_testing.render_block_ids
      ~width:30
      ~height:30
      ~model
      ~render
    |> ignore
  in
  render_all ();
  rendered := [];
  ignore
    (Model.agent_call_trace
       model
       ~call_id:"outer"
       (Tool_progress
          { call_id = "nested"; progress = { channel = `Stdout; update = Append "live" } })
     : bool);
  render_all ();
  let nested_updates = List.rev !rendered in
  rendered := [];
  ignore
    (Model.agent_call_finished model ~call_id:"outer" ~outcome:Returned ~output:None
     : bool);
  render_all ();
  let terminal_updates = List.rev !rendered in
  print_s [%sexp (nested_updates : int list), (terminal_updates : int list)];
  [%expect {| ((0) (-4)) |}]
;;
