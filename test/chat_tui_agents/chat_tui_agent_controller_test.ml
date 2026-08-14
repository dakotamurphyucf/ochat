open Core
module Controller = Chat_tui.Controller
module Model = Chat_tui.Model

let dummy_term : Notty_eio.Term.t = Obj.magic 0

let make_model
      ?(input_line = "draft")
      ?(cursor_pos = 2)
      ?(selection_anchor = Some 1)
      ?(mode = Model.Insert)
      ?(cmdline = "command")
      ?(cmdline_cursor = 3)
      ()
  =
  Model.create
    ~history_items:[]
    ~messages:[]
    ~input_line
    ~auto_follow:false
    ~msg_buffers:(Hashtbl.create (module String))
    ~function_name_by_id:(Hashtbl.create (module String))
    ~reasoning_idx_by_id:(Hashtbl.create (module String))
    ~tool_output_by_index:(Hashtbl.create (module Int))
    ~tasks:[]
    ~kv_store:(Hashtbl.create (module String))
    ~fetch_sw:None
    ~scroll_box:(Notty_scroll_box.create Notty.I.empty)
    ~cursor_pos
    ~selection_anchor
    ~mode
    ~draft_mode:Model.Raw_xml
    ~selected_msg:None
    ~undo_stack:[]
    ~redo_stack:[]
    ~cmdline
    ~cmdline_cursor
;;

let mode_name = function
  | Model.Insert -> "Insert"
  | Normal -> "Normal"
  | Cmdline -> "Cmdline"
  | Search _ -> "Search"
;;

let reaction_name = function
  | Controller.Redraw -> "Redraw"
  | Refresh_messages -> "Refresh_messages"
  | Submit_input -> "Submit_input"
  | Cancel_or_quit -> "Cancel_or_quit"
  | Compact_context -> "Compact_context"
  | Quit -> "Quit"
  | Chat_scrolled changed -> sprintf "Chat_scrolled(%b)" changed
  | Prepare_chat_destination _ -> "Prepare_chat_destination"
  | Shell_approval_response _ -> "Shell_approval_response"
  | Shell_grant_revoke_requested _ -> "Shell_grant_revoke_requested"
  | Shell_management_refresh_requested _ -> "Shell_management_refresh_requested"
  | Moderator_input_response _ -> "Moderator_input_response"
  | Unhandled -> "Unhandled"
;;

let page_name model =
  match Model.active_page model with
  | Chat -> "Chat"
  | Agent -> "Agent"
  | Shell_security -> "Shell_security"
;;

let start model =
  ignore
    (Model.agent_call_started
       model
       ~call_id:"call"
       ~name:"worker"
       ~kind:`Function
       ~payload:"{}"
       ~agent_page_kind:Chat_response.Tool_execution_event.Subagent
     : bool)
;;

let ctrl_g = `Key (`ASCII 'g', [ `Ctrl ])
let bel = `Key (`ASCII '\007', [])

let%expect_test "Notty-decoded terminal Ctrl-G opens Agent" =
  let model = make_model () in
  start model;
  Notty.Unescape.decode [ Stdlib.Uchar.of_int 0x07 ]
  |> List.iter ~f:(fun event ->
    printf
      "%b %s\n"
      (Controller.is_ctrl_g event)
      (let reaction = Controller.handle_key ~model ~term:dummy_term event in
       Printf.sprintf "%s %s" (reaction_name reaction) (page_name model)));
  [%expect {| true Redraw Agent |}]
;;

let%expect_test "Ctrl-G and BEL toggle from every editor mode" =
  let modes = [ Model.Insert; Normal; Cmdline; Search Forward ] in
  List.iter modes ~f:(fun mode ->
    List.iter [ ctrl_g; bel ] ~f:(fun event ->
      let model = make_model ~mode () in
      start model;
      let reaction = Controller.handle_key ~model ~term:dummy_term event in
      printf
        "%s %s %s cmd=%S\n"
        (mode_name mode)
        (reaction_name reaction)
        (page_name model)
        (Model.cmdline model)));
  [%expect
    {|
    Insert Redraw Agent cmd="command"
    Insert Redraw Agent cmd="command"
    Normal Redraw Agent cmd="command"
    Normal Redraw Agent cmd="command"
    Cmdline Redraw Agent cmd="command"
    Cmdline Redraw Agent cmd="command"
    Search Redraw Agent cmd="command"
    Search Redraw Agent cmd="command"
    |}]
;;

let%expect_test "Ctrl-G with no active calls is inert" =
  let model = make_model ~mode:Model.Cmdline () in
  let reaction = Controller.handle_key ~model ~term:dummy_term bel in
  print_s
    [%sexp
      (reaction_name reaction : string)
    , (page_name model : string)
    , (Model.cmdline model : string)];
  [%expect {| (Unhandled Chat command) |}]
;;

let%expect_test "Agent Escape, Ctrl-G, and printable keys preserve Chat state" =
  List.iter
    [ `Key (`Escape, []); ctrl_g; bel ]
    ~f:(fun event ->
      let model = make_model ~mode:Model.Normal () in
      start model;
      Model.set_active_page model Agent;
      let chat_box = Model.scroll_box model in
      Notty_scroll_box.set_content
        chat_box
        (Notty.I.vcat (List.init 20 ~f:(fun _ -> Notty.I.string Notty.A.empty "chat")));
      Notty_scroll_box.scroll_by chat_box ~height:3 7;
      let before_scroll = Notty_scroll_box.scroll chat_box in
      let reaction = Controller.handle_key ~model ~term:dummy_term event in
      printf
        "%s %s draft=%S cursor=%d selection=%s scroll=%d\n"
        (reaction_name reaction)
        (page_name model)
        (Model.input_line model)
        (Model.cursor_pos model)
        (Option.value_map (Model.selection_anchor model) ~default:"none" ~f:Int.to_string)
        (Notty_scroll_box.scroll chat_box);
      assert (Int.equal before_scroll (Notty_scroll_box.scroll chat_box)));
  let model = make_model ~mode:Model.Insert () in
  start model;
  Model.set_active_page model Agent;
  let printable = Controller.handle_key ~model ~term:dummy_term (`Key (`ASCII 'x', [])) in
  print_s
    [%sexp
      (reaction_name printable : string)
    , (Model.input_line model : string)
    , (mode_name (Model.mode model) : string)];
  [%expect
    {|
    Redraw Chat draft="draft" cursor=2 selection=1 scroll=7
    Redraw Chat draft="draft" cursor=2 selection=1 scroll=7
    Redraw Chat draft="draft" cursor=2 selection=1 scroll=7
    (Unhandled draft Insert)
    |}]
;;

let%expect_test "Agent Escape returns to Chat before Chat Escape cancels or quits" =
  let model = make_model ~mode:Model.Normal () in
  start model;
  Model.set_active_page model Agent;
  let first = Controller.handle_key ~model ~term:dummy_term (`Key (`Escape, [])) in
  let second = Controller.handle_key ~model ~term:dummy_term (`Key (`Escape, [])) in
  print_s
    [%sexp
      (reaction_name first : string)
    , (page_name model : string)
    , (reaction_name second : string)];
  [%expect {| (Redraw Chat Cancel_or_quit) |}]
;;

let%expect_test "Agent scrolling is page-local" =
  let model = make_model () in
  start model;
  ignore
    (Model.agent_call_progress
       model
       ~call_id:"call"
       { channel = `Stdout
       ; update = Append (String.concat ~sep:"\n" (List.init 20 ~f:Int.to_string))
       }
     : bool);
  Model.set_active_page model Agent;
  ignore (Chat_tui.Renderer.render_full ~size:(30, 8) ~model);
  let chat_box = Model.scroll_box model in
  Notty_scroll_box.set_content
    chat_box
    (Notty.I.vcat (List.init 20 ~f:(fun _ -> Notty.I.string Notty.A.empty "chat")));
  Notty_scroll_box.scroll_by chat_box ~height:3 4;
  let handle event =
    Chat_tui.Controller_agent.For_testing.handle_key ~model ~size:(fun () -> 30, 8) event
  in
  ignore (handle (`Key (`Page `Down, [])) : Controller.reaction);
  printf
    "down agent=%d chat=%d\n"
    (Notty_scroll_box.scroll (Model.agent_scroll_box model))
    (Notty_scroll_box.scroll chat_box);
  ignore (handle (`Key (`Page `Up, [])) : Controller.reaction);
  printf
    "up agent=%d chat=%d\n"
    (Notty_scroll_box.scroll (Model.agent_scroll_box model))
    (Notty_scroll_box.scroll chat_box);
  ignore (handle (`Key (`End, [])) : Controller.reaction);
  printf
    "end agent=%d chat=%d\n"
    (Notty_scroll_box.scroll (Model.agent_scroll_box model))
    (Notty_scroll_box.scroll chat_box);
  ignore (handle (`Key (`Home, [])) : Controller.reaction);
  printf
    "home agent=%d chat=%d\n"
    (Notty_scroll_box.scroll (Model.agent_scroll_box model))
    (Notty_scroll_box.scroll chat_box);
  [%expect
    {|
    down agent=24 chat=4
    up agent=19 chat=4
    end agent=24 chat=4
    home agent=0 chat=4
    |}]
;;

let%expect_test "Agent mouse wheel scrolls output without changing selection or Chat" =
  let model = make_model () in
  start model;
  ignore
    (Model.agent_call_progress
       model
       ~call_id:"call"
       { channel = `Stdout
       ; update = Append (String.concat ~sep:"\n" (List.init 20 ~f:Int.to_string))
       }
     : bool);
  Model.set_active_page model Agent;
  ignore (Chat_tui.Renderer.render_full ~size:(30, 8) ~model);
  let chat_box = Model.scroll_box model in
  Notty_scroll_box.set_content
    chat_box
    (Notty.I.vcat (List.init 20 ~f:(fun _ -> Notty.I.string Notty.A.empty "chat")));
  Notty_scroll_box.scroll_by chat_box ~height:3 4;
  let selected_before =
    Model.selected_agent_call model |> Option.value_exn |> Model.agent_call_id
  in
  let handle direction =
    Chat_tui.Controller_agent.For_testing.handle_key
      ~model
      ~size:(fun () -> 30, 8)
      (`Mouse (`Press (`Scroll direction), (2, 3), []))
  in
  ignore (handle `Up : Controller.reaction);
  printf
    "up agent=%d chat=%d selected=%b follow=%b\n"
    (Notty_scroll_box.scroll (Model.agent_scroll_box model))
    (Notty_scroll_box.scroll chat_box)
    (String.equal
       selected_before
       (Model.selected_agent_call model |> Option.value_exn |> Model.agent_call_id))
    (Model.agent_auto_follow model);
  ignore (handle `Down : Controller.reaction);
  printf
    "down agent=%d chat=%d follow=%b\n"
    (Notty_scroll_box.scroll (Model.agent_scroll_box model))
    (Notty_scroll_box.scroll chat_box)
    (Model.agent_auto_follow model);
  [%expect
    {|
    up agent=23 chat=4 selected=true follow=false
    down agent=24 chat=4 follow=true
    |}]
;;

let%expect_test "Agent Ctrl-arrows scroll output while j and k select calls" =
  let model = make_model () in
  start model;
  ignore
    (Model.agent_call_started
       model
       ~call_id:"second"
       ~name:"second"
       ~kind:`Function
       ~payload:"{}"
       ~agent_page_kind:Chat_response.Tool_execution_event.Subagent
     : bool);
  ignore
    (Model.agent_call_progress
       model
       ~call_id:"call"
       { channel = `Stdout
       ; update = Append (String.concat ~sep:"\n" (List.init 20 ~f:Int.to_string))
       }
     : bool);
  Model.set_active_page model Agent;
  ignore (Chat_tui.Renderer.render_full ~size:(30, 8) ~model);
  let chat_box = Model.scroll_box model in
  Notty_scroll_box.set_content
    chat_box
    (Notty.I.vcat (List.init 20 ~f:(fun _ -> Notty.I.string Notty.A.empty "chat")));
  Notty_scroll_box.scroll_by chat_box ~height:3 4;
  let selected () =
    Model.selected_agent_call model |> Option.value_exn |> Model.agent_call_id
  in
  let handle event =
    Chat_tui.Controller_agent.For_testing.handle_key ~model ~size:(fun () -> 30, 8) event
  in
  ignore (handle (`Key (`Arrow `Up, [ `Ctrl ])) : Controller.reaction);
  printf
    "arrow-up agent=%d chat=%d selected=%s follow=%b\n"
    (Notty_scroll_box.scroll (Model.agent_scroll_box model))
    (Notty_scroll_box.scroll chat_box)
    (selected ())
    (Model.agent_auto_follow model);
  ignore (handle (`Key (`Arrow `Down, [ `Ctrl ])) : Controller.reaction);
  printf
    "arrow-down agent=%d selected=%s follow=%b\n"
    (Notty_scroll_box.scroll (Model.agent_scroll_box model))
    (selected ())
    (Model.agent_auto_follow model);
  ignore (handle (`Key (`ASCII 'j', [])) : Controller.reaction);
  printf "j selected=%s\n" (selected ());
  ignore (handle (`Key (`ASCII 'k', [])) : Controller.reaction);
  printf "k selected=%s\n" (selected ());
  [%expect
    {|
    arrow-up agent=23 chat=4 selected=call follow=false
    arrow-down agent=24 selected=call follow=true
    j selected=second
    k selected=call
    |}]
;;

let%expect_test "opening Agent clears pending Normal operator" =
  Chat_tui.Controller_normal.cancel_pending ();
  let model =
    make_model
      ~mode:Model.Normal
      ~input_line:"alpha beta"
      ~cursor_pos:0
      ~selection_anchor:None
      ()
  in
  start model;
  let pending = Controller.handle_key ~model ~term:dummy_term (`Key (`ASCII 'd', [])) in
  let opened = Controller.handle_key ~model ~term:dummy_term ctrl_g in
  Model.set_active_page model Chat;
  let moved = Controller.handle_key ~model ~term:dummy_term (`Key (`ASCII 'w', [])) in
  print_s
    [%sexp
      (reaction_name pending : string)
    , (reaction_name opened : string)
    , (reaction_name moved : string)
    , (Model.input_line model : string)
    , (Model.cursor_pos model : int)];
  [%expect {| (Unhandled Redraw Redraw "alpha beta" 6) |}]
;;
