open Core
module R1 = Chat_tui.Renderer
module R2 = Chat_tui.Renderer2
module M = Chat_tui.Model
module SB = Notty_scroll_box

let cap_dumb = Notty.Cap.dumb
let cap_ansi = Notty.Cap.ansi

let render_to_string ~cap ~w ~h (img : Notty.I.t) =
  let buf = Buffer.create 4096 in
  Notty.Render.to_buffer buf cap (0, 0) (w, h) img;
  Buffer.contents buf
;;

let mk_model ~(messages : Chat_tui.Types.message list) ~selected_idx ~auto_follow : M.t =
  let history_items = [] in
  let input_line = "" in
  let msg_buffers = Hashtbl.create (module String) in
  let function_name_by_id = Hashtbl.create (module String) in
  let reasoning_idx_by_id = Hashtbl.create (module String) in
  let tasks = [] in
  let kv_store = Hashtbl.create (module String) in
  let fetch_sw = None in
  let scroll_box = SB.create Notty.I.empty in
  let cursor_pos = 0 in
  let selection_anchor = None in
  let mode = M.Insert in
  let draft_mode = M.Plain in
  let undo_stack = [] in
  let redo_stack = [] in
  let cmdline = "" in
  let cmdline_cursor = 0 in
  M.create
    ~history_items
    ~messages
    ~input_line
    ~auto_follow
    ~msg_buffers
    ~function_name_by_id
    ~reasoning_idx_by_id
    ~tasks
    ~kv_store
    ~fetch_sw
    ~scroll_box
    ~cursor_pos
    ~selection_anchor
    ~mode
    ~draft_mode
    ~selected_msg:selected_idx
    ~undo_stack
    ~redo_stack
    ~cmdline
    ~cmdline_cursor
;;

let check_full_parity ~w ~h ~messages ~selected_idx ~auto_follow ~cap () =
  let m1 = mk_model ~messages ~selected_idx ~auto_follow in
  let m2 = mk_model ~messages ~selected_idx ~auto_follow in
  let img1, (cx1, cy1) = R1.render_full ~size:(w, h) ~model:m1 in
  let img2, (cx2, cy2) = R2.render_full ~size:(w, h) ~model:m2 in
  [%test_eq: int] (Notty.I.width img1) (Notty.I.width img2);
  [%test_eq: int] (Notty.I.height img1) (Notty.I.height img2);
  [%test_eq: int * int] (cx1, cy1) (cx2, cy2);
  let s1 = render_to_string ~cap ~w ~h img1 in
  let s2 = render_to_string ~cap ~w ~h img2 in
  [%test_eq: string] s1 s2
;;

let%test_unit "chat-tui renderer parity: simple conversation, dumb cap" =
  let w, h = 60, 18 in
  let messages =
    [ "user", "hello world"
    ; ( "assistant"
      , "this is a longer line with emoji 😀😀 and enough text to wrap across widths" )
    ]
  in
  check_full_parity ~w ~h ~messages ~selected_idx:None ~auto_follow:false ~cap:cap_dumb ()
;;

let%test_unit "chat-tui renderer parity: fenced code, tool role, ansi cap" =
  let w, h = 72, 22 in
  let code = "```python\nprint(\"hi\")\n```\nAnd now some text." in
  let messages = [ "tool_output", code ] in
  check_full_parity ~w ~h ~messages ~selected_idx:None ~auto_follow:false ~cap:cap_ansi ()
;;

let%test_unit "chat-tui renderer parity: developer label trimming" =
  let w, h = 60, 12 in
  let messages = [ "developer", "Developer: do this" ] in
  check_full_parity ~w ~h ~messages ~selected_idx:None ~auto_follow:false ~cap:cap_dumb ()
;;

let%test_unit "chat-tui renderer parity: selection of a message" =
  let w, h = 50, 14 in
  let messages =
    [ "user", "short"
    ; "assistant", "selected message should be inverted but otherwise identical"
    ; "assistant", "tail"
    ]
  in
  check_full_parity
    ~w
    ~h
    ~messages
    ~selected_idx:(Some 1)
    ~auto_follow:false
    ~cap:cap_ansi
    ()
;;

let%test_unit "chat-tui renderer parity: multi-paragraph, wrap, auto-follow" =
  let w, h = 48, 16 in
  let para = "First paragraph with enough text to wrap to another line." in
  let para2 = "\n\nSecond paragraph after a blank line." in
  let messages = [ "assistant", para ^ para2 ] in
  check_full_parity ~w ~h ~messages ~selected_idx:None ~auto_follow:true ~cap:cap_dumb ()
;;

let%test_unit "chat-tui renderer parity: scroll mid-window" =
  let w, h = 40, 10 in
  let mk_long s =
    String.concat ~sep:"\n" (List.init 20 ~f:(fun i -> s ^ Int.to_string i))
  in
  let messages = [ "assistant", mk_long "line-" ] in
  (* Build models, set an explicit scroll, then compare *)
  let m1 = mk_model ~messages ~selected_idx:None ~auto_follow:false in
  let m2 = mk_model ~messages ~selected_idx:None ~auto_follow:false in
  (* Pre-render once to populate content and total height, then set scroll *)
  let _ = R1.render_full ~size:(w, h) ~model:m1 in
  let _ = R2.render_full ~size:(w, h) ~model:m2 in
  SB.scroll_to (M.scroll_box m1) 5;
  SB.scroll_to (M.scroll_box m2) 5;
  let img1, (cx1, cy1) = R1.render_full ~size:(w, h) ~model:m1 in
  let img2, (cx2, cy2) = R2.render_full ~size:(w, h) ~model:m2 in
  [%test_eq: int] (Notty.I.width img1) (Notty.I.width img2);
  [%test_eq: int] (Notty.I.height img1) (Notty.I.height img2);
  [%test_eq: int * int] (cx1, cy1) (cx2, cy2);
  let s1 = render_to_string ~cap:cap_dumb ~w ~h img1 in
  let s2 = render_to_string ~cap:cap_dumb ~w ~h img2 in
  [%test_eq: string] s1 s2
;;
