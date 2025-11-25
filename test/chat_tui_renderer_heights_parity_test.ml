open Core
module R1 = Chat_tui.Renderer
module R2 = Chat_tui.Renderer2
module M = Chat_tui.Model
module SB = Notty_scroll_box

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

let arrays_equal_int a b =
  let la = Array.length a
  and lb = Array.length b in
  Int.equal la lb
  &&
  let rec loop i = i = la || (Int.equal a.(i) b.(i) && loop (i + 1)) in
  loop 0
;;

let%test_unit "heights/prefix parity across sizes and corpuses" =
  let corpuses : Chat_tui.Types.message list list =
    [ [ "user", "hi"; "assistant", "hello" ]
    ; [ "assistant", "para 1 with enough text to wrap across widths and some emoji 😀😀" ]
    ; [ "tool_output", "```bash\necho hi\n```\ntrailing text" ]
    ; [ "assistant", "text before\n```unknownlang\nabc\n```\ntext after" ]
    ; [ "developer", "Developer: label"; "assistant", "tail" ]
    ]
  in
  let sizes = [ 30, 10; 50, 12; 80, 20 ] in
  List.iter corpuses ~f:(fun messages ->
    List.iter sizes ~f:(fun (w, h) ->
      let m1 = mk_model ~messages ~selected_idx:None ~auto_follow:false in
      let m2 = mk_model ~messages ~selected_idx:None ~auto_follow:false in
      let _ = R1.render_full ~size:(w, h) ~model:m1 in
      let _ = R2.render_full ~size:(w, h) ~model:m2 in
      [%test_eq: int] (Array.length (M.msg_heights m1)) (Array.length (M.msg_heights m2));
      [%test_eq: int]
        (Array.length (M.height_prefix m1))
        (Array.length (M.height_prefix m2));
      [%test_eq: int list]
        (Array.to_list (M.msg_heights m1))
        (Array.to_list (M.msg_heights m2));
      [%test_eq: int list]
        (Array.to_list (M.height_prefix m1))
        (Array.to_list (M.height_prefix m2))))
;;
