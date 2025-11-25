open Core
module R1 = Chat_tui.Renderer
module R2 = Chat_tui.Renderer2
module M = Chat_tui.Model
module SB = Notty_scroll_box

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

let corpuses : Chat_tui.Types.message list list =
  [ [ "user", "hi"; "assistant", "hello" ]
  ; [ "assistant", "**bold** then __bold__ again, and emoji 😀😀😀 to wrap" ]
  ; [ "tool_output", "```bash\nprintf 'x'\n```\nplain text" ]
  ; [ "assistant", "pre\n```unknownlang\nx\n```\npost" ]
  ; [ "assistant", "text before\n\ntext after" ]
  ; [ "developer", "Developer: head"; "assistant", "tail" ]
  ; [ "tool", "```json\n{\n  \"a\": 1\n}\n```" ]
  ]
;;

let sizes = [ 32, 9; 48, 12; 64, 16; 80, 22 ]
let caps = [ Notty.Cap.dumb; Notty.Cap.ansi ]

let%test_unit "renderer fuzz parity across corpuses, sizes, caps, selection" =
  List.iter corpuses ~f:(fun messages ->
    List.iter sizes ~f:(fun (w, h) ->
      List.iter caps ~f:(fun cap ->
        List.iter
          [ None; Some 0; Some (List.length messages - 1) ]
          ~f:(fun selected_idx ->
            let m1 = mk_model ~messages ~selected_idx ~auto_follow:false in
            let m2 = mk_model ~messages ~selected_idx ~auto_follow:false in
            let img1, _ = R1.render_full ~size:(w, h) ~model:m1 in
            let img2, _ = R2.render_full ~size:(w, h) ~model:m2 in
            [%test_eq: int] (Notty.I.width img1) (Notty.I.width img2);
            [%test_eq: int] (Notty.I.height img1) (Notty.I.height img2);
            let s1 = render_to_string ~cap ~w ~h img1 in
            let s2 = render_to_string ~cap ~w ~h img2 in
            [%test_eq: string] s1 s2))))
;;
