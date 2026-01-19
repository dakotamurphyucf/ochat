open Core
module H = Chat_tui.Highlight_theme
module R = Chat_tui.Renderer
module M = Chat_tui.Model
module SB = Notty_scroll_box

let time_s f =
  let start = Time_float.now () in
  let () = f () in
  let stop = Time_float.now () in
  Time_float.Span.to_sec (Time_float.diff stop start)
;;

let micro_scope_sets : string list list =
  [ []
  ; [ "keyword" ]
  ; [ "keyword"; "source.ocaml" ]
  ; [ "keyword.operator"; "source.ocaml" ]
  ; [ "string"; "source.ocaml" ]
  ; [ "markup.heading.markdown"; "source.gfm" ]
  ; [ "markup.heading.setext.1.markdown"; "source.gfm" ]
  ; [ "variable.other.shell"; "source.shell" ]
  ; [ "entity.name.keyword.echo.shell"; "source.shell" ]
  ; [ "comment.line.number-sign.shell"; "source.shell" ]
  ; [ "keyword"; "keyword.operator"; "source.ocaml" ]
  ; [ "constant.numeric"; "constant"; "source.ocaml" ]
  ]
;;

let run_microbench () =
  let theme = H.github_dark in
  let iterations = 50_000 in
  let calls_per_iter = List.length micro_scope_sets in
  let total_calls = iterations * calls_per_iter in
  let trie_time =
    time_s (fun () ->
      for _ = 1 to iterations do
        List.iter micro_scope_sets ~f:(fun scopes ->
          let (_ : Notty.A.t) = H.attr_of_scopes theme ~scopes in
          ())
      done)
  in
  let per_call_trie = trie_time /. Float.of_int total_calls in
  printf
    "Microbench: github_dark, %d scope sets, %d iterations (%d total calls)\n"
    calls_per_iter
    iterations
    total_calls;
  printf "  trie+cache  : %.6fs total, %.3fus / call\n" trie_time (per_call_trie *. 1e6)
;;

let mk_model ~(messages : Chat_tui.Types.message list) ~selected_idx ~auto_follow : M.t =
  let history_items = [] in
  let input_line = "" in
  let msg_buffers = Hashtbl.create (module String) in
  let function_name_by_id = Hashtbl.create (module String) in
  let reasoning_idx_by_id = Hashtbl.create (module String) in
  let tool_output_by_index = Hashtbl.create (module Int) in
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
    ~tool_output_by_index
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

let run_render_bench () =
  let w, h = 80, 24 in
  let iterations = 200 in
  let cap = Notty.Cap.ansi in
  printf
    "Render bench: %d corpuses, size=%dx%d, %d iterations each\n"
    (List.length corpuses)
    w
    h
    iterations;
  List.iteri corpuses ~f:(fun idx messages ->
    let model = mk_model ~messages ~selected_idx:None ~auto_follow:false in
    let (_ : Notty.I.t), (_ : int * int) = R.render_full ~size:(w, h) ~model in
    let buf = Buffer.create 4096 in
    let total_time =
      time_s (fun () ->
        for _ = 1 to iterations do
          let img, (_ : int * int) = R.render_full ~size:(w, h) ~model in
          let () = Buffer.clear buf in
          let () = Notty.Render.to_buffer buf cap (0, 0) (w, h) img in
          ()
        done)
    in
    let per_iter = total_time /. Float.of_int iterations in
    printf
      "  corpus %d (messages=%d): %.6fs total, %.3fms / frame\n"
      (idx + 1)
      (List.length messages)
      total_time
      (per_iter *. 1e3))
;;

let () =
  run_microbench ();
  let () = Out_channel.newline stdout in
  run_render_bench ()
;;
