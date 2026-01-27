open Core
open Apply_patch

(* A tiny in-memory “file-system” ------------------------------------------------ *)

let apply_patch_in_memory ~patch_text ~files =
  let fs = ref (String.Map.of_alist_exn files) in
  let open_fn path = Map.find_exn !fs path in
  let write_fn path s = fs := Map.set !fs ~key:path ~data:s in
  let remove_fn path = fs := Map.remove !fs path in
  ignore (process_patch ~text:patch_text ~open_fn ~write_fn ~remove_fn);
  !fs
;;

(* ----------------------------------------------------------------------------- *)

let%expect_test "section break line *** terminates a hunk (should be accepted)" =
  let initial_fs = [ "f.txt", "a\nb\nc" ] in
  let patch =
    {|*** Begin Patch
*** Update File: f.txt
@@
-a
+A
***
*** End Patch|}
  in
  let final_fs = apply_patch_in_memory ~patch_text:patch ~files:initial_fs in
  print_s [%sexp (Map.to_alist final_fs : (string * string) list)];
  [%expect
    {|
    ((f.txt  "A\
            \nb\
            \nc\
            \n"))
    |}]
;;

let%expect_test "add file: empty raw line is rejected with syntax error (no crash)" =
  let patch =
    {|*** Begin Patch
*** Add File: new.txt
+hello

+world
*** End Patch|}
  in
  (try
     ignore (apply_patch_in_memory ~patch_text:patch ~files:[]);
     print_endline "should fail"
   with
   | Apply_patch.Diff_error err -> print_endline (Apply_patch.error_to_string err));
  [%expect {| Syntax error at line 4: |}]
;;

let%expect_test
    "wrapped continuation line in a hunk (missing diff prefix) is tolerated (no hang) \
     but results in a context mismatch"
  =
  let initial_fs = [ "w.txt", "alpha\nbeta\ngamma\n" ] in
  let patch =
    {|*** Begin Patch
*** Update File: w.txt
@@
-beta
+BE
TA
@@
*** End Patch|}
  in
  (try
     ignore (apply_patch_in_memory ~patch_text:patch ~files:initial_fs);
     print_endline "should fail"
   with
   | Apply_patch.Diff_error err -> print_endline (Apply_patch.error_to_string err));
  [%expect {| should fail |}]
;;

let%expect_test
    "missing-prefix continuation after + is treated as insertion continuation (applies)"
  =
  let initial_fs = [ "c.txt", "alpha\nbeta\ngamma\n" ] in
  let patch =
    {|*** Begin Patch
*** Update File: c.txt
@@
-beta
+BE
TA
@@
*** End Patch|}
  in
  let final_fs = apply_patch_in_memory ~patch_text:patch ~files:initial_fs in
  print_s [%sexp (Map.to_alist final_fs : (string * string) list)];
  [%expect
    {|
    ((c.txt  "alpha\
            \nBE\
            \nTA\
            \ngamma\
            \n"))
    |}]
;;

let%expect_test "debug trace: renderer_page_chat patch parse failure repro" =
  Apply_patch.set_debug true;
  let initial_file =
    String.concat_lines
      [ "module Compose = struct"
      ; "  let top_visible_index"
      ; "        ~(model : Model.t)"
      ; "        ~(scroll_height : int)"
      ; "        ~(messages : message list)"
      ; "    : int option"
      ; "    ="
      ; "    let len = List.length messages in"
      ; "    if Int.equal len 0"
      ; "    then None"
      ; "    else ("
      ; "      let prefix = Model.height_prefix model in"
      ; "      if Array.length prefix < len + 1"
      ; "      then None"
      ; "      else ("
      ; "        let total_height = prefix.(len) in"
      ; "        let max_scroll = Int.max 0 (total_height - scroll_height) in"
      ; "        let scroll ="
      ; "          if Model.auto_follow model"
      ; "          then max_scroll"
      ; "          else ("
      ; "            let s = Notty_scroll_box.scroll (Model.scroll_box model) in"
      ; "            Int.max 0 (Int.min s max_scroll))"
      ; "        in"
      ; "        let bsearch_first_gt arr ~len ~target ="
      ; "          let lo = ref 0 in"
      ; "          let hi = ref len in"
      ; "          while !lo < !hi do"
      ; "            let mid = (!lo + !hi) lsr 1 in"
      ; "            if arr.(mid) <= target then lo := mid + 1 else hi := mid"
      ; "          done;"
      ; "          !lo"
      ; "        in"
      ; "        let k = bsearch_first_gt prefix ~len:(len + 1) ~target:scroll in"
      ; "        let idx = Int.max 0 (k - 1) in"
      ; "        if idx >= len"
      ; "        then None"
      ; "        else ("
      ; "          let message_start_y = prefix.(idx) in"
      ; "          let header_y = message_start_y + 1 in"
      ; "          let header_vpos = header_y - scroll in"
      ; "          let exclusion_band = 2 in"
      ; "          if header_vpos >= 0 && header_vpos < exclusion_band then None else \
         Some idx)))"
      ; "  ;;"
      ; ""
      ; "  let render_full ~(size : int * int) ~(model : Model.t) : I.t * (int * int) ="
      ; "    let messages = Model.messages model in"
      ; "    let history_img ="
      ; "      Viewport.render"
      ; "        ~model"
      ; "        ~width:w"
      ; "        ~height:scroll_height"
      ; "        ~messages"
      ; "        ~selected_idx:(Model.selected_msg model)"
      ; "        ~render_message"
      ; "    in"
      ; "    Notty_scroll_box.set_content (Model.scroll_box model) history_img;"
      ; "    if Model.auto_follow model"
      ; "    then Notty_scroll_box.scroll_to_bottom (Model.scroll_box model) \
         ~height:scroll_height;"
      ; "    let top_visible_idx = top_visible_index ~model ~scroll_height ~messages in"
      ; "    let scroll_view ="
      ; "      Notty_scroll_box.render (Model.scroll_box model) ~width:w \
         ~height:scroll_height"
      ; "    in"
      ]
  in
  let initial_fs = [ "lib/chat_tui/renderer_page_chat.ml", initial_file ] in
  let patch =
    {|*** Begin Patch
*** Update File: lib/chat_tui/renderer_page_chat.ml
@@
-module Compose = struct
-  let top_visible_index
-        ~(model : Model.t)
-        ~(scroll_height : int)
-        ~(messages : message list)
-    : int option
-    =
-    let len = List.length messages in
-    if Int.equal len 0
-    then None
-    else (
-      let prefix = Model.height_prefix model in
-      if Array.length prefix < len + 1
-      then None
-      else (
-        let total_height = prefix.(len) in
-        let max_scroll = Int.max 0 (total_height - scroll_height) in
-        let scroll =
-          if Model.auto_follow model
-          then max_scroll
-          else (
-            let s = Notty_scroll_box.scroll (Model.scroll_box model) in
-            Int.max 0 (Int.min s max_scroll))
-        in
-        let bsearch_first_gt arr ~len ~target =
-          let lo = ref 0 in
-          let hi = ref len in
-          while !lo < !hi do
-            let mid = (!lo + !hi) lsr 1 in
-            if arr.(mid) <= target then lo := mid + 1 else hi := mid
-          done;
-          !lo
-        in
-        let k = bsearch_first_gt prefix ~len:(len + 1) ~target:scroll in
-        let idx = Int.max 0 (k - 1) in
-        if idx >= len
-        then None
-        else (
-          let message_start_y = prefix.(idx) in
-          let header_y = message_start_y + 1 in
-          let header_vpos = header_y - scroll in
-          let exclusion_band = 2 in
-          if header_vpos >= 0 && header_vpos < exclusion_band then None else Some idx)))
-  ;;
-
-  let render_full ~(size : int * int) ~(model : Model.t) : I.t * (int * int) =
+ module Compose = struct
+
+   let render_full ~(size : int * int) ~(model : Model.t) : I.t * (int * int) =
@@
-    let messages = Model.messages model in
-    let history_img =
-      Viewport.render
-        ~model
-        ~width:w
-        ~height:scroll_height
-        ~messages
-        ~selected_idx:(Model.selected_msg model)
-        ~render_message
-    in
-    Notty_scroll_box.set_content (Model.scroll_box model) history_img;
-    if Model.auto_follow model
-    then Notty_scroll_box.scroll_to_bottom (Model.scroll_box model) ~height:scroll_height;
-    let top_visible_idx = top_visible_index ~model ~scroll_height ~messages in
-    let scroll_view =
-      Notty_scroll_box.render (Model.scroll_box model) ~width:w ~height:scroll_height
-    in
+     let messages = Model.messages model in
+     let history_img =
+       Renderer_component_history.render
+         ~model
+         ~width:w
+         ~height:scroll_height
+         ~messages
+         ~selected_idx:(Model.selected_msg model)
+         ~render_message
+     in
+     Notty_scroll_box.set_content (Model.scroll_box model) history_img;
+     if Model.auto_follow model
+     then Notty_scroll_box.scroll_to_bottom (Model.scroll_box model) ~height:scroll_height;
+     let top_visible_idx =
+       Renderer_component_history.top_visible_index ~model ~scroll_height ~messages
+     in
+     let scroll_view =
+       Notty_scroll_box.render (Model.scroll_box model) ~width:w ~height:scroll_height
+     in
*** End Patch|}
  in
  (try
     ignore (apply_patch_in_memory ~patch_text:patch ~files:initial_fs);
     print_endline "applied"
   with
   | Apply_patch.Diff_error err -> print_endline (Apply_patch.error_to_string err));
  Apply_patch.set_debug false;
  [%expect {| applied |}]
;;

(* ----------------------------------------------------------------------------- *)

let%expect_test "context mismatch update" =
  let initial_fs = [ "ctx.txt", "foo\nbar\nbaz" ] in
  let patch =
    {|*** Begin Patch
*** Update File: ctx.txt
@@
-old
+new
@@
*** End Patch|}
  in
  (try
     ignore (apply_patch_in_memory ~patch_text:patch ~files:initial_fs);
     print_endline "should fail"
   with
   | Apply_patch.Diff_error err -> print_endline (Apply_patch.error_to_string err));
  [%expect
    {|
    Context mismatch (fuzz=0) in ctx.txt:
    --- expected ---
    old
    --- actual ---
    foo
    bar
    baz
    |}]
;;

(* ----------------------------------------------------------------------------- *)

let%expect_test "update file missing" =
  let patch =
    {|*** Begin Patch
*** Update File: missing.txt
@@
-old
+new
@@
*** End Patch|}
  in
  (try ignore (apply_patch_in_memory ~patch_text:patch ~files:[]) with
   | Apply_patch.Diff_error err -> print_endline (Apply_patch.error_to_string err));
  [%expect {|Update file missing: missing.txt|}]
;;

(* ----------------------------------------------------------------------------- *)

let%expect_test "missing end patch" =
  let patch =
    {|*** Begin Patch
*** Add File: new.txt
+hello|}
  in
  (try
     ignore (apply_patch_in_memory ~patch_text:patch ~files:[]);
     print_endline "should fail"
   with
   | Apply_patch.Diff_error err -> print_endline (Apply_patch.error_to_string err));
  [%expect
    {|
    Syntax error at line 0:
    Invalid patch text
    |}]
;;

(* ----------------------------------------------------------------------------- *)

let%expect_test "rename file with update" =
  let initial_fs = [ "src.txt", "line1\nline2\nline3" ] in
  let patch =
    {|*** Begin Patch
*** Update File: src.txt
*** Move to: dest.txt
@@
-line1
+first
@@
*** End Patch|}
  in
  let final_fs = apply_patch_in_memory ~patch_text:patch ~files:initial_fs in
  print_s [%sexp (Map.to_alist final_fs : (string * string) list)];
  [%expect
    {|
    ((dest.txt  "first\
               \nline2\
               \nline3\
               \n"))
    |}]
;;

(* ----------------------------------------------------------------------------- *)

let%expect_test "multiple update chunks (double @@ separator)" =
  let initial_fs = [ "multi.txt", "alpha\nbeta\ngamma\ndelta" ] in
  let patch =
    {|*** Begin Patch
*** Update File: multi.txt
@@
-beta
+B
@@
@@
-delta
+D
@@
*** End Patch|}
  in
  let final_fs = apply_patch_in_memory ~patch_text:patch ~files:initial_fs in
  print_s [%sexp (Map.to_alist final_fs : (string * string) list)];
  [%expect
    {|
    ((multi.txt  "alpha\
                \nB\
                \ngamma\
                \nD\
                \n"))
    |}]
;;

let%expect_test "multiple update chunks (single @@ separator)" =
  let initial_fs = [ "multi.txt", "alpha\nbeta\ngamma\ndelta" ] in
  let patch =
    {|*** Begin Patch
*** Update File: multi.txt
@@
-beta
+B
@@
-delta
+D
@@
*** End Patch|}
  in
  let final_fs = apply_patch_in_memory ~patch_text:patch ~files:initial_fs in
  print_s [%sexp (Map.to_alist final_fs : (string * string) list)];
  [%expect
    {|
    ((multi.txt  "alpha\
                \nB\
                \ngamma\
                \nD\
                \n"))
    |}]
;;

(* ----------------------------------------------------------------------------- *)

let%expect_test "add file only" =
  let initial_fs = [] in
  let patch =
    {|*** Begin Patch
*** Add File: new.txt
+hello
+world
*** End Patch|}
  in
  let final_fs = apply_patch_in_memory ~patch_text:patch ~files:initial_fs in
  print_s [%sexp (Map.to_alist final_fs : (string * string) list)];
  [%expect
    {|
    ((new.txt  "hello\
              \nworld\
              \n"))
    |}]
;;

(* ----------------------------------------------------------------------------- *)

let%expect_test "delete file only" =
  let initial_fs = [ "del.txt", "something"; "keep.txt", "ok" ] in
  let patch =
    {|*** Begin Patch
*** Delete File: del.txt
*** End Patch|}
  in
  let final_fs = apply_patch_in_memory ~patch_text:patch ~files:initial_fs in
  print_s [%sexp (Map.to_alist final_fs : (string * string) list)];
  [%expect {| ((keep.txt ok)) |}]
;;

(* ----------------------------------------------------------------------------- *)

let%expect_test "insert-only update" =
  let initial_fs = [ "insert.txt", "a\nb\nc" ] in
  let patch =
    {|*** Begin Patch
*** Update File: insert.txt
@@
+intro
@@
*** End Patch|}
  in
  let final_fs = apply_patch_in_memory ~patch_text:patch ~files:initial_fs in
  print_s [%sexp (Map.to_alist final_fs : (string * string) list)];
  [%expect
    {|
    ((insert.txt  "intro\
                 \na\
                 \nb\
                 \nc\
                 \n"))
    |}]
;;

(* ----------------------------------------------------------------------------- *)

let%expect_test "add, update and delete files" =
  let initial_fs =
    [ "foo.txt", "apple\nworld\norange"; "bar.txt", "alpha\nbeta\ngamma" ]
  in
  let patch =
    {|*** Begin Patch
*** Update File: foo.txt
@@
-world
+Earth
@@
*** Delete File: bar.txt
*** Add File: baz.txt
+First line
+Second line
*** End Patch
		|}
  in
  let final_fs = apply_patch_in_memory ~patch_text:patch ~files:initial_fs in
  print_s [%sexp (Map.to_alist final_fs : (string * string) list)];
  [%expect
    {|
    ((baz.txt  "First line\
              \nSecond line\
              \n")
     (foo.txt  "apple\
              \nEarth\
              \norange\
              \n"))
    |}]
;;

(* ----------------------------------------------------------------------------- *)
