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

let%expect_test "multiple update chunks" =
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
  [%expect {|
    ((new.txt  "hello\
              \nworld\
              \n"))
    |}]
;;

(* ----------------------------------------------------------------------------- *)

let%expect_test "delete file only" =
  let initial_fs = [ "del.txt", "something"; "keep.txt", "ok" ] in
  let patch = {|*** Begin Patch
*** Delete File: del.txt
*** End Patch|} in
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
  [%expect {|
    ((insert.txt  "intro\
                 \na\
                 \nb\
                 \nc\
                 \n"))
    |}]
;;
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

