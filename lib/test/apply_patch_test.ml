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
