open Core

let%expect_test "success snippet add file" =
  let patch =
    {|*** Begin Patch
*** Add File: fruits.txt
+apple
+banana
*** End Patch|}
  in
  let status, snippets =
    Apply_patch.process_patch
      ~text:patch
      ~open_fn:(fun _ -> "")
      ~write_fn:(fun _ _ -> ())
      ~remove_fn:(fun _ -> ())
  in
  print_endline status;
  List.iter snippets ~f:(fun (path, snippet) -> printf ">>> %s\n%s\n" path snippet);
  [%expect
    {|
Done!
>>> fruits.txt
   1 | +apple
   2 | +banana
|}]
;;

(* ----------------------------------------------------------------------------- *)

let apply_patch_in_memory ~patch_text ~files =
  let fs = ref (String.Map.of_alist_exn files) in
  let open_fn path = Map.find_exn !fs path in
  let write_fn path s = fs := Map.set !fs ~key:path ~data:s in
  let remove_fn path = fs := Map.remove !fs path in
  Apply_patch.process_patch ~text:patch_text ~open_fn ~write_fn ~remove_fn
;;

let%expect_test "condensed update snippet" =
  let initial_fs =
    [ "file.txt", String.concat_lines [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i" ] ]
  in
  let patch =
    {|*** Begin Patch
*** Update File: file.txt
@@
-d
+D
@@
@@
-e
+E
@@
*** End Patch|}
  in
  let status, snippets = apply_patch_in_memory ~patch_text:patch ~files:initial_fs in
  print_endline status;
  List.iter snippets ~f:(fun (path, snippet) -> printf ">>> %s\n%s\n" path snippet);
  [%expect
    {|    Done!
    >>> file.txt
       4 | +D
       5 | +E
    |}]
;;
