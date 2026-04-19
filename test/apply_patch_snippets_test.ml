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
Addition of file successful.
o:   - n:   1 | +apple
o:   - n:   2 | +banana
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

let%expect_test "success snippet delete file" =
  let initial_fs = [ "fruits.txt", String.concat_lines [ "apple"; "banana" ] ] in
  let patch =
    {|*** Begin Patch
*** Delete File: fruits.txt
*** End Patch|}
  in
  let status, snippets = apply_patch_in_memory ~patch_text:patch ~files:initial_fs in
  print_endline status;
  List.iter snippets ~f:(fun (path, snippet) -> printf ">>> %s\n%s\n" path snippet);
  [%expect
    {|
Done!
>>> fruits.txt
Deletion of file successful.
|}]
;;

let%expect_test "update snippet shows only changed lines" =
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
-h
+H
@@
*** End Patch|}
  in
  let status, snippets = apply_patch_in_memory ~patch_text:patch ~files:initial_fs in
  print_endline status;
  List.iter snippets ~f:(fun (path, snippet) -> printf ">>> %s\n%s\n" path snippet);
  [%expect
    {|    Done!
    >>> file.txt
    Update of file successful. 2 insertions, 2 deletions, 2 hunks.
    [hunk 1/2]
    o:   3 n:   3 |  c
    o:   4 n:   - | -d
    o:   - n:   - | ~ replaced by ~
    o:   - n:   4 | +D
    o:   5 n:   5 |  e
    o:   - n:   - | ... 1 unchanged line ...
    [hunk 2/2]
    o:   7 n:   7 |  g
    o:   8 n:   - | -h
    o:   - n:   - | ~ replaced by ~
    o:   - n:   8 | +H
    o:   9 n:   9 |  i
    |}]
;;

let%expect_test "update snippet includes multi-level generic anchors" =
  let initial_fs =
    [ ( "demo.py"
      , String.concat_lines
          [ "class Report:"
          ; "    def render(self, value):"
          ; "        return str(value)"
          ] )
    ]
  in
  let patch =
    {|*** Begin Patch
*** Update File: demo.py
@@
-        return str(value)
+        return f"{value + 1}"
@@
*** End Patch|}
  in
  let status, snippets = apply_patch_in_memory ~patch_text:patch ~files:initial_fs in
  print_endline status;
  List.iter snippets ~f:(fun (path, snippet) -> printf ">>> %s\n%s\n" path snippet);
  [%expect
    {|    Done!
    >>> demo.py
    Update of file successful. 1 insertion, 1 deletion, 1 hunk.
    [hunk 1/1]
    @ scope[1]: class Report:
    @ scope[2]: def render(self, value):
    o:   2 n:   2 |      def render(self, value):
    o:   3 n:   - | -        return str(value)
    o:   - n:   - | ~ replaced by ~
    o:   - n:   3 | +        return f"{value + 1}"
    |}]
;;

let%expect_test "update snippet supports typescript modifiers and methods" =
  let initial_fs =
    [ ( "demo.ts"
      , String.concat_lines
          [ "export class UserService {"
          ; "  render(total: number): string {"
          ; "    return `${total}`;"
          ; "  }"
          ; "}"
          ] )
    ]
  in
  let patch =
    {|*** Begin Patch
*** Update File: demo.ts
@@
-    return `${total}`;
+    const status = total > 40 ? "review" : "ok";
+    return `${total}:${status}`;
@@
*** End Patch|}
  in
  let status, snippets = apply_patch_in_memory ~patch_text:patch ~files:initial_fs in
  print_endline status;
  List.iter snippets ~f:(fun (path, snippet) -> printf ">>> %s\n%s\n" path snippet);
  [%expect
    {|    Done!
    >>> demo.ts
    Update of file successful. 2 insertions, 1 deletion, 1 hunk.
    [hunk 1/1]
    @ scope[1]: export class UserService {
    @ scope[2]: render(total: number): string {
    o:   2 n:   2 |    render(total: number): string {
    o:   3 n:   - | -    return `${total}`;
    o:   - n:   - | ~ replaced by ~
    o:   - n:   3 | +    const status = total > 40 ? "review" : "ok";
    o:   - n:   4 | +    return `${total}:${status}`;
    o:   4 n:   5 |    }
    |}]
;;

let%expect_test "update snippet supports java style signatures" =
  let initial_fs =
    [ ( "Demo.java"
      , String.concat_lines
          [ "public class InvoiceService {"
          ; "  public String render(int total) {"
          ; "    return Integer.toString(total);"
          ; "  }"
          ; "}"
          ] )
    ]
  in
  let patch =
    {|*** Begin Patch
*** Update File: Demo.java
@@
-    return Integer.toString(total);
+    String status = total > 40 ? "review" : "ok";
+    return total + ":" + status;
@@
*** End Patch|}
  in
  let status, snippets = apply_patch_in_memory ~patch_text:patch ~files:initial_fs in
  print_endline status;
  List.iter snippets ~f:(fun (path, snippet) -> printf ">>> %s\n%s\n" path snippet);
  [%expect
    {|    Done!
    >>> Demo.java
    Update of file successful. 2 insertions, 1 deletion, 1 hunk.
    [hunk 1/1]
    @ scope[1]: public class InvoiceService {
    @ scope[2]: public String render(int total) {
    o:   2 n:   2 |    public String render(int total) {
    o:   3 n:   - | -    return Integer.toString(total);
    o:   - n:   - | ~ replaced by ~
    o:   - n:   3 | +    String status = total > 40 ? "review" : "ok";
    o:   - n:   4 | +    return total + ":" + status;
    o:   4 n:   5 |    }
    |}]
;;

let%expect_test "update snippet supports go package and function scopes" =
  let initial_fs =
    [ ( "demo.go"
      , String.concat_lines
          [ "package billing"
          ; ""
          ; "func renderTotal(total int) string {"
          ; "    return strconv.Itoa(total)"
          ; "}"
          ] )
    ]
  in
  let patch =
    {|*** Begin Patch
*** Update File: demo.go
@@
-    return strconv.Itoa(total)
+    status := "ok"
+    if total > 40 {
+        status = "review"
+    }
+    return strconv.Itoa(total) + ":" + status
@@
*** End Patch|}
  in
  let status, snippets = apply_patch_in_memory ~patch_text:patch ~files:initial_fs in
  print_endline status;
  List.iter snippets ~f:(fun (path, snippet) -> printf ">>> %s\n%s\n" path snippet);
  [%expect
    {|    Done!
    >>> demo.go
    Update of file successful. 5 insertions, 1 deletion, 1 hunk.
    [hunk 1/1]
    @ scope[1]: package billing
    @ scope[2]: func renderTotal(total int) string {
    o:   3 n:   3 |  func renderTotal(total int) string {
    o:   4 n:   - | -    return strconv.Itoa(total)
    o:   - n:   - | ~ replaced by ~
    o:   - n:   4 | +    status := "ok"
    o:   - n:   5 | +    if total > 40 {
    o:   - n:   6 | +        status = "review"
    o:   - n:   7 | +    }
    o:   - n:   8 | +    return strconv.Itoa(total) + ":" + status
    o:   5 n:   9 |  }
    |}]
;;

let%expect_test "update snippet supports c and c++ style signatures" =
  let initial_fs =
    [ ( "demo.c"
      , String.concat_lines
          [ "int render_total(int total) {"
          ; "  return total;"
          ; "}"
          ] )
    ; ( "demo.cpp"
      , String.concat_lines
          [ "class ReportService {"
          ; "public:"
          ; "  std::string Render(int total) const {"
          ; "    return std::to_string(total);"
          ; "  }"
          ; "};"
          ] )
    ]
  in
  let patch_c =
    {|*** Begin Patch
*** Update File: demo.c
@@
-  return total;
+  int status = total > 40;
+  return total + status;
@@
*** End Patch|}
  in
  let patch_cpp =
    {|*** Begin Patch
*** Update File: demo.cpp
@@
-    return std::to_string(total);
+    std::string status = total > 40 ? "review" : "ok";
+    return std::to_string(total) + ":" + status;
@@
*** End Patch|}
  in
  let status_c, snippets_c = apply_patch_in_memory ~patch_text:patch_c ~files:initial_fs in
  print_endline status_c;
  List.iter snippets_c ~f:(fun (path, snippet) -> printf ">>> %s\n%s\n" path snippet);
  let status_cpp, snippets_cpp = apply_patch_in_memory ~patch_text:patch_cpp ~files:initial_fs in
  print_endline status_cpp;
  List.iter snippets_cpp ~f:(fun (path, snippet) -> printf ">>> %s\n%s\n" path snippet);
  [%expect
    {|Done!
>>> demo.c
Update of file successful. 2 insertions, 1 deletion, 1 hunk.
[hunk 1/1]
@ scope[1]: int render_total(int total) {
o:   1 n:   1 |  int render_total(int total) {
o:   2 n:   - | -  return total;
o:   - n:   - | ~ replaced by ~
o:   - n:   2 | +  int status = total > 40;
o:   - n:   3 | +  return total + status;
o:   3 n:   4 |  }
Done!
>>> demo.cpp
Update of file successful. 2 insertions, 1 deletion, 1 hunk.
[hunk 1/1]
@ scope[1]: class ReportService {
@ scope[2]: std::string Render(int total) const {
o:   3 n:   3 |    std::string Render(int total) const {
o:   4 n:   - | -    return std::to_string(total);
o:   - n:   - | ~ replaced by ~
o:   - n:   4 | +    std::string status = total > 40 ? "review" : "ok";
o:   - n:   5 | +    return std::to_string(total) + ":" + status;
o:   5 n:   6 |    }
|}]
;;

let%expect_test "update snippet supports stacked modifiers in containers" =
  let initial_fs =
    [ ( "stacked.ts"
      , String.concat_lines
          [ "export default class MetricsService {"
          ; "  render(total: number): string {"
          ; "    return `${total}`;"
          ; "  }"
          ; "}"
          ] )
    ]
  in
  let patch =
    {|*** Begin Patch
*** Update File: stacked.ts
@@
-    return `${total}`;
+    const status = total > 40 ? "review" : "ok";
+    return `${total}:${status}`;
@@
*** End Patch|}
  in
  let status, snippets = apply_patch_in_memory ~patch_text:patch ~files:initial_fs in
  print_endline status;
  List.iter snippets ~f:(fun (path, snippet) -> printf ">>> %s\n%s\n" path snippet);
  [%expect
    {|    Done!
    >>> stacked.ts
    Update of file successful. 2 insertions, 1 deletion, 1 hunk.
    [hunk 1/1]
    @ scope[1]: export default class MetricsService {
    @ scope[2]: render(total: number): string {
    o:   2 n:   2 |    render(total: number): string {
    o:   3 n:   - | -    return `${total}`;
    o:   - n:   - | ~ replaced by ~
    o:   - n:   3 | +    const status = total > 40 ? "review" : "ok";
    o:   - n:   4 | +    return `${total}:${status}`;
    o:   4 n:   5 |    }
    |}]
;;
