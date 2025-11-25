open! Core

type t =
  | Syntax_error of
      { line : int
      ; text : string
      }
  | Missing_file of
      { path : string
      ; action : [ `Update | `Delete ]
      }
  | File_exists of { path : string }
  | Context_mismatch of
      { path : string
      ; expected : string list
      ; fuzz : int
      ; snippet : string list
      }
  | Bounds_error of
      { path : string
      ; index : int
      ; len : int
      }

exception Diff_error of t

let _tips =
  {|
Tips:
- remember that Add file still requires a + at the start of each new line
- there needs to be a space after the + or - at the start of each line
- If you are trying to use the @@ syntax for unique search context remember that you must put the context line on the same line as
the @@ with a space after the @@
correct syntax:
@@ module My_module
[3 lines of pre-context]
- [old_code]
+ [new_code]
[3 lines of post-context]
incorrect syntax:
@@
module My_module
[3 lines of pre-context]
- [old_code]
+ [new_code]
[3 lines of post-context]
|}
;;

let to_string =
  let join = String.concat ~sep:"\n" in
  function
  | Syntax_error { line; text } -> sprintf "Syntax error at line %d:\n%s" line text
  | Missing_file { path; action } ->
    (match action with
     | `Update -> sprintf "Update file missing: %s" path
     | `Delete -> sprintf "Delete file missing: %s" path)
  | File_exists { path } -> sprintf "Add file already exists: %s" path
  | Context_mismatch { path; expected; fuzz; snippet } ->
    let expected_block = join expected in
    let snippet_block = join snippet in
    sprintf
      "Context mismatch (fuzz=%d) in %s:\n--- expected ---\n%s\n--- actual ---\n%s"
      fuzz
      path
      expected_block
      snippet_block
  | Bounds_error { path; index; len } ->
    sprintf "%s: index %d out of bounds (len=%d)" path index len
;;
