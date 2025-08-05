# `Apply_patch_error` – Diagnostic payloads for patch failures

This document complements the inline `odoc` comments in
[`apply_patch_error.mli`](../../lib/apply_patch_error.mli) with a
broader discussion, usage examples, and implementation notes that do
**not** belong in the API reference.

Table of contents
-----------------

1. High-level overview  
2. Variant walk-through  
3. Working with `Diff_error`  
4. Example – graceful error handling in Eio  
5. Known limitations / future work

------------------------------------------------------------------------

1  High-level overview
---------------------

`Apply_patch_error.t` is the *structured* counterpart to the
plain-string diagnostics emitted by tools such as `git apply`.  When
the high-level helper

```ocaml
Apply_patch.process_patch :
  text:string ->
  open_fn:(string -> string) ->
  write_fn:(string -> string -> unit) ->
  remove_fn:(string -> unit) ->
  string * (string * string) list
```

encounters a problem it raises

```ocaml
exception Apply_patch_error.Diff_error of Apply_patch_error.t
```

The *payload* carries enough metadata for an IDE or CLI to pinpoint
the issue precisely – file paths, line numbers, context snippets, … –
and enables programmatic recovery strategies such as automatically
retrying with relaxed fuzzing or falling back to a manual merge.

------------------------------------------------------------------------

2  Variant walk-through
----------------------

| Constructor            | Meaning & typical trigger |
|------------------------|----------------------------|
| `Syntax_error`         | The patch text is **not valid** Ochat diff. Examples: missing `*** End Patch`, malformed hunk headers, lines that start with `-`/`+` but no following space. |
| `Missing_file`         | The diff references a file that is **absent** from the workspace.  The [`action`] field indicates whether the patch tried to *update* or *delete* it. |
| `File_exists`          | An `*** Add File:` section would **overwrite** an existing path. |
| `Context_mismatch`     | Fuzzy matching failed to find the hunk’s context in the target file.  The error embeds the *expected* lines, a *snippet* of the real file around the attempted location, and a `fuzz` score accumulated during increasingly relaxed matching passes. |
| `Bounds_error`         | A chunk’s index or length is **out of bounds** for the destination file – usually a logic error in the upstream diff generator. |

See the source code for the exact field layout.

------------------------------------------------------------------------

3  Working with `Diff_error`
---------------------------

`to_string` converts any `t` to a human-friendly message:

```ocaml
let () =
  try
    let (_result, _snippets) = Apply_patch.process_patch ~text ~open_fn ~write_fn ~remove_fn in
    print_endline "Patch applied successfully"
  with
  | Apply_patch_error.Diff_error err ->
    eprintf "%s\n%!" (Apply_patch_error.to_string err)
```

Because the exception carries the *typed* value you may wish to
pattern-match instead:

```ocaml
| Apply_patch_error.Diff_error (Context_mismatch { path; snippet; _ }) ->
    show_diff_in_ui ~file:path ~snippet
```

------------------------------------------------------------------------

4  Example – graceful error handling in Eio
-----------------------------------------

Below is a minimal wrapper that applies a patch against the current
working directory and prints colourised diagnostics when something
goes wrong.

```ocaml
open Eio.Std

let apply_with_feedback ~env patch_text =
  let cwd = Eio.Stdenv.cwd env in
  let open_fn   path = Eio.Path.(load (cwd / path)) in
  let write_fn  path contents =
    let file = Eio.Path.(cwd / path) in
    Eio.Path.save ~create:(`Or_truncate 0o644) file contents
  in
  let remove_fn path = Eio.Path.(unlink (cwd / path)) in
  try
    let (_msg, _snippets) =
      Apply_patch.process_patch ~text:patch_text ~open_fn ~write_fn ~remove_fn
    in
    Ok ()
  with
  | Apply_patch_error.Diff_error err ->
    (* The helper already yields plain text – colourise it with ANSI codes. *)
    let red s = "\027[31m" ^ s ^ "\027[0m" in
    Eio.traceln "%s" (red (Apply_patch_error.to_string err));
    Error ()

let () = Eio_main.run @@ fun env ->
  match apply_with_feedback ~env my_patch_text with
  | Ok () -> ()
  | Error () -> exit 1
```

------------------------------------------------------------------------

5  Known limitations / future work
----------------------------------

1. The type is **non-extensible**.  If additional failure modes are
   discovered the library will need a minor version bump.
2. The [`snippet`] field in `Context_mismatch` is limited to *±3*
   lines around the faulty section.  A smarter selection (e.g. based
   on syntax highlighting or diff granularity) would improve the UX in
   very large files.

---

