# `apply_patch` – Developer documentation

This document complements the inline `odoc` comments in
[`apply_patch.{mli,ml}`](./apply_patch.mli) with a more discursive
overview, extended examples, and a few implementation notes that do
**not** belong in the API reference.

Table of contents
-----------------

1. High-level overview
2. Patch format cheat-sheet
3. Public API walk-through
4. Usage examples
5. Internals & performance notes
6. Known limitations / future work

------------------------------------------------------------------------

1  High-level overview
---------------------

`apply_patch` is a tiny interpreter for the *Ochat diff* dialect that
often appears in “coding-assistant” style conversations.  A valid patch
can:

* add new files,
* delete files,
* update existing files (with multiple hunks per file), and
* rename a file while updating it.

The module is agnostic to storage – the host application supplies three
callbacks (`open_fn`, `write_fn`, `remove_fn`) and `apply_patch`
operates exclusively through them.  This makes it trivial to unit-test
against an in-memory map or to plug into an Eio virtual file-system.


2  Patch format cheat-sheet
--------------------------

```text
*** Begin Patch
*** Update File: src/foo.ml
@@
-let foo = 1
+let foo = 42
@@
*** Move to: lib/foo.ml
*** Delete File: obsolete.txt
*** Add File: docs/usage.txt
+hello
+world
*** End Patch
```

Elements:

* The patch is bracketed by `*** Begin Patch` / `*** End Patch`.
* Each *file section* starts with one of:
  * `*** Add File: <path>`
  * `*** Delete File: <path>`
  * `*** Update File: <path>` (optionally followed by `*** Move to: <new-path>`)
* Update sections contain one or more unified-diff hunks delimited by
  `@@`.  Leading spaces are optional and line numbers are ignored –
  context is matched heuristically.


3  Public API walk-through
-------------------------

```ocaml
exception Diff_error of Apply_patch_error.t
```

Raised on the first parsing or application error.  The payload is a
rich, structured value (see {!module:Apply_patch_error}) that conveys
the exact reason of the failure together with helpful metadata such as
file paths, line numbers, and—in the case of a context mismatch—a
snippet of the conflicting section.  Use
`Apply_patch_error.to_string` for a human-readable representation or
pattern-match directly to implement custom recovery strategies.

```ocaml
val process_patch :
  text:string ->
  open_fn:(string -> string) ->
  write_fn:(string -> string -> unit) ->
  remove_fn:(string -> unit) ->
  string * (string * string) list
```

Parses and applies the patch.  The callback trio abstracts over your
storage layer (POSIX FS, Git index, in-memory map, …).  On success it
returns a tuple:

* The literal string `"Done!"` – kept for API compatibility with the
  reference implementation.
* A list of `(path, snippet)` pairs, one for every affected file.  Each
  *snippet* shows a small, **line-numbered** window around the modified
  hunks and is convenient for logging or chat-ops style confirmations.

If anything goes wrong a `Diff_error` is raised (see above).


4  Usage examples
----------------

### 4.1 Apply a patch against an in-memory map

```ocaml
open Core
open Apply_patch

let apply_in_memory ~patch_text ~files =
  let fs = ref (String.Map.of_alist_exn files) in
  let open_fn   path = Map.find_exn !fs path in
  let write_fn  path contents = fs := Map.set !fs ~key:path ~data:contents in
  let remove_fn path = fs := Map.remove !fs path in
  ignore (process_patch ~text:patch_text ~open_fn ~write_fn ~remove_fn);
  !fs

let () =
  let initial = [ "hello.txt", "hello" ] in
  let patch =
    """*** Begin Patch
*** Update File: hello.txt
@@
-hello
+hi
@@
*** End Patch""" in
  let final = apply_in_memory ~patch_text:patch ~files:initial in
  assert ([%equal: string] (Map.find_exn final "hello.txt") "hi\n")
```

### 4.2 Hook into a real file-system with Eio

```ocaml
open Eio.Std

let apply_patch_on_disk ~env patch_text =
  let cwd = Eio.Stdenv.cwd env in
  let open_fn path   = Eio.Path.(load (cwd / path)) in
  let write_fn path s =
    let file = Eio.Path.(cwd / path) in
    Eio.Path.save ~create:(`Or_truncate 0o644) file s
  in
  let remove_fn path = Eio.Path.(unlink (cwd / path)) in
  Apply_patch.process_patch ~text:patch_text ~open_fn ~write_fn ~remove_fn

let () = Eio_main.run @@ fun env ->
  let _ = apply_patch_on_disk ~env my_patch in
  ()
```


5  Internals & performance notes
--------------------------------

* **Unicode canonicalisation** – all matching happens in NFC, and a
  small table converts common “smart punctuation” to ASCII look-alikes
  (`—` → `-`, `“`/`”` → `"`, …).  This makes the algorithm resilient to
  inconsistencies that frequently arise when LLMs generate code with
  fancy quotes.

* **Fuzzy context search** – the `find_context` helper searches the
  whole file if the initial line guess fails and incrementally relaxes
  whitespace requirements.  The amount of “fuzz” encountered is
  reported back to the caller of `text_to_patch` (currently an internal
  helper).

* **Streaming parser** – the patch is processed line-by-line without
  holding additional copies in memory.  This keeps peak memory usage
  roughly at `O(|patch| + |largest_file|)`.


6  Known limitations / future work
----------------------------------

1. No support for binary diffs – all files are treated as UTF-8 text.
2. The context search is *O(n²)* in the worst case (where *n* is the
   file length) but works well in practice.  Porting the Myers diff
   algorithm would be an interesting optimisation.
3. Empty rename-only patches are not allowed – you must accompany a
   move by at least one hunk (this mirrors the reference behaviour).

---

