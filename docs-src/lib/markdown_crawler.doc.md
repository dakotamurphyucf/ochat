# `Markdown_crawler`

Walk a directory tree, pick out hand-written Markdown files, and stream them
to user code.  The module is the *source* component of the Markdown indexing
pipeline described in the [search guide](../guide/search-and-indexing.md).

```
root/
├─ README.md           (✓)
├─ doc/guide.md        (✓)
├─ .gitignore          (# add your own patterns)
└─ _build/artefacts.md (✗ – ignored by fallback block-list)
```

Features

* **Ignore rules** – consults the nearest `.gitignore` (root-level only) and a
  static deny-list containing `_build/`, `dist/`, `node_modules/`, …​.
* **File filter**   – accepts basenames ending in one of
  `".md"`, `".markdown"`, `".mdown"`.
* **Size filter** – files larger than **10 MiB** are skipped after a full read.
  This does not bound peak read memory. Empty files are also skipped.
* **Per-directory concurrency** – traversal uses
  `Eio.Fiber.List.iter ~max_fibers:25` in each recursive directory call.
  This is not a global 25-file/fiber limit.

---

## API

```ocaml
val crawl :
  root:_ Eio.Path.t ->
  f:(doc_path:string -> markdown:string -> unit) ->
  unit
```

### Parameters

* `root` – directory that will be scanned recursively.
* `f` – callback invoked for **every** Markdown document found.  The arguments
  are:
    * `doc_path` – path *relative to* `root` (POSIX separators).
    * `markdown` – UTF-8 content of the file.

### Guarantees

* Failed metadata/file reads are skipped silently; a failed root `.gitignore`
  read is treated as no additional rules. Directory enumeration errors escape.
* Callback and logging errors propagate. Only selected metadata/read operations
  are wrapped, and those broad wrappers also catch cancellation exceptions.
  Do not assume this is a fully cancellation-transparent traversal.
* Callback order is not deterministic and callbacks can overlap across fibers.
  Markdown is supplied as bytes; UTF-8 validity is not checked.

---

## Usage example

```ocaml
open Eio.Std

let () =
  Eio_main.run @@ fun env ->
  let root = Eio.Path.(env#fs / "docs") in
  Markdown_crawler.crawl ~root ~f:(fun ~doc_path ~markdown ->
    Printf.printf "• %s – %d bytes\n" doc_path (String.length markdown))
```

---

## Limitations & future work

* Only the *root* `.gitignore` is parsed.  Nested ignore files are ignored for
  performance reasons.
* `.gitattributes` and other VCS ignore files are not supported.
* The fallback block-list is heuristic; adapt it to your repository layout if
  necessary.
* Symlinks are followed and there is no visited-directory/cycle guard. Crawl
  trusted trees without cycles; this API does not enforce confinement beneath
  the logical root. Eio's supplied filesystem capability remains the boundary.
* Negated Git ignore rules are not implemented as ordered re-inclusions. This
  is best-effort matching, not Git's complete ignore semantics.
