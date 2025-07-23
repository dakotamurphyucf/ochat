# `Markdown_crawler`

Walk a regular directory tree, pick up hand-written Markdown files and feed
them to user-supplied code.  The module is the *source* component of the
Markdown indexing pipeline described in `markdown_indexing_plan.md`.

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
* **Size cap**      – files larger than **10 MiB** are skipped to avoid memory
  exhaustion.
* **Bounded concurrency** – traversal uses
  `Eio.Fiber.List.iter ~max_fibers:25` for predictable throughput.

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

* **Best-effort** – unreadable paths and all non-fatal I/O problems are logged
  via `Log.emit` and silently skipped.
* **Exception safety** – only exceptions raised *by the user callback* escape
  `crawl`; everything else is caught inside the helper.

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

