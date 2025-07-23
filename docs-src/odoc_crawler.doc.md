# `Odoc_crawler`

Traverse a directory tree produced by [`odoc`] or `dune build @doc`, convert
HTML pages to Markdown and hand them to user-defined code.

```
└─ /my/project/_build/default/_doc/_html/
   ├─ core/              <-- package        (opam package "core")
   │  ├─ Core/           <-- modules, functors, … (HTML)
   │  └─ _doc-dir/       <-- misc. assets
   │      └─ README.md   <-- package README (Markdown)
   └─ eio/
      └─ …
```

The root passed to `Odoc_crawler.crawl` is expected to be the `_html` folder
shown above.  Each first-level directory is interpreted as an **opam package**.

The crawler looks for two different kinds of documents:

* **HTML pages** (module documentation) – every file whose name ends in
  `.html`.  Before the page is reported to the caller it is converted to
  Markdown using `Webpage_markdown.Html_to_md`.  Pages whose body contains the
  string `"This module is hidden."` are skipped because they do not belong to
  the public API.

* **README files** – inside an `_doc-dir` subtree a file called `README`,
  `README.md`, `README.markdown`, …​ is considered the package README.  The
  file is forwarded *unmodified*.

For every document discovered the user-supplied callback `f` is invoked:

```ocaml
val f : pkg:string -> doc_path:string -> markdown:string -> unit
```

• `pkg` – the opam package name (directory basename)

• `doc_path` – path of the document *relative to the root*, including the
  package directory; this is a stable identifier you can use as a primary key
  in a database.

• `markdown` – UTF-8 encoded Markdown

Concurrency is handled with `Eio.Fiber.List.iter ~max_fibers:25`; up to 25
files are processed in parallel inside each directory.

---

## API

```ocaml
val crawl :
  root:_ Eio.Path.t ->
  f:(pkg:string -> doc_path:string -> markdown:string -> unit) ->
  unit
```

### Parameters

* `root` – directory produced by `dune build @doc` (the one that contains the
  individual package folders).

* `f` – callback to receive each document (see above).

### Behaviour & guarantees

* **Best-effort** – unreadable paths and conversion failures are logged and
  ignored.

* **No unexpected exceptions** – the only unchecked exceptions that may escape
  are those raised by `f` itself.

* **Bounded concurrency** – at most 25 fibers per directory provide
  throughput while keeping memory usage predictable.

* **Deterministic ordering inside a directory** – the directory entries are
  obtained from `Eio.Path.read_dir`, which returns a list sorted with
  `String.compare`.  When multiple fibers run concurrently the completion
  order is *not* deterministic, therefore do not rely on the callback being
  called in lexical order.

---

## Usage example

```ocaml
open Eio.Std

let () =
  Eio_main.run @@ fun env ->
  let root = Eio.Path.(Eio.Stdenv.cwd env / "_build/default/_doc/_html") in
  Odoc_crawler.crawl root ~f:(fun ~pkg ~doc_path ~markdown ->
    Printf.printf "[%s] %s – %d bytes\n%!" pkg doc_path (String.length markdown))
```

---

## Limitations & future work

* **Hard-coded heuristics** – the test for hidden modules relies on a magic
  string produced by odoc.  A more robust approach would parse the page’s DOM
  instead.

* **Binary assets are ignored** – only text documents (`.html`, `README.*`)
  are reported.  If you need images or other resources you will have to extend
  the crawler.

* **No cancellation propagation** – a failure in the callback cancels the
  current fiber but not the *whole* traversal.  You can wrap your callback in
  a custom cancellation context if needed.

---


