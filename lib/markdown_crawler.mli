(** Markdown file discovery.

    {!crawl} walks a directory tree, picks out hand-written Markdown files and
    streams their content to user code.  The helper is IO-only: it does not
    parse or inspect the documents beyond basic filtering.

    {1 Example}

    {[
      open Eio.Std

      let () =
        Eio_main.run @@ fun env ->
        let root = Eio.Path.(env#fs / "docs") in
        Markdown_crawler.crawl ~root ~f:(fun ~doc_path ~markdown ->
          Fmt.pr "• %s (%d bytes)@." doc_path (String.length markdown))
    ]}
*)

open! Core

val crawl :
  root:_ Eio.Path.t ->
  f:(doc_path:string -> markdown:string -> unit) ->
  unit
(** [crawl ~root ~f] traverses [root] recursively and invokes [f] for each
    Markdown document found.

    A file is considered Markdown if its basename ends with one of
    [".md"], [".markdown"] or [".mdown"].  Files larger than {b 10 MiB} or
    empty files are skipped.

    Arguments supplied to the callback:
    • [doc_path] – path relative to [root] using POSIX separators.
    • [markdown] – UTF-8 contents of the file.

    Traversal details:
    • Uses {!Eio.Fiber.List.iter} with [~max_fibers:25] for bounded
      concurrency at each directory level.
    • Ignore rules combine the root-level {.gitignore} (if present) with a
      built-in deny-list (["_build/"], ["dist/"], ["node_modules/"], …).

    Only exceptions raised from within [f] propagate to the caller; all other
    recoverable I/O problems are logged through {!Log.emit} and silently
    skipped. *)
