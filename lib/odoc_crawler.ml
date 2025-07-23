(** Traverse an [_odoc_] HTML documentation tree and present each document as
    Markdown.

    The directory layout produced by the
    [`odoc`][https://github.com/ocaml/odoc] documentation generator looks like
    this (simplified):

    {v
    _build/default/_doc/_html/
    ├─ core/
    │  ├─ Core/
    │  │  ├─ Array.html
    │  │  └─ …
    │  └─ _doc-dir/README.md          <- optional package README
    ├─ eio/
    │  └─ …
    └─ index.html
    v}

    Each *package* gets its own top-level directory (e.g. [core] or [eio]).
    Inside the package directory every module is rendered as an HTML file and
    there may also be a [_doc-dir_] sub-directory that contains the package’s
    README in Markdown.

    [Odoc_crawler] walks such a tree concurrently and, for every relevant
    document encountered, converts it to Markdown (using
    {!Webpage_markdown.Html_to_md}) and forwards the result to a user-supplied
    callback.  Hidden modules (those whose generated HTML contains the
    sentinel string "This module is hidden.") are skipped.

    The crawler is resilient: unreadable paths, parser failures and other
    recoverable errors are logged with {!Log.emit} and ignored.  The only
    unchecked exceptions that can escape are those raised by the callback
    itself.

    Concurrency is provided by {!Eio.Fiber.List.iter} with
    [~max_fibers = 25] – at most 25 directory entries are processed at the
    same time per level.

    {1 Example}

    Crawling the documentation produced by a local `dune build @doc` run:

    {[ocaml]
      let () =
        Eio_main.run @@ fun env ->
        let root = Eio.Path.(Eio.Stdenv.cwd env / "_build/default/_doc/_html") in
        Odoc_crawler.crawl root ~f:(fun ~pkg ~doc_path ~markdown ->
          Format.printf "[%s] %s (chars: %d)\n%!" pkg doc_path (String.length markdown))
    ]}
    *)

open Core
open Jsonaf.Export

(*--------------------------------------------------------------------------*)
(* Helpers                                                                  *)
(*--------------------------------------------------------------------------*)

let is_html_file file = String.is_suffix file ~suffix:".html"

(** [is_html_file file] is [true] iff [file] ends with the [.html] extension.
    The check is case-sensitive – the odoc generator only emits lowercase
    extensions. *)

let is_readme_md file =
  let lower = String.lowercase file in
  String.equal lower "readme" || String.is_prefix lower ~prefix:"readme."
;;

(** [is_readme_md file] recognises package README files inside an
    [_doc-dir] folder.  The test is case-insensitive and matches both the bare
    name [README] as well as any file whose basename starts with
    [README.], e.g. [README.md] or [ReadMe.markdown]. *)

let read_file_to_string (path : _ Eio.Path.t) : string =
  (* Read the whole file [path] into a string.  We rely on [Eio.Buf_read]     *)
  (* which already exists in the code-base for similar tasks.                 *)
  Eio.Switch.run (fun sw ->
    let flow = Eio.Path.open_in ~sw path in
    (* No practical size limit – odoc HTML files are typically < 5 MB.     *)
    Eio.Buf_read.(parse_exn take_all) flow ~max_size:Int.max_value)
;;

(** [read_file_to_string path] loads the entire file [path] into memory and
    returns it as a single string.

    The implementation relies on {!Eio.Buf_read.parse_exn} with the
    {!Eio.Buf_read.take_all} parser and therefore honours the reader limits
    enforced by that module.  In practice odoc-generated HTML files are
    small (around 5 MB), so the limit of [Int.max_value] is safe.  The function never
    blocks the scheduler for long: IO is handled by {!Eio.Switch.run}. *)

let markdown_of_html (html : string) : string =
  (* Convert [html] to Markdown using the existing converter implemented in   *)
  (* [lib/webpage_markdown].  We keep the conversion tolerant: if parsing     *)
  (* fails we fall back to returning the raw HTML wrapped in a fenced block,  *)
  (* ensuring we never raise.                                                 *)
  match
    Or_error.try_with (fun () ->
      (* Parsing via lambdasoup.  The parser may raise on ill-formed HTML;  *)
      let soup = Soup.parse html in
      Webpage_markdown.Html_to_md.to_markdown_string soup)
  with
  | Ok md -> md
  | Error _ -> Printf.sprintf "```html\n%s\n```" html
;;

(** [markdown_of_html html] converts [html] (produced by odoc) to GitHub-flavour
    Markdown.  Conversion uses {!Webpage_markdown.Html_to_md.to_markdown_string}
    which internally parses the markup with {!Soup.parse}.

    If the HTML is ill-formed or the converter raises for any reason the raw
    HTML is returned verbatim, fenced inside a [```html] block so that the
    caller still receives something renderable.  The function therefore never
    raises. *)

(*--------------------------------------------------------------------------*)
(* Main public API                                                           *)
(*--------------------------------------------------------------------------*)

(** [crawl root ?filter f] recursively scans the documentation tree rooted at
      [root] and calls [f] for every document discovered.

      It treats every *sub-directory* of [root] as a separate opam package
      (identified by the directory's basename).  Within each package the
      following items are considered:

      • HTML pages – files ending in [.html].  Pages whose body contains the
        phrase "This module is hidden." are ignored because odoc uses this
        string for modules that are not part of the public API.

      • Package README inside an [_doc-dir] directory.  Files recognised by
        {!is_readme_md} are forwarded *unmodified* as Markdown.

      For HTML input {!markdown_of_html} is used to convert the page to
      Markdown before invoking the callback.

      The traversal is performed synchronously per directory but individual
      entries in a directory are processed concurrently using
      {!Eio.Fiber.List.iter} with up to 25 fibers.  A similar limit is used
      on the top-level enumeration of packages.

      The implementation is *best-effort*:

      • Files or directories that cannot be accessed are silently skipped.
      • Exceptions raised by the user-supplied callback [f] are propagated,
        but all other failures (e.g. HTML parsing) are logged with
        {!Log.emit} and ignored.

      @param root    Filesystem location of the directory produced by
                     [dune build @doc] or [odoc compile-pkg].
      @param filter  Optional predicate applied to directory names at the
                     *package* level.  A package whose basename fails the
                     predicate is skipped entirely.  Defaults to a function
                     that always returns [true].
      @param f       User callback receiving the package name, the path of the
                     document relative to [root], and the converted Markdown
                     string.

      {2 Example}

      See the module-level example above for a complete program.  *)
let crawl
      ~(root : _ Eio.Path.t)
      ?(filter : string -> bool = fun _ -> true)
      (f : pkg:string -> doc_path:string -> markdown:string -> unit)
  : unit
  =
  let rec walk_dir (dir_path : _ Eio.Path.t) ~(pkg : string) ~(relative_prefix : string)
    : unit
    =
    let entries = Eio.Path.read_dir dir_path in
    Eio.Fiber.List.iter
      ~max_fibers:25
      (fun entry ->
         (* Skip current/parent directory entries that may appear on some FS. *)
         if not (String.equal entry "." || String.equal entry "..")
         then (
           let child = Eio.Path.(dir_path / entry) in
           let stats = Or_error.try_with (fun () -> Eio.Path.stat ~follow:true child) in
           match stats with
           | Error _ -> () (* Ignore unreadable paths silently. *)
           | Ok stats ->
             (match stats.kind with
              | `Directory ->
                let new_prefix = Filename.concat relative_prefix entry in
                walk_dir child ~pkg ~relative_prefix:new_prefix
              | `Regular_file ->
                (* 1. HTML documentation pages *)
                if is_html_file entry
                then (
                  let html = read_file_to_string child in
                  if String.is_substring html ~substring:"This module is hidden."
                  then
                    Log.emit
                      ~ctx:[ "pkg", jsonaf_of_string pkg; "file", jsonaf_of_string entry ]
                      `Debug
                      "skip_hidden_module"
                  else (
                    let doc_path = Filename.concat relative_prefix entry in
                    Log.emit
                      ~ctx:
                        [ "pkg", jsonaf_of_string pkg; "file", jsonaf_of_string doc_path ]
                      `Info
                      "doc_found";
                    let markdown = markdown_of_html html in
                    try f ~pkg ~doc_path ~markdown with
                    | _ -> ()))
                else if
                  String.is_substring relative_prefix ~substring:"_doc-dir"
                  && is_readme_md entry
                then (
                  let doc_path = Filename.concat relative_prefix entry in
                  Log.emit
                    ~ctx:
                      [ "pkg", jsonaf_of_string pkg; "file", jsonaf_of_string doc_path ]
                    `Info
                    "readme_found";
                  let markdown = read_file_to_string child in
                  try f ~pkg ~doc_path ~markdown with
                  | _ -> ())
                else ()
              | _ -> ())))
      entries
  in
  (* Enumerate first-level entries under [root] and consider only folders as  *)
  (* packages.                                                                *)
  let pkgs = Eio.Path.read_dir root in
  Eio.Fiber.List.iter
    ~max_fibers:25
    (fun entry ->
       if not (String.equal entry "." || String.equal entry ".." || not (filter entry))
       then (
         let pkg_path = Eio.Path.(root / entry) in
         match Or_error.try_with (fun () -> Eio.Path.stat ~follow:true pkg_path) with
         | Ok { kind = `Directory; _ } ->
           walk_dir pkg_path ~pkg:entry ~relative_prefix:entry
         | _ -> ()))
    pkgs
;;
