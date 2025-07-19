open Core
open Jsonaf.Export

(*--------------------------------------------------------------------------*)
(* Helpers                                                                  *)
(*--------------------------------------------------------------------------*)

let is_html_file file = String.is_suffix file ~suffix:".html"

let is_readme_md file =
  let lower = String.lowercase file in
  String.equal lower "readme" || String.is_prefix lower ~prefix:"readme."
;;

let read_file_to_string (path : _ Eio.Path.t) : string =
  (* Read the whole file [path] into a string.  We rely on [Eio.Buf_read]     *)
  (* which already exists in the code-base for similar tasks.                 *)
  Eio.Switch.run (fun sw ->
    let flow = Eio.Path.open_in ~sw path in
    (* No practical size limit – odoc HTML files are typically < 5 MB.     *)
    Eio.Buf_read.(parse_exn take_all) flow ~max_size:Int.max_value)
;;

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

(*--------------------------------------------------------------------------*)
(* Main public API                                                           *)
(*--------------------------------------------------------------------------*)

let crawl
      ~(root : _ Eio.Path.t)
      ~(f : pkg:string -> doc_path:string -> markdown:string -> unit)
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
       if not (String.equal entry "." || String.equal entry "..")
       then (
         let pkg_path = Eio.Path.(root / entry) in
         match Or_error.try_with (fun () -> Eio.Path.stat ~follow:true pkg_path) with
         | Ok { kind = `Directory; _ } ->
           walk_dir pkg_path ~pkg:entry ~relative_prefix:entry
         | _ -> ()))
    pkgs
;;
