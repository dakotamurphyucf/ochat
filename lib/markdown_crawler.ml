(** Discover Markdown documents under a directory tree.

    The implementation is intentionally similar to {!Odoc_crawler} but
    simplified for plain hand-written documentation:

    • Recurses under [root] with a concurrency limit of 25 fibres per
      directory level.
    • Best-effort support for *.gitignore* patterns located at the root
      of the crawl (no nested *gitignore* resolution) plus a static
      deny-list of common build artefacts.
    • Considers regular files whose basename ends in one of
      [".md"; ".markdown"; ".mdown"].  Empty files are ignored.
    • Files larger than 10 MiB are skipped to protect memory usage.
    • All non-fatal problems are logged via {!Log.emit}; only exceptions
      raised by the user supplied callback propagate further. *)

open Core
open Jsonaf.Export

(*------------------------------------------------------------------*)
(* Configuration                                                    *)
(*------------------------------------------------------------------*)

let max_size_bytes = 10 * 1024 * 1024 (* 10 MiB *)
let valid_exts = [ ".md"; ".markdown"; ".mdown" ]

(*------------------------------------------------------------------*)
(* Gitignore handling (best-effort)                                  *)
(*------------------------------------------------------------------*)

let read_file_to_string (path : _ Eio.Path.t) : string =
  Eio.Switch.run (fun sw ->
    let flow = Eio.Path.open_in ~sw path in
    Eio.Buf_read.(parse_exn take_all) flow ~max_size:Int.max_value)
;;

(*------------------------------------------------------------------*)
(* Helpers (generic)                                                *)
(*------------------------------------------------------------------*)

(* We prefer the [path_glob] library for matching ignore patterns as it
   supports most Git-style globs.  If parsing fails (for example due to
   unsupported syntax) we fall back to a conservative PCRE conversion
   implemented below. *)

module Fallback_glob = struct
  (* Same as previous [Glob] implementation but renamed. *)

  let is_meta = function
    | '.' | '+' | '(' | ')' | '[' | ']' | '{' | '}' | '\\' | '^' | '$' | '|' -> true
    | _ -> false
  ;;

  let regex_of_glob (pattern : string) : string =
    let buf = Buffer.create (String.length pattern + 10) in
    Buffer.add_char buf '^';
    let len = String.length pattern in
    let rec loop i =
      if i = len
      then ()
      else (
        let c = pattern.[i] in
        (match c with
         | '*' -> Buffer.add_string buf ".*"
         | '?' -> Buffer.add_char buf '.'
         | '/' -> Buffer.add_char buf '/'
         | c when is_meta c ->
           Buffer.add_char buf '\\';
           Buffer.add_char buf c
         | c -> Buffer.add_char buf c);
        loop (i + 1))
    in
    loop 0;
    Buffer.add_char buf '$';
    Buffer.contents buf
  ;;

  let compile ?(dir_pattern = false) pattern : Pcre.regexp =
    let pat =
      if dir_pattern
      then (
        let trimmed = String.rstrip ~drop:(Char.equal '/') pattern in
        (* directory pattern → match prefix *)
        let re_source = regex_of_glob trimmed in
        (* remove leading ^ and trailing $ *)
        let body = String.sub re_source ~pos:1 ~len:(String.length re_source - 2) in
        Printf.sprintf "^%s(/.*)?$" body)
      else regex_of_glob pattern
    in
    Pcre.regexp pat
  ;;

  let matcher_of_pattern (pattern : string) ~(dir_pattern : bool) : string -> bool =
    let re = compile ~dir_pattern pattern in
    fun rel -> Pcre.pmatch ~rex:re rel
  ;;
end

(*------------------------------------------------------------------*)
(* Ignore-rule compilation                                           *)
(*------------------------------------------------------------------*)

type matcher = string -> bool

let glob_matcher_of_gitignore_line (line : string) : matcher option =
  let line = String.strip line in
  if String.is_empty line || Char.equal line.[0] '#'
  then None
  else (
    let dir_pattern = String.is_suffix line ~suffix:"/" in
    let pat = if dir_pattern then String.rstrip ~drop:(Char.equal '/') line else line in
    (* Try [path_glob] first *)
    let using_path_glob =
      match
        Or_error.try_with (fun () -> Path_glob.Glob.parse (Printf.sprintf "<%s>" pat))
      with
      | Ok glob -> Some (fun rel -> Path_glob.Glob.eval glob rel)
      | Error _ -> None
    in
    match using_path_glob with
    | Some m ->
      if dir_pattern
      then
        (* Dir pattern should ignore any path under that dir.  Augment matcher. *)
        Some (fun rel -> m rel || String.is_prefix rel ~prefix:(pat ^ "/"))
      else Some m
    | None -> Some (Fallback_glob.matcher_of_pattern line ~dir_pattern))
;;

let load_gitignore ~root : matcher list =
  let path = Eio.Path.(root / ".gitignore") in
  match Or_error.try_with (fun () -> read_file_to_string path) with
  | Error _ -> []
  | Ok contents ->
    let lines = String.split_lines contents in
    List.filter_map lines ~f:glob_matcher_of_gitignore_line
;;

let fallback_blocklist : matcher list =
  List.map
    [ "_build/"; "dist/"; "node_modules/"; ".git/"; ".hg/"; ".svn/" ]
    ~f:(fun pat ->
      let dir_pattern = true in
      Fallback_glob.matcher_of_pattern pat ~dir_pattern)
;;

(*------------------------------------------------------------------*)
(* Helpers                                                          *)
(*------------------------------------------------------------------*)

let has_valid_extension (file : string) : bool =
  List.exists valid_exts ~f:(fun ext -> String.is_suffix file ~suffix:ext)
;;

let should_ignore ~(matchers : matcher list) (rel_path : string) : bool =
  List.exists matchers ~f:(fun m -> m rel_path)
;;

(*------------------------------------------------------------------*)
(* Main traversal                                                   *)
(*------------------------------------------------------------------*)

let crawl ~(root : _ Eio.Path.t) ~(f : doc_path:string -> markdown:string -> unit) : unit =
  let gitignore_matchers = load_gitignore ~root in
  let ignore_matchers = gitignore_matchers @ fallback_blocklist in
  let rec walk_dir (dir : _ Eio.Path.t) ~(relative_prefix : string) : unit =
    let entries = Eio.Path.read_dir dir in
    Eio.Fiber.List.iter
      ~max_fibers:25
      (fun entry ->
         (* Skip pseudo entries *)
         if String.equal entry ".." || String.equal entry "."
         then ()
         else (
           let child = Eio.Path.(dir / entry) in
           match Or_error.try_with (fun () -> Eio.Path.stat ~follow:true child) with
           | Error _ -> () (* unreadable -> skip *)
           | Ok stats ->
             let rel_path =
               if String.is_empty relative_prefix
               then entry
               else Filename.concat relative_prefix entry
             in
             if should_ignore ~matchers:ignore_matchers rel_path
             then ()
             else (
               match stats.kind with
               | `Directory -> walk_dir child ~relative_prefix:rel_path
               | `Regular_file ->
                 if has_valid_extension entry
                 then (
                   match Or_error.try_with (fun () -> read_file_to_string child) with
                   | Error _ -> ()
                   | Ok markdown ->
                     if String.length markdown > max_size_bytes
                     then
                       Log.emit
                         ~ctx:[ "file", jsonaf_of_string rel_path ]
                         `Warn
                         "skip_large_markdown"
                     else if String.is_empty markdown
                     then ()
                     else (
                       Log.emit
                         ~ctx:[ "file", jsonaf_of_string rel_path ]
                         `Debug
                         "md_found";
                       try f ~doc_path:rel_path ~markdown with
                       | ex -> raise ex))
                 else ()
               | _ -> ())))
      entries
  in
  walk_dir root ~relative_prefix:""
;;
