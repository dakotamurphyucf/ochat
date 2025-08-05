open Core

(*----------------------------------------------------------------------*)
(* Attachments helper                                                   *)
(*----------------------------------------------------------------------*)

(* [copy_recursively ~src ~dst] copies the directory tree rooted at
   [src] into [dst].  It is a best-effort helper that silently ignores
   missing sources (non-existing [src]) so that callers can freely pass
   multiple optional locations.  Directory permissions are set to
   0o700 which is consistent with the rest of the persistence helpers. *)

let copy_recursively ~(src : _ Eio.Path.t) ~(dst : _ Eio.Path.t) : unit =
  let rec aux src dst =
    if Eio.Path.is_directory src
    then (
      (match Eio.Path.is_directory dst with
       | true -> ()
       | false -> Eio.Path.mkdirs ~perm:0o700 dst);
      List.iter (Eio.Path.read_dir src) ~f:(fun name ->
        let ( / ) = Eio.Path.( / ) in
        let src_item = src / name in
        let dst_item = dst / name in
        if Eio.Path.is_directory src_item
        then aux src_item dst_item
        else (
          let content = Io.load_doc ~dir:src name in
          Io.save_doc ~dir:dst name content)))
  in
  (* Silently ignore absent [src] so that callers can always supply all
     potential locations without pre-checking. *)
  try aux src dst with
  | _ -> ()
;;

(* [copy_all ~prompt_dir ~cwd ~session_dir ~dst] copies *.chatmd
   attachment directories from three potential locations into [dst]:

   1. [prompt_dir /.chatmd] – sibling of the original prompt file.
   2. [cwd /.chatmd]       – runtime working directory (tool outputs).
   3. [session_dir /.chatmd] – session directory used by the TUI.

   The helper de-duplicates identical source directories but otherwise
   performs the copies sequentially so that later sources can overwrite
   earlier ones (e.g. runtime artefacts override prompt-time files). *)

let copy_all ~prompt_dir ~cwd ~session_dir ~dst =
  let ( / ) = Eio.Path.( / ) in
  let srcs = [ prompt_dir / ".chatmd"; cwd / ".chatmd"; session_dir / ".chatmd" ] in
  (* Use physical path strings to eliminate duplicates. *)
  let unique_srcs =
    let tbl = Hashtbl.create (module String) in
    List.filter srcs ~f:(fun p ->
      let s = Format.asprintf "%a" Eio.Path.pp p in
      match Hashtbl.find tbl s with
      | Some _ -> false
      | None ->
        Hashtbl.set tbl ~key:s ~data:();
        true)
  in
  List.iter unique_srcs ~f:(fun src -> copy_recursively ~src ~dst)
;;
