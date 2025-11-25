(** Implementation notes for Highlight_tm_loader.

    This module bridges Jsonaf and [textmate-language]'s Yojson decoder,
    builds a registry of grammars, and provides a few lookup helpers.
    See the interface file for the public API and detailed documentation. *)
open Core

type registry = TmLanguage.t

let create_registry () = TmLanguage.create ()

let number_to_yojson (s : string) : [> `Int of int | `Float of float ] Or_error.t =
  match Int.of_string_opt s with
  | Some i -> Ok (`Int i)
  | None ->
    (match Or_error.try_with (fun () -> Float.of_string s) with
     | Ok f -> Ok (`Float f)
     | Error e -> Error e)
;;

let rec yo_of_jsonaf (j : Jsonaf.t) : TmLanguage.yojson Or_error.t =
  match j with
  | `Null -> Ok `Null
  | `False -> Ok (`Bool false)
  | `True -> Ok (`Bool true)
  | `String s -> Ok (`String s)
  | `Number s -> number_to_yojson s
  | `Object kvs ->
    let open Or_error.Let_syntax in
    let%bind kvs' =
      List.map kvs ~f:(fun (k, v) ->
        let%map v' = yo_of_jsonaf v in
        k, v')
      |> Or_error.combine_errors
    in
    Ok (`Assoc kvs')
  | `Array xs ->
    let open Or_error.Let_syntax in
    let%map xs' = xs |> List.map ~f:yo_of_jsonaf |> Or_error.combine_errors in
    `List xs'
;;

let add_grammar_jsonaf (t : registry) (j : Jsonaf.t) : unit Or_error.t =
  let open Or_error.Let_syntax in
  let%bind j_yo = yo_of_jsonaf j in
  let%map g = Or_error.try_with (fun () -> TmLanguage.of_yojson_exn j_yo) in
  TmLanguage.add_grammar t g
;;

let add_grammar_jsonaf_file (t : registry) ~(path : string) : unit Or_error.t =
  let open Or_error.Let_syntax in
  let%bind contents = Or_error.try_with (fun () -> In_channel.read_all path) in
  let%bind j = Jsonaf.parse contents in
  add_grammar_jsonaf t j
;;

let find_grammar_by_lang_tag (t : registry) (lang : string) : TmLanguage.grammar option =
  let lower = String.lowercase lang in
  let candidates : string list =
    match lower with
    | "ocaml" | "ml" | "mli" -> [ "OCaml"; "source.ocaml"; "ml"; "mli" ]
    | "dune" | "dune-project" | "dune-workspace" ->
      [ "Dune"; "source.dune"; "dune"; "dune-project"; "dune-workspace" ]
    | "opam" -> [ "OPAM"; "source.opam"; "opam" ]
    | "bash" | "sh" | "shell" -> [ "Shell Script"; "Bash"; "source.shell"; "sh"; "bash" ]
    | "diff" | "patch" -> [ "Diff"; "source.diff"; "diff"; "patch" ]
    | "markdown" | "md" | "gfm" ->
      [ "Markdown"
      ; "GitHub Markdown"
      ; "text.html.markdown"
      ; "source.gfm"
      ; "markdown"
      ; "md"
      ; "gfm"
      ]
    | other -> [ other; "source." ^ other ]
  in
  let rec try_find = function
    | [] -> None
    | c :: cs ->
      (match TmLanguage.find_by_name t c with
       | Some _ as g -> g
       | None ->
         (match TmLanguage.find_by_scope_name t c with
          | Some _ as g -> g
          | None ->
            (match TmLanguage.find_by_filetype t c with
             | Some _ as g -> g
             | None -> try_find cs)))
  in
  try_find candidates
;;

let find_grammar_for_info_string (t : registry) (info : string option)
  : TmLanguage.grammar option
  =
  match info with
  | None -> None
  | Some s ->
    let s = String.strip s in
    let lang =
      match String.lsplit2 ~on:' ' s with
      | None -> s
      | Some (hd, _) -> hd
    in
    if String.is_empty lang then None else find_grammar_by_lang_tag t lang
;;
