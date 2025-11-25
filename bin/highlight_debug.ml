open Core
module Chat = Chat_tui

let create_registry () = Chat.Highlight_registry.get ()

let print_token_debug ~line ~tokens =
  (* Replicates the commented-out debug prints in highlight_text. *)
  print_endline "highlight_text Line output: ";
  print_endline (Printf.sprintf "Line: `%s`" line);
  print_endline (Printf.sprintf "Tokens: %d" (List.length tokens));
  let scopes_per_token =
    List.map tokens ~f:(fun tok -> String.concat ~sep:", " (TmLanguage.scopes tok))
  in
  print_endline (String.concat ~sep:" | " scopes_per_token);
  print_endline "";
  (* Per-token detailed line, as in the commented fold. *)
  let line_len = String.length line in
  let _prev_end =
    List.fold_left tokens ~init:0 ~f:(fun prev_end tok ->
      let ending = TmLanguage.ending tok in
      let start_i = Int.min prev_end line_len in
      let end_i = Int.min ending line_len in
      let seg =
        if end_i <= start_i
        then ""
        else String.sub line ~pos:start_i ~len:(end_i - start_i)
      in
      print_endline
        (Printf.sprintf
           "  Token: scopes=[%s], start=%d, end=%d, seg=`%s`"
           (String.concat ~sep:", " (TmLanguage.scopes tok))
           start_i
           end_i
           seg);
      ending)
  in
  ()
;;

let tokenize_and_debug_lines reg grammar text =
  let lines = String.split_lines text in
  let rec process stack = function
    | [] -> ()
    | line :: rest ->
      let line_for_tm = line ^ "\n" in
      (match
         Or_error.try_with (fun () ->
           TmLanguage.tokenize_exn reg grammar stack line_for_tm)
       with
       | Error e ->
         eprintf "[warn] Tokenization failed: %s\n" (Error.to_string_hum e);
         process stack rest
       | Ok (tokens, stack') ->
         print_token_debug ~line ~tokens;
         process stack' rest)
  in
  List.iter lines ~f:(fun l -> process TmLanguage.empty [ l ])
;;

let is_markdown_path path =
  let lower = String.lowercase path in
  String.is_suffix lower ~suffix:".md"
  || String.is_suffix lower ~suffix:".markdown"
  || String.is_suffix lower ~suffix:".mdown"
  || String.is_suffix lower ~suffix:".mkd"
;;

let lang_of_path path =
  match String.rsplit2 ~on:'.' path with
  | None -> None
  | Some (_base, ext) -> Some (String.lowercase ext)
;;

let run ~path ~override_lang ~split_markdown =
  let reg = create_registry () in
  let contents = In_channel.read_all path in
  match override_lang with
  | Some lang ->
    (match Chat.Highlight_tm_loader.find_grammar_by_lang_tag reg lang with
     | None -> eprintf "[error] No grammar found for lang=%s\n" lang
     | Some grammar -> tokenize_and_debug_lines reg grammar contents)
  | None ->
    if is_markdown_path path && not split_markdown
    then (
      match Chat.Highlight_tm_loader.find_grammar_by_lang_tag reg "markdown" with
      | None -> eprintf "[error] No markdown grammar available\n"
      | Some g -> tokenize_and_debug_lines reg g contents)
    else if is_markdown_path path && split_markdown
    then (
      let segments = Chat.Markdown_fences.split contents in
      List.iter segments ~f:(function
        | Chat.Markdown_fences.Text t ->
          (match Chat.Highlight_tm_loader.find_grammar_by_lang_tag reg "markdown" with
           | None -> ()
           | Some g -> tokenize_and_debug_lines reg g t)
        | Chat.Markdown_fences.Code_block { lang; code } ->
          let lang_tag = Option.value ~default:"" lang in
          (match Chat.Highlight_tm_loader.find_grammar_by_lang_tag reg lang_tag with
           | None -> ()
           | Some g -> tokenize_and_debug_lines reg g code)))
    else (
      match lang_of_path path with
      | None -> eprintf "[error] Could not infer language for %s\n" path
      | Some ext ->
        (match Chat.Highlight_tm_loader.find_grammar_by_lang_tag reg ext with
         | None -> eprintf "[error] No grammar found for extension=%s\n" ext
         | Some g -> tokenize_and_debug_lines reg g contents))
;;

let command =
  Command.basic
    ~summary:
      "Tokenize a file and print the same debug info as highlight_text's commented logs"
    (let open Command.Let_syntax in
     let%map_open path = anon ("FILE" %: string)
     and override_lang = flag "lang" (optional string) ~doc:"LANG Override language tag"
     and split_markdown =
       flag
         "split-markdown"
         no_arg
         ~doc:
           "Split Markdown into text/code segments like the TUI renderer.\n\
            By default, Markdown is processed as a single document to match\n\
            the highlight_text debug prints exactly."
     in
     fun () -> run ~path ~override_lang ~split_markdown)
;;

let () = Command_unix.run command
