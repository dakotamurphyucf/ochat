(* Terminal syntax-highlighting engine.

   This module turns plain text into per-line [(Notty.A.t * string)] spans.
   When configured with a TextMate registry (from the [textmate-language]
   library) and a resolved [lang], lines are tokenized and coloured via
   {!Highlight_theme}. Otherwise, it falls back to a single plain span per
   line. See the interface for user-facing documentation. *)

open Core

type t =
  { theme : Highlight_theme.t
  ; registry : Highlight_tm_loader.registry option
  }

type span = Notty.A.t * string

type fallback_reason =
  | No_registry
  | Unknown_language of string
  | Tokenize_error

type info = { fallback : fallback_reason option }

let create ~theme = { theme; registry = None }
let with_theme (t : t) ~theme = { t with theme }
let with_registry (t : t) ~registry = { t with registry = Some registry }

let fallback_spans ~text =
  String.split_lines text |> List.map ~f:(fun line -> [ Notty.A.empty, line ])
;;

let compress_adjacent (spans : span list) : span list =
  let rec loop acc = function
    | [] -> List.rev acc
    | (attr, s) :: rest ->
      (match acc with
       | (attr', s') :: acc_rest when phys_equal attr attr' ->
         loop ((attr, s' ^ s) :: acc_rest) rest
       | _ -> loop ((attr, s) :: acc) rest)
  in
  loop [] spans
;;

let highlight_text (t : t) ~lang ~text =
  match t.registry, lang with
  | Some reg, Some l ->
    (match Highlight_tm_loader.find_grammar_by_lang_tag reg l with
     | None -> fallback_spans ~text
     | Some grammar ->
       let lines = String.split_lines text in
       let rec process acc stack = function
         | [] -> List.rev acc
         | line :: rest ->
           let line_for_tm = line ^ "\n" in
           (match
              Or_error.try_with (fun () ->
                TmLanguage.tokenize_exn reg grammar stack line_for_tm)
            with
            | Error e ->
              (* ignore @@ raise (Failure "Tokenization failed"); *)
              ignore @@ raise (Failure (Error.to_string_hum e));
              process ([ Notty.A.empty, line ] :: acc) stack rest
            | Ok (tokens, stack') ->
              let line_len = String.length line in
              let spans, _prev_end =
                Stdlib.List.fold_left
                  (fun (acc_spans, prev_end) tok ->
                     let ending = TmLanguage.ending tok in
                     let start_i = Int.min prev_end line_len in
                     let end_i = Int.min ending line_len in
                     let seg =
                       if end_i <= start_i
                       then ""
                       else String.sub line ~pos:start_i ~len:(end_i - start_i)
                     in
                     let attr =
                       Highlight_theme.attr_of_scopes
                         t.theme
                         ~scopes:(TmLanguage.scopes tok)
                     in
                     if String.length seg = 0
                     then acc_spans, ending
                     else (attr, seg) :: acc_spans, ending)
                  ([], 0)
                  tokens
              in
              let spans = compress_adjacent (List.rev spans) in
              process (spans :: acc) stack' rest)
       in
       process [] TmLanguage.empty lines)
  | _ -> fallback_spans ~text
;;

let highlight_text_with_info (t : t) ~lang ~text : span list list * info =
  match t.registry, lang with
  | None, _ -> fallback_spans ~text, { fallback = Some No_registry }
  | Some _, None -> fallback_spans ~text, { fallback = Some (Unknown_language "") }
  | Some reg, Some l ->
    (match Highlight_tm_loader.find_grammar_by_lang_tag reg l with
     | None -> fallback_spans ~text, { fallback = Some (Unknown_language l) }
     | Some _grammar ->
       let spans = highlight_text t ~lang ~text in
       spans, { fallback = None })
;;
