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

type scoped_span =
  { attr : Notty.A.t
  ; text : string
  ; scopes : string list
  }

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

let fallback_scoped_spans ~text =
  String.split_lines text
  |> List.map ~f:(fun line -> [ { attr = Notty.A.empty; text = line; scopes = [] } ])
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

let compress_adjacent_scoped (spans : scoped_span list) : scoped_span list =
  let equal_scopes = List.equal String.equal in
  let rec loop acc = function
    | [] -> List.rev acc
    | ({ attr; text; scopes } as span) :: rest ->
      (match acc with
       | { attr = attr'; text = text'; scopes = scopes' } :: acc_rest
         when phys_equal attr attr' && equal_scopes scopes scopes' ->
         loop ({ attr; text = text' ^ text; scopes } :: acc_rest) rest
       | _ -> loop (span :: acc) rest)
  in
  loop [] spans
;;

let scoped_spans_of_tokens ~theme ~line (tokens : TmLanguage.token list)
  : scoped_span list
  =
  let line_len = String.length line in
  let spans, _prev_end =
    Stdlib.List.fold_left
      (fun (acc_spans, prev_end) tok ->
         let ending = TmLanguage.ending tok in
         let start_i = Int.min prev_end line_len in
         let end_i = Int.min ending line_len in
         let text =
           if end_i <= start_i
           then ""
           else String.sub line ~pos:start_i ~len:(end_i - start_i)
         in
         let scopes = TmLanguage.scopes tok in
         let attr = Highlight_theme.attr_of_scopes theme ~scopes in
         if String.is_empty text
         then acc_spans, ending
         else { attr; text; scopes } :: acc_spans, ending)
      ([], 0)
      tokens
  in
  List.rev spans
;;

let spans_of_tokens ~theme ~line (tokens : TmLanguage.token list) : span list =
  let line_len = String.length line in
  let spans, _prev_end =
    Stdlib.List.fold_left
      (fun (acc_spans, prev_end) tok ->
         let ending = TmLanguage.ending tok in
         let start_i = Int.min prev_end line_len in
         let end_i = Int.min ending line_len in
         let text =
           if end_i <= start_i
           then ""
           else String.sub line ~pos:start_i ~len:(end_i - start_i)
         in
         let attr =
           Highlight_theme.attr_of_scopes theme ~scopes:(TmLanguage.scopes tok)
         in
         if String.is_empty text
         then acc_spans, ending
         else (attr, text) :: acc_spans, ending)
      ([], 0)
      tokens
  in
  List.rev spans
;;

let tokenize_line reg grammar stack line =
  let line_for_tm = line ^ "\n" in
  Or_error.try_with (fun () -> TmLanguage.tokenize_exn reg grammar stack line_for_tm)
;;

let highlight_lines_with_scopes ~theme reg grammar ~text =
  let lines = String.split_lines text in
  let rec process acc stack = function
    | [] -> Ok (List.rev acc)
    | line :: rest ->
      (match tokenize_line reg grammar stack line with
       | Error _e -> Error ()
       | Ok (tokens, stack') ->
         let spans =
           scoped_spans_of_tokens ~theme ~line tokens |> compress_adjacent_scoped
         in
         let spans =
           if List.is_empty spans
           then [ { attr = Notty.A.empty; text = line; scopes = [] } ]
           else spans
         in
         process (spans :: acc) stack' rest)
  in
  process [] TmLanguage.empty lines
;;

let highlight_lines ~theme reg grammar ~text =
  let lines = String.split_lines text in
  let rec process acc stack = function
    | [] -> Ok (List.rev acc)
    | line :: rest ->
      (match tokenize_line reg grammar stack line with
       | Error _e -> Error ()
       | Ok (tokens, stack') ->
         let spans = spans_of_tokens ~theme ~line tokens |> compress_adjacent in
         let spans = if List.is_empty spans then [ Notty.A.empty, line ] else spans in
         process (spans :: acc) stack' rest)
  in
  process [] TmLanguage.empty lines
;;

let highlight_text_with_scopes (t : t) ~lang ~text =
  match t.registry, lang with
  | Some reg, Some l ->
    (match Highlight_tm_loader.find_grammar_by_lang_tag reg l with
     | None -> fallback_scoped_spans ~text
     | Some grammar ->
       (match highlight_lines_with_scopes ~theme:t.theme reg grammar ~text with
        | Ok spans -> spans
        | Error () -> fallback_scoped_spans ~text))
  | _ -> fallback_scoped_spans ~text
;;

let highlight_text (t : t) ~lang ~text =
  match t.registry, lang with
  | Some reg, Some l ->
    (match Highlight_tm_loader.find_grammar_by_lang_tag reg l with
     | None -> fallback_spans ~text
     | Some grammar ->
       (match highlight_lines ~theme:t.theme reg grammar ~text with
        | Ok spans -> spans
        | Error () -> fallback_spans ~text))
  | _ -> fallback_spans ~text
;;

let highlight_text_with_info (t : t) ~lang ~text : span list list * info =
  match t.registry, lang with
  | None, _ -> fallback_spans ~text, { fallback = Some No_registry }
  | Some _, None -> fallback_spans ~text, { fallback = Some (Unknown_language "") }
  | Some reg, Some l ->
    (match Highlight_tm_loader.find_grammar_by_lang_tag reg l with
     | None -> fallback_spans ~text, { fallback = Some (Unknown_language l) }
     | Some grammar ->
       (match highlight_lines ~theme:t.theme reg grammar ~text with
        | Ok spans -> spans, { fallback = None }
        | Error () -> fallback_spans ~text, { fallback = Some Tokenize_error }))
;;

let highlight_text_with_scopes_with_info (t : t) ~lang ~text
  : scoped_span list list * info
  =
  match t.registry, lang with
  | None, _ -> fallback_scoped_spans ~text, { fallback = Some No_registry }
  | Some _, None -> fallback_scoped_spans ~text, { fallback = Some (Unknown_language "") }
  | Some reg, Some l ->
    (match Highlight_tm_loader.find_grammar_by_lang_tag reg l with
     | None -> fallback_scoped_spans ~text, { fallback = Some (Unknown_language l) }
     | Some grammar ->
       (match highlight_lines_with_scopes ~theme:t.theme reg grammar ~text with
        | Ok spans -> spans, { fallback = None }
        | Error () -> fallback_scoped_spans ~text, { fallback = Some Tokenize_error }))
;;
