open Core
module Styles = Highlight_styles

let attr scope =
  Highlight_theme.attr_of_scopes Highlight_theme.github_dark ~scopes:[ scope ]
;;

let key_attr = attr "entity.name.type.json"
let string_attr = attr "string.quoted.double.json"
let number_attr = attr "constant.numeric.json"
let literal_attr = attr "constant.language.json"
let punctuation_attr = attr "punctuation.separator.json"

let is_punctuation = function
  | '{' | '}' | '[' | ']' | ':' | ',' -> true
  | _ -> false
;;

let is_number_start c = Char.is_digit c || Char.equal c '-'

let is_number_char c =
  Char.is_digit c || List.mem [ '-'; '+'; '.'; 'e'; 'E' ] c ~equal:Char.equal
;;

let is_word_char c = Char.is_alpha c

let rec quoted_end line ~index ~escaped =
  if index >= String.length line
  then index
  else if escaped
  then quoted_end line ~index:(index + 1) ~escaped:false
  else (
    match line.[index] with
    | '\\' -> quoted_end line ~index:(index + 1) ~escaped:true
    | '"' -> index + 1
    | _ -> quoted_end line ~index:(index + 1) ~escaped:false)
;;

let take_quoted line start = quoted_end line ~index:(start + 1) ~escaped:false

let rec next_non_whitespace line index =
  if index >= String.length line
  then None
  else if Char.is_whitespace line.[index]
  then next_non_whitespace line (index + 1)
  else Some line.[index]
;;

let is_key line stop =
  Option.value_map (next_non_whitespace line stop) ~default:false ~f:(Char.equal ':')
;;

let rec take_while line index ~f =
  if index < String.length line && f line.[index]
  then take_while line (index + 1) ~f
  else index
;;

let add_span line ~start ~stop attr spans =
  if stop <= start
  then spans
  else (attr, String.sub line ~pos:start ~len:(stop - start)) :: spans
;;

let literal_style line ~start ~stop =
  match String.sub line ~pos:start ~len:(stop - start) with
  | "true" | "false" | "null" -> literal_attr
  | _ -> Styles.empty
;;

let plain_stop line index =
  take_while line index ~f:(fun c ->
    not (Char.equal c '"' || is_punctuation c || is_number_start c || is_word_char c))
;;

let token line index =
  match line.[index] with
  | '"' ->
    let stop = take_quoted line index in
    stop, if is_key line stop then key_attr else string_attr
  | c when is_punctuation c -> index + 1, punctuation_attr
  | c when is_number_start c -> take_while line index ~f:is_number_char, number_attr
  | c when is_word_char c ->
    let stop = take_while line index ~f:is_word_char in
    stop, literal_style line ~start:index ~stop
  | _ -> plain_stop line index, Styles.empty
;;

let rec highlight_from line index spans =
  if index >= String.length line
  then List.rev spans
  else (
    let stop, style = token line index in
    highlight_from line stop (add_span line ~start:index ~stop style spans))
;;

let highlight text =
  String.split_lines text |> List.map ~f:(fun line -> highlight_from line 0 [])
;;
