open! Core

type t =
  { history : string list
  ; query : string
  ; match_index : int option
  }

let create ~history =
  let rec retain acc bytes count = function
    | [] -> List.rev acc
    | source :: rest ->
      let len = String.length source in
      if count >= 1_024 || len > (8 * 1_024 * 1_024) - bytes
      then List.rev acc
      else retain (source :: acc) (bytes + len) (count + 1) rest
  in
  let history = retain [] 0 0 history in
  { history; query = ""; match_index = (if List.is_empty history then None else Some 0) }
;;

let query t = t.query
let match_index t = t.match_index
let matched_source t = Option.bind t.match_index ~f:(List.nth t.history)
let matches t source = String.is_substring source ~substring:t.query

let restart t =
  { t with
    match_index =
      List.findi t.history ~f:(fun _ source -> matches t source) |> Option.map ~f:fst
  }
;;

let valid_utf8 text =
  Uutf.String.fold_utf_8
    (fun valid _ -> function
       | `Uchar _ -> valid
       | `Malformed _ -> false)
    true
    text
;;

let insert t text =
  if not (valid_utf8 text)
  then Or_error.error_string "History search requires valid UTF-8"
  else if String.length text > 256 - String.length t.query
  then Or_error.error_string "History search query exceeds 256 bytes"
  else Ok (restart { t with query = t.query ^ text })
;;

let backspace t =
  let _, last =
    Uuseg_string.fold_utf_8
      `Grapheme_cluster
      (fun (offset, _) cluster -> offset + String.length cluster, offset)
      (0, 0)
      t.query
  in
  restart { t with query = String.prefix t.query last }
;;

let next t =
  let after = Option.value t.match_index ~default:(-1) in
  match
    List.findi t.history ~f:(fun index source -> index > after && matches t source)
  with
  | Some (index, _) -> { t with match_index = Some index }
  | None -> restart t
;;
