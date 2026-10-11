open Core

module Portable_path = struct
  type t = string [@@deriving compare, equal, sexp_of]

  let create path =
    if
      (not (String.is_empty path))
      && String.length path <= 1024
      && Stdlib.String.is_valid_utf_8 path
      && (not
            (String.exists path ~f:(function
               | '\\' | ':' | '\000' | '\r' | '\n' -> true
               | _ -> false)))
      && List.for_all (String.split path ~on:'/') ~f:(fun part ->
        not (String.is_empty part || String.equal part "." || String.equal part ".."))
    then Ok path
    else Or_error.error_string "invalid portable relative source path"
  ;;

  let to_string t = t

  let first_collision paths ~checkpoint =
    let folded =
      List.map paths ~f:(fun path ->
        checkpoint ();
        path, String.lowercase path)
      |> List.sort ~compare:(fun (left, folded_left) (right, folded_right) ->
        checkpoint ();
        let comparison = String.compare folded_left folded_right in
        if comparison = 0 then compare left right else comparison)
    in
    let rec distinct index = function
      | [] -> Ok index
      | (path, folded) :: rest ->
        checkpoint ();
        (match Map.find index folded with
         | Some previous -> Error (previous, path)
         | None -> distinct (Map.set index ~key:folded ~data:path) rest)
    in
    match distinct String.Map.empty folded with
    | Error pair -> Some pair
    | Ok index ->
      let rec inspect = function
        | [] -> None
        | (path, folded) :: rest ->
          checkpoint ();
          let rec prefixes offset =
            if offset = String.length folded
            then inspect rest
            else if Char.equal folded.[offset] '/'
            then (
              checkpoint ();
              match Map.find index (String.prefix folded offset) with
              | Some parent -> Some (parent, path)
              | None -> prefixes (offset + 1))
            else prefixes (offset + 1)
          in
          prefixes 0
      in
      inspect folded
  ;;
end

(* ----- Source, position and span -------------------------------------------------------------- *)

type t =
  { path : string option
  ; content : string
  }
[@@deriving sexp]

type position =
  { line : int
  ; column : int
  ; offset : int
  }
[@@deriving sexp]

type span =
  { left : position
  ; right : position
  }
[@@deriving sexp]

(* ----- Source manipulation functions ---------------------------------------------------------- *)

let make string = { path = None; content = string }

let from_file path =
  let file = Stdlib.In_channel.open_text path in
  let content = In_channel.input_all file in
  In_channel.close file;
  { path = Some path; content }
;;

let length source = String.length source.content

let at source offset =
  try Some source.content.[offset] with
  | Invalid_argument _ -> None
;;

let read source span =
  let start = max 0 (min span.left.offset (length source)) in
  let stop = max start (min span.right.offset (length source)) in
  String.sub source.content ~pos:start ~len:(stop - start)
;;

(* ----- Span manipulation functions ------------------------------------------------------------ *)

let merge lspan rspan =
  { left = (if lspan.left.offset <= rspan.left.offset then lspan.left else rspan.left)
  ; right =
      (if lspan.right.offset >= rspan.right.offset then lspan.right else rspan.right)
  }
;;
