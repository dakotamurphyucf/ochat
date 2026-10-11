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

module Position_index = struct
  (* These arrays are private and mutate only inside [create], before publication. *)
  type t =
    { contents : string
    ; lines : int array
    ; columns : int array
    }

  let stride = 4096

  let scan contents ~start_byte ~end_byte ~line ~column ~checkpoint ~record =
    let length = String.length contents in
    let rec loop offset line column =
      if offset mod stride = 0
      then (
        checkpoint ();
        match record with
        | None -> ()
        | Some t ->
          let slot = offset / stride in
          t.lines.(slot) <- line;
          t.columns.(slot) <- column);
      if offset = end_byte
      then { line; column; offset }
      else (
        match String.get contents offset with
        | '\r' ->
          if offset + 1 < length && Char.equal (String.get contents (offset + 1)) '\n'
          then loop (offset + 1) line (column + 1)
          else loop (offset + 1) (line + 1) 0
        | '\n' -> loop (offset + 1) (line + 1) 0
        | _ -> loop (offset + 1) line (column + 1))
    in
    loop start_byte line column
  ;;

  let create contents ~checkpoint =
    let length = String.length contents in
    let slots = (length / stride) + 1 in
    let t =
      { contents; lines = Array.create ~len:slots 1; columns = Array.create ~len:slots 0 }
    in
    let (_ : position) =
      scan
        contents
        ~start_byte:0
        ~end_byte:length
        ~line:1
        ~column:0
        ~checkpoint
        ~record:(Some t)
    in
    t
  ;;

  let position t ~byte_offset ~checkpoint =
    if byte_offset < 0 || byte_offset > String.length t.contents
    then Or_error.error_string "source byte offset is out of bounds"
    else (
      let slot = byte_offset / stride in
      Ok
        (scan
           t.contents
           ~start_byte:(slot * stride)
           ~end_byte:byte_offset
           ~line:t.lines.(slot)
           ~column:t.columns.(slot)
           ~checkpoint
           ~record:None))
  ;;
end
