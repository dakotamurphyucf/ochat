open Core
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
