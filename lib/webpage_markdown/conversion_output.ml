open Core
module B = Conversion_budget

type t =
  { node : node
  ; length : int
  }

and node =
  | Leaf of string * int
  | Char of char
  | Repeat of char
  | Concat of t list
  | Slice of t * int

let text budget s =
  B.nodes budget 2;
  { node = Leaf (s, 0); length = String.length s }
;;

let char budget c =
  B.nodes budget 2;
  { node = Char c; length = 1 }
;;

let repeat budget c count =
  if count < 0 then invalid_arg "negative text repetition";
  B.nodes budget 2;
  { node = Repeat c; length = count }
;;

let concat budget pieces =
  let length =
    List.fold pieces ~init:0 ~f:(fun total piece ->
      B.work budget 1;
      if piece.length > Int.max_value - total then B.reject_output budget;
      total + piece.length)
  in
  B.nodes budget 2;
  { node = Concat pieces; length }
;;

(* Exact cached lengths plus an explicit continuation stack keep flat/left-deep
   pieces stack safe. No intermediate encoded indentation/string is allocated. *)
let iter budget root ~f =
  let frame piece skip take rest =
    B.nodes budget 3;
    (piece, skip, take) :: rest
  in
  let rec loop = function
    | [] -> ()
    | (piece, skip, take) :: rest ->
      B.work budget 1;
      (match piece.node with
       | Leaf (s, start) ->
         if take > 0 then f (`String (s, start + skip, take));
         loop rest
       | Char c ->
         if take > 0 then f (`Char c);
         loop rest
       | Repeat c ->
         if take > 0 then f (`Repeat (c, take));
         loop rest
       | Slice (inner, start) -> loop (frame inner (start + skip) take rest)
       | Concat pieces ->
         let rec expand skip take acc = function
           | [] -> B.rev_append budget acc rest
           | _ when take = 0 -> B.rev_append budget acc rest
           | child :: xs ->
             B.work budget 1;
             if skip >= child.length
             then expand (skip - child.length) take acc xs
             else (
               let count = Int.min (child.length - skip) take in
               expand 0 (take - count) (frame child skip count acc) xs)
         in
         loop (expand skip take [] pieces))
  in
  loop (frame root 0 root.length [])
;;

let iter_chars budget piece ~f =
  iter budget piece ~f:(function
    | `Char c ->
      B.work budget 1;
      f c
    | `Repeat (c, count) ->
      for _i = 1 to count do
        B.work budget 1;
        f c
      done
    | `String (s, start, count) ->
      for i = start to start + count - 1 do
        B.work budget 1;
        f s.[i]
      done)
;;

let strip budget piece =
  let offset = ref 0
  and first = ref None
  and last = ref 0 in
  iter_chars budget piece ~f:(fun c ->
    if not (Char.is_whitespace c)
    then (
      if Option.is_none !first then first := Some !offset;
      last := !offset + 1);
    incr offset);
  let start = Option.value !first ~default:0 in
  B.nodes budget 2;
  { node = Slice (piece, start); length = !last - start }
;;

let escape_markdown budget s =
  let needs_escape = function
    | '\\'
    | '`'
    | '*'
    | '_'
    | '{'
    | '}'
    | '['
    | ']'
    | '('
    | ')'
    | '#'
    | '+'
    | '-'
    | '!'
    | '|'
    | '>'
    | '~'
    | '.' -> true
    | _ -> false
  in
  let pieces = ref []
  and start = ref 0 in
  for i = 0 to String.length s - 1 do
    B.work budget 1;
    if needs_escape s.[i]
    then (
      if i > !start
      then (
        B.nodes budget 3;
        pieces := { node = Leaf (s, !start); length = i - !start } :: !pieces);
      B.nodes budget 6;
      pieces
      := { node = Char s.[i]; length = 1 } :: { node = Char '\\'; length = 1 } :: !pieces;
      start := i + 1)
  done;
  if !start < String.length s
  then (
    B.nodes budget 3;
    pieces := { node = Leaf (s, !start); length = String.length s - !start } :: !pieces);
  concat budget (B.rev budget !pieces)
;;

let to_string budget piece =
  let count = ref 0 in
  iter budget piece ~f:(fun segment ->
    let n =
      match segment with
      | `Char _ -> 1
      | `Repeat (_, n) | `String (_, _, n) -> n
    in
    if n > Int.max_value - !count then B.reject_output budget;
    count := !count + n;
    B.output_bytes budget !count);
  B.output_bytes budget !count;
  (* Admit both the writes and the immutable final copy before allocating the
     full output buffer. Delivery still polls between bounded write batches. *)
  B.work budget !count;
  B.work budget !count;
  let since_poll = ref 0 in
  let writing count =
    if count >= 4096 - !since_poll
    then (
      since_poll := 0;
      B.poll budget)
    else since_poll := !since_poll + count
  in
  let buffer = Buffer.create !count in
  iter budget piece ~f:(function
    | `Char c ->
      writing 1;
      Buffer.add_char buffer c
    | `Repeat (c, n) ->
      for _i = 1 to n do
        writing 1;
        Buffer.add_char buffer c
      done
    | `String (s, start, n) ->
      let rec write offset =
        if offset < n
        then (
          let chunk = Int.min 4096 (n - offset) in
          writing chunk;
          Buffer.add_substring buffer s ~pos:(start + offset) ~len:chunk;
          write (offset + chunk))
      in
      write 0);
  Buffer.contents buffer
;;

let byte_length t = t.length
