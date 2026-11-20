open! Core

type t =
  { index : int
  ; text : string
  }

let limit message = Protocol_error.create Resource_limit ~message ~retryable:false ()

let create ~index ~text =
  if index < 0 || index >= 4096
  then Error (Protocol_error.invalid_request "invalid history text part index")
  else if String.length text > 1_048_576
  then Error (limit "history text part exceeds 1 MiB")
  else if not (Stdlib.String.is_valid_utf_8 text)
  then Error (Protocol_error.invalid_request "search entry is not valid UTF-8")
  else Ok { index; text }
;;

(* Only used on an admitted part with an offset in [0, length]. *)
let boundary text offset =
  offset = String.length text || Char.to_int text.[offset] land 0xc0 <> 0x80
;;

let rec previous text offset =
  if boundary text offset then offset else previous text (offset - 1)
;;

let rec next text offset = if boundary text offset then offset else next text (offset + 1)

module Slice = struct
  type t =
    { text : string
    ; next_offset : int option
    }
end

let read ?(max_bytes = 4096) t ~start ~stop =
  if max_bytes <= 0 || max_bytes > 16_384
  then Error (Protocol_error.invalid_request "invalid history text byte budget")
  else if
    start < 0
    || stop < start
    || stop > String.length t.text
    || not (boundary t.text start && boundary t.text stop)
  then Error (Protocol_error.invalid_request "invalid history text byte range")
  else (
    let finish = previous t.text (Int.min stop (start + max_bytes)) in
    if finish = start && finish < stop
    then Error (limit "history text budget cannot fit the next scalar")
    else
      Ok
        Slice.
          { text = String.sub t.text ~pos:start ~len:(finish - start)
          ; next_offset = Option.some_if (finish < stop) finish
          })
;;

module Literal = struct
  type t =
    { pattern : String.Search_pattern.t
    ; match_bytes : int
    }

  let create term =
    let text = Search_term.text term in
    { pattern = String.Search_pattern.create ~case_sensitive:false text
    ; match_bytes = String.length text
    }
  ;;

  module Match = struct
    type t =
      { start : int
      ; stop : int
      ; snippet : Search_snippet.t
      }
  end

  let find t part =
    match String.Search_pattern.index t.pattern ~in_:part.text with
    | None -> Ok None
    | Some start ->
      let length = String.length part.text in
      let left = next part.text (Int.max 0 (start - ((512 - t.match_bytes) / 2))) in
      let right = previous part.text (Int.min length (left + 512)) in
      let open Result.Let_syntax in
      let%map snippet =
        Search_snippet.create
          ~text:(String.sub part.text ~pos:left ~len:(right - left))
          ~highlight_start:(start - left)
          ~highlight_length:t.match_bytes
          ~truncated_before:(left > 0)
          ~truncated_after:(right < length)
      in
      Some Match.{ start; stop = start + t.match_bytes; snippet }
  ;;
end
