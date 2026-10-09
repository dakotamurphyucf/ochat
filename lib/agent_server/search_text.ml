open! Core
module P = Agent_protocol

type t =
  { pattern : String.Search_pattern.t
  ; match_bytes : int
  }

let create term =
  let text = P.Search_term.text term in
  { pattern = String.Search_pattern.create ~case_sensitive:false text
  ; match_bytes = String.length text
  }
;;

let is_boundary text offset =
  if offset = String.length text
  then true
  else (
    let byte = Char.to_int text.[offset] in
    byte < 128 || byte >= 192)
;;

let snippet t text start =
  let length = String.length text in
  let match_end = start + t.match_bytes in
  let proposed_start = Int.max 0 (start - ((512 - t.match_bytes) / 2)) in
  let rec next_boundary offset =
    if is_boundary text offset then offset else next_boundary (offset + 1)
  in
  let snippet_start = next_boundary proposed_start in
  let rec previous_boundary offset =
    if is_boundary text offset then offset else previous_boundary (offset - 1)
  in
  let snippet_end = previous_boundary (Int.min length (snippet_start + 512)) in
  P.Search_snippet.create
    ~text:(String.sub text ~pos:snippet_start ~len:(snippet_end - snippet_start))
    ~highlight_start:(start - snippet_start)
    ~highlight_length:(match_end - start)
    ~truncated_before:(snippet_start > 0)
    ~truncated_after:(snippet_end < length)
;;

module Match = struct
  type t =
    { part_index : int
    ; snippet : P.Search_snippet.t
    }
end

let find t entry =
  let rec find_parts = function
    | [] -> Ok None
    | (part : Search_entry.Part.t) :: rest ->
      (match String.Search_pattern.index t.pattern ~in_:part.text with
       | None -> find_parts rest
       | Some start ->
         Result.map (snippet t part.text start) ~f:(fun snippet ->
           Some Match.{ part_index = part.index; snippet }))
  in
  find_parts (Search_entry.parts entry)
;;
