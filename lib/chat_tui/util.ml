open Core

(* ------------------------------------------------------------------------- *)
(*  Control-character helpers                                                *)
(* ------------------------------------------------------------------------- *)

let is_C0 x = x < 0x20 || x = 0x7f

let is_ctrl_non_nl c =
  let code = Char.to_int c in
  (code < 32 || code = 127) && not (Char.equal c '\n')
;;

(* ------------------------------------------------------------------------- *)
(*  Sanitising & truncation                                                  *)
(* ------------------------------------------------------------------------- *)

let sanitize ?(strip = true) s =
  let buf = Buffer.create (String.length s) in
  String.iter s ~f:(fun c ->
    match c with
    | '\n' -> Buffer.add_char buf '\n'
    | '\t' -> Buffer.add_string buf "    " (* 4-space TAB expansion *)
    | _ when is_C0 (Char.to_int c) -> Buffer.add_char buf ' '
    | _ when is_ctrl_non_nl c -> Buffer.add_char buf ' '
    | _ -> Buffer.add_char buf c);
  let res = Buffer.contents buf in
  if strip then String.strip res else res
;;

let truncate ?(max_len = 300) s =
  let s = String.strip s in
  if String.length s > max_len then String.sub s ~pos:0 ~len:max_len ^ "…" else s
;;

(* ------------------------------------------------------------------------- *)
(*  UTF-8 aware byte-length word wrapping                                     *)
(* ------------------------------------------------------------------------- *)

(* Determine the byte length of the UTF-8 code-point that starts with the
   given first byte [b].  We assume that the input is valid UTF-8 and fall
   back to a single-byte character for any malformed sequence so we always
   make progress. *)
let utf8_char_len b =
  if b land 0x80 = 0
  then 1
  else if b land 0xE0 = 0xC0
  then 2
  else if b land 0xF0 = 0xE0
  then 3
  else if b land 0xF8 = 0xF0
  then 4
  else 1
;;

(* [wrap_line ~limit s] splits [s] (UTF-8) into chunks whose {e byte} length
   is ≤ [limit] while ensuring that we never cut in the middle of a multi-byte
   code-point.  The function does not attempt to measure display width – wide
   glyphs are still counted as one unit here. *)
let wrap_line ~limit s =
  let len = String.length s in
  let rec loop pos acc =
    if pos >= len
    then List.rev acc
    else (
      (* Walk forward until adding the next code-point would exceed [limit].
         Track the last whitespace position to prefer breaking at spaces. *)
      let rec forward p cells_left last_ws =
        if p >= len || cells_left <= 0
        then p, last_ws
        else (
          let ch = String.unsafe_get s p in
          let char_len = utf8_char_len (Char.to_int ch) in
          let last_ws = if Char.equal ch ' ' then Some p else last_ws in
          if 1 > cells_left
          then if p = pos then p + char_len, last_ws else p, last_ws
          else forward (p + char_len) (cells_left - 1) last_ws)
      in
      let p_end, last_ws = forward pos limit None in
      let reached_limit = p_end < len in
      let cut, next_pos =
        if not reached_limit
        then len, len
        else (
          let slop =
            match Sys.getenv "OCHAT_WRAP_SLOP_CELLS" with
            | Some s ->
              (try Int.max 0 (Int.of_string s) with
               | _ -> Int.min 12 (limit / 3))
            | None -> Int.min 12 (limit / 3)
          in
          let rec find_next_ws i upper =
            if i >= upper
            then None
            else if Char.equal (String.unsafe_get s i) ' '
            then Some i
            else find_next_ws (i + 1) upper
          in
          let next_ws = find_next_ws p_end (Int.min len (p_end + slop + 1)) in
          let cut_ws =
            match next_ws, last_ws with
            | Some ws, _ when ws > pos -> Some ws
            | _, Some ws when ws > pos -> Some ws
            | _ -> None
          in
          let cut = Int.min (Option.value cut_ws ~default:p_end) len in
          let next_pos =
            match cut_ws with
            | Some ws when ws >= pos && ws <= len -> Int.min (ws + 1) len
            | _ -> if p_end = pos then pos + 1 else p_end
          in
          cut, next_pos)
      in
      let slice_len = Int.max 0 (cut - pos) in
      let slice = if slice_len > 0 then String.sub s ~pos ~len:slice_len else "" in
      let acc = if String.is_empty slice then acc else slice :: acc in
      loop next_pos acc)
  in
  loop 0 []
;;

(* ------------------------------------------------------------------------- *)
(*  Reasoning text reflow                                                     *)
(* ------------------------------------------------------------------------- *)

let reflow_reasoning_paragraphs s =
  let is_blank l = String.is_empty (String.strip l) in
  let is_bullet l =
    let l = String.strip l in
    if String.is_empty l
    then false
    else
      Char.(l.[0] = '-' || l.[0] = '*')
      || (String.length l >= 2 && Char.is_digit l.[0] && Char.equal l.[1] '.')
  in
  let flush_para acc out =
    match List.rev acc with
    | [] -> out
    | xs -> String.concat ~sep:" " xs :: out
  in
  let rec loop lines acc out =
    match lines with
    | [] -> List.rev (flush_para acc out)
    | l :: rest when is_blank l -> loop rest [] (flush_para acc out)
    | l :: rest when is_bullet l ->
      let out = flush_para acc out in
      loop rest [] (l :: out)
    | l :: rest -> loop rest (String.strip l :: acc) out
  in
  loop (String.split_lines s) [] [] |> String.concat ~sep:"\n\n"
;;

(* ------------------------------------------------------------------------- *)
(*  Generic soft-break reflow (markdown-ish)                                  *)
(* ------------------------------------------------------------------------- *)

let reflow_soft_breaks s =
  let is_blank l = String.is_empty (String.strip l) in
  let is_list l =
    let l = String.strip l in
    String.is_prefix l ~prefix:"- "
    || String.is_prefix l ~prefix:"* "
    || String.is_prefix l ~prefix:"+ "
    || String.is_prefix l ~prefix:"– "
    || String.is_prefix l ~prefix:"— "
    || String.is_prefix l ~prefix:"• "
    || (String.length l >= 2 && Char.is_digit l.[0] && Char.equal l.[1] '.')
  in
  let is_heading l =
    let l = String.strip l in
    String.length l > 0 && Char.equal l.[0] '#'
  in
  let is_quote l =
    let l = String.strip l in
    String.is_prefix l ~prefix:">"
  in
  let is_preformatted l =
    String.is_prefix l ~prefix:"    " || String.is_prefix l ~prefix:"\t"
  in
  let flush acc out =
    match List.rev acc with
    | [] -> out
    | xs -> String.concat ~sep:" " xs :: out
  in
  let rec loop lines acc out in_code =
    match lines with
    | [] -> List.rev (flush acc out)
    | l :: rest ->
      let t = String.strip l in
      if String.is_prefix t ~prefix:"```" || String.is_prefix t ~prefix:"~~~"
      then (
        let out = flush acc out in
        loop rest [] (t :: out) (not in_code))
      else if in_code
      then loop rest [] (l :: flush acc out) in_code
      else if is_blank l
      then loop rest [] (flush acc out) in_code
      else if is_list l || is_heading l || is_quote l || is_preformatted l
      then (
        let out = flush acc out in
        loop rest [] (l :: out) in_code)
      else loop rest (t :: acc) out in_code
  in
  loop (String.split_lines s) [] [] false |> String.concat ~sep:"\n\n"
;;

(* ------------------------------------------------------------------------- *)
(*  Reflow multi-line bullet items                                            *)
(* ------------------------------------------------------------------------- *)

let reflow_bulleted_paragraphs s =
  let lines = String.split_lines s in
  let is_blank l = String.is_empty (String.strip l) in
  let is_fence l =
    let t = String.strip l in
    String.is_prefix t ~prefix:"```" || String.is_prefix t ~prefix:"~~~"
  in
  let bullet_prefix trimmed =
    let len = String.length trimmed in
    let starts c = len >= 2 && Char.equal trimmed.[0] c && Char.equal trimmed.[1] ' ' in
    if starts '-' || starts '*' || starts '+'
    then Some 2
    else (
      let rec digits i =
        if i < len && Char.is_digit trimmed.[i] then digits (i + 1) else i
      in
      let i = digits 0 in
      if i > 0 && i < len && Char.equal trimmed.[i] '.'
      then (
        let j = if i + 1 < len && Char.equal trimmed.[i + 1] ' ' then i + 2 else i + 1 in
        Some j)
      else None)
  in
  let rec loop acc in_code = function
    | [] -> List.rev acc |> String.concat ~sep:"\n"
    | l :: rest when is_fence l -> loop (l :: acc) (not in_code) rest
    | l :: rest when in_code -> loop (l :: acc) in_code rest
    | l :: rest ->
      let indent =
        let rec count i =
          if i < String.length l && Char.equal l.[i] ' ' then count (i + 1) else i
        in
        count 0
      in
      let trimmed = String.drop_prefix l indent in
      (match bullet_prefix trimmed with
       | None -> loop (l :: acc) in_code rest
       | Some pref_len ->
         let prefix = String.sub trimmed ~pos:0 ~len:pref_len in
         let head = String.drop_prefix trimmed pref_len |> String.strip in
         let rec gather parts = function
           | [] -> parts, []
           | nxt :: more when is_fence nxt || is_blank nxt -> parts, nxt :: more
           | nxt :: more ->
             let ntrim = String.strip nxt in
             let stop =
               let nlen = String.length ntrim in
               nlen > 0
               && (Char.(ntrim.[0] = '-')
                   || Char.(ntrim.[0] = '*')
                   || Char.(ntrim.[0] = '+')
                   || Char.(ntrim.[0] = '#')
                   || Char.(ntrim.[0] = '>')
                   || String.is_prefix nxt ~prefix:"    "
                   || String.is_prefix nxt ~prefix:"\t"
                   || Char.is_digit ntrim.[0])
             in
             if stop then parts, nxt :: more else gather (ntrim :: parts) more
         in
         let parts, rest_cont = gather [ head ] rest in
         let body = String.concat ~sep:" " (List.rev parts) in
         let rebuilt = String.make indent ' ' ^ prefix ^ body in
         loop (rebuilt :: acc) in_code rest_cont)
  in
  loop [] false lines
;;
