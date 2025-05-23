open Core

(* Suppress «unused value» warnings in this helper module – several low-level
   predicates are kept around because they are useful elsewhere but may remain
   unused depending on the call-site. *)
[@@@warning "-32"]

(* ------------------------------------------------------------------------- *)
(*  Control-character helpers                                                *)
(* ------------------------------------------------------------------------- *)

let is_C0 x = x < 0x20 || x = 0x7f
let is_C1 x = 0x80 <= x && x < 0xa0

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
      (* Walk forward until adding the next code-point would exceed [limit] *)
      let rec forward p bytes_left =
        if p >= len || bytes_left <= 0
        then p
        else (
          let char_len = utf8_char_len (Char.to_int (String.unsafe_get s p)) in
          if char_len > bytes_left
          then
            if
              (* If we haven't consumed anything yet we {must} include this
               (oversized) code-point to guarantee progress and validity. *)
              p = pos
            then p + char_len
            else p
          else forward (p + char_len) (bytes_left - char_len))
      in
      (* [forward] may, in rare cases, overshoot [len] if the input string
         ends with a truncated or otherwise malformed multi–byte UTF-8
         sequence.  Guard against that by clamping [cut] to [len] before we
         attempt to slice – this prevents the occasional
         "pos + len past end" exception that was observed when the very last
         line of a streamed assistant response happened to trigger the corner
         case. *)
      let cut = forward pos limit in
      let cut = Int.min cut len in
      let slice =
        if cut > pos
        then
          (* Do NOT assume [cut - pos] is valid – clamp once more to be extra
             safe. *)
          let slice_len = Int.min (cut - pos) (len - pos) in
          if slice_len > 0 then String.sub s ~pos ~len:slice_len else ""
        else ""
      in
      let acc = if String.is_empty slice then acc else slice :: acc in
      (* Ensure progress by at least one byte to avoid infinite loops. *)
      let next_pos = if cut = pos then pos + 1 else cut in
      loop next_pos acc)
  in
  loop 0 []
;;
