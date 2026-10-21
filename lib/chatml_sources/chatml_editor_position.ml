open Core

module Encoding = struct
  type t =
    | Utf8_bytes
    | Utf16_units
  [@@deriving compare, equal, sexp_of]
end

module Error = struct
  type t =
    | Invalid_index_limit
    | Index_limit_exceeded
    | Invalid_coordinate
    | Reversed_range
    | Line_out_of_bounds of int
    | Column_out_of_bounds of
        { line : int
        ; column : int
        }
    | Byte_offset_out_of_bounds of int
    | Invalid_utf8 of { byte_offset : int }
    | Inside_utf8_scalar of { byte_offset : int }
    | Inside_utf16_surrogate_pair of
        { line : int
        ; column : int
        }
    | Inside_crlf of { byte_offset : int }
    | Foreign_location
    | Canonical_location_unavailable
  [@@deriving compare, equal, sexp_of]
end

module Position = struct
  type t =
    { line : int
    ; column : int
    }
  [@@deriving compare, equal, sexp_of]

  let create ~line ~column =
    if line < 0 || column < 0 then Error Error.Invalid_coordinate else Ok { line; column }
  ;;

  let line t = t.line
  let column t = t.column
end

module Range = struct
  type t =
    { start : Position.t
    ; finish : Position.t
    }
  [@@deriving compare, equal, sexp_of]

  let create ~start ~finish =
    if Position.compare start finish > 0
    then Error Error.Reversed_range
    else Ok { start; finish }
  ;;

  let start t = t.start
  let finish t = t.finish
end

type checkpoint =
  { byte_offset : int
  ; utf16_column : int
  }

type raw_line =
  { start_byte : int
  ; end_byte : int
  }

type line =
  { start_byte : int
  ; end_byte : int
  ; utf16_length : int
  ; checkpoints : checkpoint array
  }

type t =
  { source : Chatml_source.t
  ; lines : line array
  ; invalid_utf8 : int option
  ; index_entry_count : int
  }

let source_id t = Chatml_source.id t.source
let source_digest t = Chatml_source.digest t.source
let line_count t = Array.length t.lines
let index_entry_count t = t.index_entry_count
let stride = 4096

(* A complete scalar is admitted only in its shortest UTF-8 form, outside the
   surrogate range and at most U+10FFFF. Invalid bytes are left individually
   addressable by byte-coordinate mode. *)
let scalar contents ~byte_offset =
  let length = String.length contents in
  let byte i = Char.to_int contents.[i] in
  let continuation i = i < length && byte i land 0xc0 = 0x80 in
  let first = byte byte_offset in
  if first < 0x80
  then Some (first, 1)
  else if first >= 0xc2 && first <= 0xdf
  then
    if continuation (byte_offset + 1)
    then Some (((first land 0x1f) lsl 6) lor (byte (byte_offset + 1) land 0x3f), 2)
    else None
  else if first >= 0xe0 && first <= 0xef
  then (
    let second = byte_offset + 1 in
    let second_valid =
      continuation second
      &&
      if Int.equal first 0xe0
      then byte second >= 0xa0
      else if Int.equal first 0xed
      then byte second <= 0x9f
      else true
    in
    if second_valid && continuation (byte_offset + 2)
    then
      Some
        ( ((first land 0x0f) lsl 12)
          lor ((byte second land 0x3f) lsl 6)
          lor (byte (byte_offset + 2) land 0x3f)
        , 3 )
    else None)
  else if first >= 0xf0 && first <= 0xf4
  then (
    let second = byte_offset + 1 in
    let second_valid =
      continuation second
      &&
      if Int.equal first 0xf0
      then byte second >= 0x90
      else if Int.equal first 0xf4
      then byte second <= 0x8f
      else true
    in
    if second_valid && continuation (byte_offset + 2) && continuation (byte_offset + 3)
    then
      Some
        ( ((first land 0x07) lsl 18)
          lor ((byte second land 0x3f) lsl 12)
          lor ((byte (byte_offset + 2) land 0x3f) lsl 6)
          lor (byte (byte_offset + 3) land 0x3f)
        , 4 )
    else None)
  else None
;;

let utf16_units scalar = if scalar > 0xffff then 2 else 1

let poll_before_advance ~byte_offset ~next_byte ~next_poll ~checkpoint =
  if next_byte > next_poll
  then (
    checkpoint ();
    byte_offset + stride)
  else next_poll
;;

(* Count a final empty line and charge that base entry before publication. The
   first pass also records the earliest invalid UTF-8 lead without repairing it. *)
let scan_lines contents ~max_index_entries ~checkpoint =
  let length = String.length contents in
  let rec loop byte_offset line_start count invalid_utf8 reversed next_poll =
    let next_poll =
      if byte_offset >= next_poll
      then (
        checkpoint ();
        byte_offset + stride)
      else next_poll
    in
    if Int.equal byte_offset length
    then
      if count >= max_index_entries
      then Error Error.Index_limit_exceeded
      else
        Ok
          ( Array.of_list
              (List.rev ({ start_byte = line_start; end_byte = length } :: reversed))
          , invalid_utf8 )
    else (
      let char = contents.[byte_offset] in
      if Char.equal char '\r' || Char.equal char '\n'
      then
        if count >= max_index_entries
        then Error Error.Index_limit_exceeded
        else (
          let next_byte =
            if
              Char.equal char '\r'
              && byte_offset + 1 < length
              && Char.equal contents.[byte_offset + 1] '\n'
            then byte_offset + 2
            else byte_offset + 1
          in
          let next_poll =
            poll_before_advance ~byte_offset ~next_byte ~next_poll ~checkpoint
          in
          loop
            next_byte
            next_byte
            (count + 1)
            invalid_utf8
            ({ start_byte = line_start; end_byte = byte_offset } :: reversed)
            next_poll)
      else (
        match scalar contents ~byte_offset with
        | Some (_, width) ->
          let next_byte = byte_offset + width in
          let next_poll =
            poll_before_advance ~byte_offset ~next_byte ~next_poll ~checkpoint
          in
          loop next_byte line_start count invalid_utf8 reversed next_poll
        | None ->
          let invalid_utf8 = Option.first_some invalid_utf8 (Some byte_offset) in
          loop (byte_offset + 1) line_start count invalid_utf8 reversed next_poll))
  in
  loop 0 0 0 None [] 0
;;

let build_line contents (raw : raw_line) ~used ~max_index_entries ~checkpoint =
  let rec loop byte_offset utf16_column last_checkpoint reversed used next_poll =
    let next_poll =
      if byte_offset >= next_poll
      then (
        checkpoint ();
        byte_offset + stride)
      else next_poll
    in
    if Int.equal byte_offset raw.end_byte
    then
      Ok
        ( { start_byte = raw.start_byte
          ; end_byte = raw.end_byte
          ; utf16_length = utf16_column
          ; checkpoints = Array.of_list (List.rev reversed)
          }
        , used )
    else (
      match scalar contents ~byte_offset with
      | None -> Error (Error.Invalid_utf8 { byte_offset })
      | Some (codepoint, width) ->
        let next_byte = byte_offset + width in
        let next_column = utf16_column + utf16_units codepoint in
        let next_poll =
          poll_before_advance ~byte_offset ~next_byte ~next_poll ~checkpoint
        in
        if next_byte - last_checkpoint > stride
        then
          if used >= max_index_entries
          then Error Error.Index_limit_exceeded
          else
            loop
              next_byte
              next_column
              byte_offset
              ({ byte_offset; utf16_column } :: reversed)
              (used + 1)
              next_poll
        else loop next_byte next_column last_checkpoint reversed used next_poll)
  in
  loop raw.start_byte 0 raw.start_byte [] used raw.start_byte
;;

let create source ~max_index_entries ~checkpoint =
  if max_index_entries <= 0
  then Error Error.Invalid_index_limit
  else (
    checkpoint ();
    let contents = Chatml_source.contents source in
    let open Result.Let_syntax in
    let%bind raw_lines, invalid_utf8 =
      scan_lines contents ~max_index_entries ~checkpoint
    in
    let%bind lines, index_entry_count =
      match invalid_utf8 with
      | Some _ ->
        Ok
          ( Array.map raw_lines ~f:(fun raw ->
              checkpoint ();
              { start_byte = raw.start_byte
              ; end_byte = raw.end_byte
              ; utf16_length = 0
              ; checkpoints = [||]
              })
          , Array.length raw_lines )
      | None ->
        let rec build index used reversed =
          if Int.equal index (Array.length raw_lines)
          then Ok (Array.of_list (List.rev reversed), used)
          else (
            let%bind line, used =
              build_line contents raw_lines.(index) ~used ~max_index_entries ~checkpoint
            in
            build (index + 1) used (line :: reversed))
        in
        build 0 (Array.length raw_lines) []
    in
    checkpoint ();
    Ok { source; lines; invalid_utf8; index_entry_count })
;;

let line_at t line =
  if line >= Array.length t.lines
  then Error (Error.Line_out_of_bounds line)
  else Ok t.lines.(line)
;;

let line_for_byte t byte_offset =
  let rec search low high =
    if high - low <= 1
    then low
    else (
      let middle = low + ((high - low) / 2) in
      if t.lines.(middle).start_byte <= byte_offset
      then search middle high
      else search low middle)
  in
  let index = search 0 (Array.length t.lines) in
  index, t.lines.(index)
;;

let inside_scalar contents byte_offset =
  let rec inspect start =
    if start >= byte_offset
    then false
    else (
      match scalar contents ~byte_offset:start with
      | Some (_, width) when start + width > byte_offset -> true
      | None | Some _ -> inspect (start + 1))
  in
  inspect (Int.max 0 (byte_offset - 3))
;;

let checkpoint_for_byte line byte_offset =
  let entries = line.checkpoints in
  let rec search low high =
    if low >= high
    then low - 1
    else (
      let middle = low + ((high - low) / 2) in
      if entries.(middle).byte_offset <= byte_offset
      then search (middle + 1) high
      else search low middle)
  in
  let index = search 0 (Array.length entries) in
  if index < 0
  then { byte_offset = line.start_byte; utf16_column = 0 }
  else entries.(index)
;;

let checkpoint_for_column line column =
  let entries = line.checkpoints in
  let rec search low high =
    if low >= high
    then low - 1
    else (
      let middle = low + ((high - low) / 2) in
      if entries.(middle).utf16_column <= column
      then search (middle + 1) high
      else search low middle)
  in
  let index = search 0 (Array.length entries) in
  if index < 0
  then { byte_offset = line.start_byte; utf16_column = 0 }
  else entries.(index)
;;

let valid_utf16 t =
  match t.invalid_utf8 with
  | None -> Ok ()
  | Some byte_offset -> Error (Error.Invalid_utf8 { byte_offset })
;;

let byte_offset_of_position t ~encoding position ~checkpoint =
  checkpoint ();
  let open Result.Let_syntax in
  let line_number = Position.line position in
  let column = Position.column position in
  let%bind line = line_at t line_number in
  let out_of_bounds () =
    Error (Error.Column_out_of_bounds { line = line_number; column })
  in
  match encoding with
  | Encoding.Utf8_bytes ->
    if column > line.end_byte - line.start_byte
    then out_of_bounds ()
    else (
      let byte_offset = line.start_byte + column in
      if inside_scalar (Chatml_source.contents t.source) byte_offset
      then Error (Error.Inside_utf8_scalar { byte_offset })
      else Ok byte_offset)
  | Encoding.Utf16_units ->
    let%bind () = valid_utf16 t in
    if column > line.utf16_length
    then out_of_bounds ()
    else (
      let contents = Chatml_source.contents t.source in
      let base = checkpoint_for_column line column in
      let rec scan byte_offset units =
        if Int.equal units column
        then Ok byte_offset
        else (
          checkpoint ();
          match scalar contents ~byte_offset with
          | None -> Error (Error.Invalid_utf8 { byte_offset })
          | Some (codepoint, width) ->
            let next_units = units + utf16_units codepoint in
            if next_units > column
            then Error (Error.Inside_utf16_surrogate_pair { line = line_number; column })
            else scan (byte_offset + width) next_units)
      in
      scan base.byte_offset base.utf16_column)
;;

let position_of_byte_offset t ~encoding ~byte_offset ~checkpoint =
  checkpoint ();
  let length = Chatml_source.byte_length t.source in
  if byte_offset < 0 || byte_offset > length
  then Error (Error.Byte_offset_out_of_bounds byte_offset)
  else
    let open Result.Let_syntax in
    let%bind () =
      match encoding with
      | Encoding.Utf8_bytes -> Ok ()
      | Encoding.Utf16_units -> valid_utf16 t
    in
    let line_number, line = line_for_byte t byte_offset in
    if byte_offset > line.end_byte
    then Error (Error.Inside_crlf { byte_offset })
    else (
      let contents = Chatml_source.contents t.source in
      if inside_scalar contents byte_offset
      then Error (Error.Inside_utf8_scalar { byte_offset })
      else (
        match encoding with
        | Encoding.Utf8_bytes ->
          Ok { Position.line = line_number; column = byte_offset - line.start_byte }
        | Encoding.Utf16_units ->
          let base = checkpoint_for_byte line byte_offset in
          let rec scan offset units =
            if Int.equal offset byte_offset
            then Ok { Position.line = line_number; column = units }
            else (
              checkpoint ();
              match scalar contents ~byte_offset:offset with
              | None -> Error (Error.Invalid_utf8 { byte_offset = offset })
              | Some (codepoint, width) ->
                scan (offset + width) (units + utf16_units codepoint))
          in
          scan base.byte_offset base.utf16_column))
;;

let location_of_range t ~encoding range ~checkpoint =
  let open Result.Let_syntax in
  let%bind start_byte =
    byte_offset_of_position t ~encoding (Range.start range) ~checkpoint
  in
  let%bind end_byte =
    byte_offset_of_position t ~encoding (Range.finish range) ~checkpoint
  in
  Chatml_source.locate t.source ~start_byte ~end_byte ~checkpoint
  |> Result.map_error ~f:(fun _ -> Error.Canonical_location_unavailable)
;;

let range_of_location t ~encoding location ~checkpoint =
  if
    (not (Chatml_source.Id.equal (source_id t) (Chatml_source.Location.source location)))
    || not
         (Chatml_source.Digest.equal
            (source_digest t)
            (Chatml_source.Location.source_digest location))
  then Error Error.Foreign_location
  else
    let open Result.Let_syntax in
    let span = Chatml_source.Location.span location in
    let%bind start =
      position_of_byte_offset t ~encoding ~byte_offset:span.left.offset ~checkpoint
    in
    let%bind finish =
      position_of_byte_offset t ~encoding ~byte_offset:span.right.offset ~checkpoint
    in
    Range.create ~start ~finish
;;
