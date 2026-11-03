open! Core
module W = Agent_protocol.Developer_repl_wire

type t =
  | Location_only of string
  | Excerpt of
      { location : string
      ; prefix : string
      ; before : string
      ; marked : string
      ; after : string
      ; marker : string
      }

let escape text ~max_bytes =
  let output = Buffer.create (Int.min max_bytes 256) in
  let stopped = ref false in
  let add text =
    if not !stopped
    then
      if String.length text <= max_bytes - Buffer.length output
      then Buffer.add_string output text
      else stopped := true
  in
  Uutf.String.fold_utf_8
    (fun () _ -> function
       | `Malformed _ -> add "�"
       | `Uchar uchar ->
         let code = Stdlib.Uchar.to_int uchar in
         if code = 9
         then add "    "
         else if code < 32 || code = 127 || (code >= 128 && code <= 159)
         then add (Printf.sprintf "\\u{%04x}" code)
         else (
           let encoded = Buffer.create 4 in
           Stdlib.Buffer.add_utf_8_uchar encoded uchar;
           add (Buffer.contents encoded)))
    ()
    text;
  Buffer.contents output
;;

let columns text = Notty.I.width (Notty.I.string Notty.A.empty text)

let line_start source byte =
  let rec loop offset line start =
    if offset >= byte
    then line, start
    else (
      match source.[offset] with
      | '\n' -> loop (offset + 1) (line + 1) (offset + 1)
      | '\r' ->
        let next =
          if offset + 1 < String.length source && Char.equal source.[offset + 1] '\n'
          then offset + 2
          else offset + 1
        in
        if next > byte then line, start else loop next (line + 1) next
      | _ -> loop (offset + 1) line start)
  in
  loop 0 1 0
;;

let create diagnostic ~submitted_source ~max_line_bytes =
  let max_line_bytes = Int.clamp_exn max_line_bytes ~min:32 ~max:8_192 in
  Option.map (W.Submission.Diagnostic.location diagnostic) ~f:(fun location ->
    let start = W.Submission.Location.start_byte location in
    let stop = W.Submission.Location.end_byte location in
    let id = W.Submission.Location.source location in
    let label =
      Printf.sprintf
        "%s:%s: bytes %d-%d"
        (escape (Chatml_source.Id.package_namespace id) ~max_bytes:128)
        (escape (Chatml_source.Id.relative_path id) ~max_bytes:256)
        start
        stop
    in
    match submitted_source with
    | Some (source, digest)
      when String.length source <= 65_536
           && start >= 0
           && stop >= start
           && stop <= String.length source
           && Chatml_source.Digest.equal digest (W.Submission.Location.digest location) ->
      let line_number, first = line_start source start in
      let last =
        String.findi source ~f:(fun index char ->
          index >= first && (Char.equal char '\n' || Char.equal char '\r'))
        |> Option.value_map ~default:(String.length source) ~f:fst
      in
      let line = String.sub source ~pos:first ~len:(last - first) in
      let boundaries =
        Uuseg_string.fold_utf_8
          `Grapheme_cluster
          (fun (offset, boundaries) cluster ->
             let next = offset + String.length cluster in
             next, next :: boundaries)
          (0, [ 0 ])
          line
        |> snd
        |> List.rev
      in
      let floor offset =
        List.take_while boundaries ~f:(fun boundary -> boundary <= offset)
        |> List.last
        |> Option.value ~default:0
      in
      let ceil offset =
        List.find boundaries ~f:(fun boundary -> boundary >= offset)
        |> Option.value ~default:(String.length line)
      in
      let prefix = Printf.sprintf "%d | " line_number in
      let budget = Int.max 1 ((max_line_bytes - String.length prefix - 6) / 8) in
      let column = Int.min (String.length line) (start - first) in
      let chunk_first = floor (Int.max 0 (column - (budget / 3))) in
      let chunk_last = floor (Int.min (String.length line) (chunk_first + budget)) in
      let mark_first = Int.min chunk_last (Int.max chunk_first (floor column)) in
      let mark_last =
        if stop = start
        then mark_first
        else Int.min chunk_last (ceil (Int.min (String.length line) (stop - first)))
      in
      let piece first last =
        String.sub line ~pos:first ~len:(Int.max 0 (last - first))
        |> fun text -> escape text ~max_bytes:max_line_bytes
      in
      let before =
        (if chunk_first > 0 then "..." else "") ^ piece chunk_first mark_first
      in
      let marked = piece mark_first mark_last in
      let after =
        piece mark_last chunk_last ^ if chunk_last < String.length line then "..." else ""
      in
      let marker =
        String.make (String.length prefix + columns before) ' '
        ^ String.make (Int.max 1 (columns marked)) '^'
      in
      Excerpt { location = label; prefix; before; marked; after; marker }
    | Some _ | None -> Location_only label)
;;
