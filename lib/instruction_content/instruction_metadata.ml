open! Core
open Instruction_model

let invalid ?field line reason =
  Error (Error.Invalid_metadata { Metadata_error.line; field; reason })
;;

let scalar_count text ~poll =
  let count = ref 0 in
  String.iteri text ~f:(fun index c ->
    if index % 256 = 0 then poll ();
    if Char.to_int c land 0xc0 <> 0x80 then Int.incr count);
  !count
;;

let valid_text text ~poll =
  poll ();
  if
    (not (Stdlib.String.is_valid_utf_8 text))
    || String.is_prefix text ~prefix:"\239\187\191"
    || String.contains text '\000'
  then Error Error.Invalid_encoding
  else (
    ignore (scalar_count text ~poll : int);
    Ok ())
;;

let scalar ~field ~line text ~poll =
  let open Result.Let_syntax in
  let invalid reason = invalid ~field line reason in
  if String.length text > 8192
  then invalid Limit_exceeded
  else (
    let text = String.strip text in
    let length = String.length text in
    let%bind value =
      if length = 0
      then invalid Invalid_value
      else if Char.equal text.[0] '"'
      then (
        let limits =
          Document_schema.Limits.create
            ~max_bytes:8192
            ~max_depth:2
            ~max_fields:2
            ~max_nodes:2
          |> function
          | Ok value -> value
          | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]
        in
        match Document_schema.Json.decode ~limits text with
        | Ok (`String value) -> Ok value
        | Ok _ | Error _ -> invalid Unsupported_syntax)
      else if Char.equal text.[0] '\''
      then
        if length < 2 || not (Char.equal text.[length - 1] '\'')
        then invalid Unsupported_syntax
        else (
          let buffer = Buffer.create length in
          let rec copy index =
            poll ();
            if index = length - 1
            then Ok (Buffer.contents buffer)
            else if Char.equal text.[index] '\''
            then
              if index + 1 < length - 1 && Char.equal text.[index + 1] '\''
              then (
                Buffer.add_char buffer '\'';
                copy (index + 2))
              else invalid Unsupported_syntax
            else (
              Buffer.add_char buffer text.[index];
              copy (index + 1))
          in
          copy 1)
      else if
        String.mem "!&*[{|>@`%#:?" text.[0]
        || String.equal text "-"
        || String.is_prefix text ~prefix:"- "
        || String.is_substring text ~substring:": "
        || String.is_substring text ~substring:" #"
      then invalid Unsupported_syntax
      else Ok text
    in
    if String.length value > 4096
    then invalid Limit_exceeded
    else if
      (not (Stdlib.String.is_valid_utf_8 value))
      || String.exists value ~f:(fun c -> Char.to_int c < 32 || Char.to_int c = 127)
    then invalid Invalid_value
    else (
      poll ();
      Ok value))
;;

(* Only the bounded header is sliced. No body-sized line list is constructed. *)
let line_at text start ~poll =
  let rec ending index =
    if index % 256 = 0 then poll ();
    if index = String.length text || Char.equal text.[index] '\n'
    then index
    else ending (index + 1)
  in
  let last = ending start in
  let content_end =
    if last > start && Char.equal text.[last - 1] '\r' then last - 1 else last
  in
  ( String.sub text ~pos:start ~len:(content_end - start)
  , if last = String.length text then last else last + 1 )
;;

let key_valid key =
  String.length key > 0
  && String.length key <= 64
  && String.for_all key ~f:(function
    | 'a' .. 'z' | '0' .. '9' | '-' | '_' -> true
    | _ -> false)
;;

let header text ~poll =
  let rec lines start number acc =
    poll ();
    if number > 128 || start > 8192
    then invalid number Limit_exceeded
    else if start >= String.length text
    then invalid number Unsupported_syntax
    else (
      (* Stop scanning before the fixed header ceiling, including an oversized
         single physical line. *)
      let remaining = Int.min (String.length text - start) (8193 - start) in
      let segment = String.sub text ~pos:start ~len:remaining in
      let line, used = line_at segment 0 ~poll in
      let next = start + used in
      if next > 8192
      then invalid number Limit_exceeded
      else if String.equal line "---"
      then Ok (List.rev acc, next)
      else lines next (number + 1) ((number, line) :: acc))
  in
  let first, next = line_at (String.prefix text 5) 0 ~poll in
  if String.equal first "---" then lines next 2 [] else Ok ([], 0)
;;

let parse_header ~slug lines ~poll =
  let open Result.Let_syntax in
  let rec collect rows in_metadata fields total = function
    | [] -> Ok (List.rev rows)
    | (line, text) :: rest ->
      poll ();
      let stripped = String.strip text in
      if String.is_empty stripped || String.is_prefix stripped ~prefix:"#"
      then collect rows in_metadata fields total rest
      else if fields >= 32
      then invalid line Limit_exceeded
      else (
        let nested = String.is_prefix text ~prefix:"  " in
        let text = if nested then String.drop_prefix text 2 else text in
        if
          (nested && not in_metadata)
          || ((not (String.is_empty text)) && Char.is_whitespace text.[0])
        then invalid line Unsupported_syntax
        else (
          match String.lsplit2 text ~on:':' with
          | None -> invalid line Unsupported_syntax
          | Some (key, raw) ->
            if not (key_valid key)
            then invalid line Invalid_value
            else if (not nested) && String.equal key "metadata"
            then
              if not (String.is_empty (String.strip raw))
              then invalid ~field:key line Unsupported_syntax
              else if
                List.exists rows ~f:(fun (name, _, _) -> String.equal name "metadata")
              then invalid ~field:key line Duplicate_field
              else collect (("metadata", "", line) :: rows) true (fields + 1) total rest
            else (
              let field = if nested then "metadata." ^ key else key in
              if List.exists rows ~f:(fun (name, _, _) -> String.equal name field)
              then invalid ~field line Duplicate_field
              else (
                let%bind value = scalar ~field ~line (String.strip raw) ~poll in
                let bytes = String.length field + String.length value in
                if bytes > 8192 - total
                then invalid ~field line Limit_exceeded
                else
                  collect
                    ((field, value, line) :: rows)
                    nested
                    (fields + 1)
                    (total + bytes)
                    rest))))
  in
  let%bind rows = collect [] false 0 0 lines in
  let required field =
    match List.find rows ~f:(fun (key, _, _) -> String.equal field key) with
    | None -> invalid ~field 1 Missing_field
    | Some (_, value, line) ->
      if String.is_empty value then invalid ~field line Invalid_value else Ok (value, line)
  in
  let%bind name, name_line = required "name" in
  let%bind description, description_line = required "description" in
  let%bind () =
    if not (String.equal name slug)
    then invalid ~field:"name" name_line Name_mismatch
    else if scalar_count description ~poll > 1024
    then invalid ~field:"description" description_line Limit_exceeded
    else Ok ()
  in
  let%bind () =
    match List.find rows ~f:(fun (key, _, _) -> String.equal key "compatibility") with
    | Some (_, value, line) when String.is_empty value || scalar_count value ~poll > 500
      -> invalid ~field:"compatibility" line Limit_exceeded
    | Some _ | None -> Ok ()
  in
  let annotations =
    List.filter_map rows ~f:(fun (key, value, _) ->
      if List.mem [ "name"; "description"; "metadata" ] key ~equal:String.equal
      then None
      else Some (key, value))
  in
  let warnings =
    List.filter_map annotations ~f:(fun (key, _) ->
      if
        List.mem [ "license"; "compatibility"; "allowed-tools" ] key ~equal:String.equal
        || String.is_prefix key ~prefix:"metadata."
      then None
      else Some (Warning.Unknown_field key))
  in
  Ok { Metadata.name = Some name; description = Some description; annotations; warnings }
;;

let parse ~kind ~member text ~poll =
  let open Result.Let_syntax in
  let%bind () = valid_text text ~poll in
  match kind with
  | Kind.Project_guidance -> Ok (Metadata.empty, 0)
  | Skill ->
    let slug =
      List.hd_exn (String.split (Source.Portable_path.to_string member) ~on:'/')
    in
    let%bind lines, body_start = header text ~poll in
    if body_start = 0
    then
      Ok
        ( Metadata.
            { name = Some slug
            ; description = None
            ; annotations = []
            ; warnings = [ Headerless_skill ]
            }
        , 0 )
    else (
      let%map metadata = parse_header ~slug lines ~poll in
      metadata, body_start)
;;
