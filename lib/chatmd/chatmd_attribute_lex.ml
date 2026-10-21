open! Core

module Byte_range = struct
  type t =
    { start_byte : int
    ; end_byte : int
    }

  let start_byte t = t.start_byte
  let end_byte t = t.end_byte
end

module Attribute = struct
  type t =
    { decoded : Chatmd_ast.attribute
    ; raw_name : Byte_range.t
    ; raw_value : Byte_range.t option
    }

  let decoded t = t.decoded
  let raw_name t = t.raw_name
  let raw_value t = t.raw_value
end

module Quote = struct
  type t =
    | Single
    | Double
  [@@deriving equal, sexp_of]
end

module Pending = struct
  type t =
    | Between_attributes
    | Attribute_name of Byte_range.t
    | After_name of
        { name : string
        ; raw_name : Byte_range.t
        }
    | Before_value of
        { name : string
        ; raw_name : Byte_range.t
        }
    | Attribute_value of
        { name : string
        ; raw_value : Byte_range.t
        ; quote : Quote.t option
        ; trailing_matching_quote_escape : bool
        ; open_entity : bool
        }
    | Opaque

  let uncertain_discriminator = function
    | Between_attributes -> false
    | Attribute_name _ | After_name _ | Before_value _ | Attribute_value _ | Opaque ->
      true
  ;;
end

module Prefix = struct
  type t =
    { completed : Attribute.t list
    ; pending : Pending.t
    }

  let completed t = t.completed
  let pending t = t.pending
end

module Error = struct
  type t =
    | Invalid_range
    | Unterminated_quote of
        { name : string
        ; content_start_byte : int
        ; expected_quote : char
        }
  [@@deriving equal, sexp_of]
end

exception Malformed of Error.t

let range start_byte end_byte = { Byte_range.start_byte; end_byte }

let is_name_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | ':' | '-' -> true
  | _ -> false
;;

let entity_decoded = function
  | "amp" -> Some '&'
  | "lt" -> Some '<'
  | "gt" -> Some '>'
  | "quot" -> Some '"'
  | "apos" -> Some '\''
  | _ -> None
;;

let decode_quoted source ~start_byte ~end_byte ~checkpoint =
  let result = Buffer.create (end_byte - start_byte) in
  let cursor = ref start_byte in
  let append_raw through =
    while !cursor < through do
      checkpoint ();
      Buffer.add_char result source.[!cursor];
      Int.incr cursor
    done
  in
  while !cursor < end_byte do
    checkpoint ();
    if not (Char.equal source.[!cursor] '&')
    then (
      Buffer.add_char result source.[!cursor];
      Int.incr cursor)
    else (
      let entity_start = !cursor in
      let semicolon = ref (entity_start + 1) in
      while
        !semicolon < end_byte
        && not
             (checkpoint ();
              Char.equal source.[!semicolon] ';')
      do
        Int.incr semicolon
      done;
      if !semicolon >= end_byte
      then append_raw end_byte
      else (
        let name_start = entity_start + 1 in
        let name_length = !semicolon - name_start in
        let decoded =
          if name_length > 4
          then None
          else entity_decoded (String.sub source ~pos:name_start ~len:name_length)
        in
        match decoded with
        | Some c ->
          Buffer.add_char result c;
          cursor := !semicolon + 1
        | None -> append_raw (!semicolon + 1)))
  done;
  Buffer.contents result
;;

let scan source ~start_byte ~end_byte ~checkpoint ~prefix =
  let open Pending in
  let cursor = ref start_byte in
  let completed = ref [] in
  let pending = ref Pending.Between_attributes in
  let inspect index =
    checkpoint ();
    source.[index]
  in
  let skip_whitespace () =
    while !cursor < end_byte && Char.is_whitespace (inspect !cursor) do
      Int.incr cursor
    done
  in
  let add_attribute name raw_name value raw_value =
    completed := { Attribute.decoded = name, value; raw_name; raw_value } :: !completed;
    pending := Between_attributes
  in
  while !cursor < end_byte do
    skip_whitespace ();
    if !cursor < end_byte
    then (
      let name_start = !cursor in
      while !cursor < end_byte && is_name_char (inspect !cursor) do
        Int.incr cursor
      done;
      if Int.equal !cursor name_start
      then (
        ignore (inspect !cursor : char);
        Int.incr cursor;
        pending := Opaque)
      else (
        let name_end = !cursor in
        let name = String.sub source ~pos:name_start ~len:(name_end - name_start) in
        let raw_name = range name_start name_end in
        skip_whitespace ();
        if !cursor >= end_byte && prefix
        then
          pending
          := if Int.equal name_end end_byte
             then Attribute_name raw_name
             else After_name { name; raw_name }
        else if !cursor < end_byte && Char.equal (inspect !cursor) '='
        then (
          Int.incr cursor;
          skip_whitespace ();
          if !cursor >= end_byte
          then
            if prefix
            then pending := Before_value { name; raw_name }
            else add_attribute name raw_name None None
          else (
            let value_start = !cursor in
            let first = inspect !cursor in
            if Char.equal first '\'' || Char.equal first '"'
            then (
              let quote = if Char.equal first '\'' then Quote.Single else Quote.Double in
              Int.incr cursor;
              let content_start = !cursor in
              let open_entity = ref false in
              let trailing_matching_quote_escape = ref false in
              let closed = ref false in
              while !cursor < end_byte && not !closed do
                let c = inspect !cursor in
                if Char.equal c first
                then (
                  closed := true;
                  Int.incr cursor)
                else if
                  Char.equal c '\\'
                  && !cursor + 1 < end_byte
                  && Char.equal (inspect (!cursor + 1)) first
                then (
                  trailing_matching_quote_escape := false;
                  cursor := !cursor + 2)
                else (
                  open_entity
                  := if Char.equal c '&'
                     then true
                     else if Char.equal c ';'
                     then false
                     else !open_entity;
                  trailing_matching_quote_escape := Char.equal c '\\';
                  Int.incr cursor)
              done;
              if !closed
              then (
                let content_end = !cursor - 1 in
                let value =
                  decode_quoted
                    source
                    ~start_byte:content_start
                    ~end_byte:content_end
                    ~checkpoint
                in
                add_attribute
                  name
                  raw_name
                  (Some value)
                  (Some (range value_start !cursor)))
              else if prefix
              then
                pending
                := Attribute_value
                     { name
                     ; raw_value = range value_start end_byte
                     ; quote = Some quote
                     ; trailing_matching_quote_escape = !trailing_matching_quote_escape
                     ; open_entity = !open_entity
                     }
              else
                raise
                  (Malformed
                     (Error.Unterminated_quote
                        { name
                        ; content_start_byte = content_start
                        ; expected_quote = first
                        })))
            else (
              while !cursor < end_byte && not (Char.is_whitespace (inspect !cursor)) do
                Int.incr cursor
              done;
              if !cursor >= end_byte && prefix
              then
                pending
                := Attribute_value
                     { name
                     ; raw_value = range value_start end_byte
                     ; quote = None
                     ; trailing_matching_quote_escape = false
                     ; open_entity = false
                     }
              else
                add_attribute
                  name
                  raw_name
                  (Some (String.sub source ~pos:value_start ~len:(!cursor - value_start)))
                  (Some (range value_start !cursor)))))
        else add_attribute name raw_name None None))
  done;
  { Prefix.completed = List.rev !completed; pending = !pending }
;;

let prefix ~source ~start_byte ~end_byte ~checkpoint =
  if start_byte < 0 || end_byte < start_byte || end_byte > String.length source
  then Error Error.Invalid_range
  else Ok (scan source ~start_byte ~end_byte ~checkpoint ~prefix:true)
;;

let complete_exn ?(error_exn = fun message -> Failure message) source ~checkpoint =
  match
    scan source ~start_byte:0 ~end_byte:(String.length source) ~checkpoint ~prefix:false
  with
  | result -> List.map (Prefix.completed result) ~f:Attribute.decoded
  | exception
      Malformed (Error.Unterminated_quote { name; content_start_byte; expected_quote }) ->
    raise
      (error_exn
         (sprintf
            "Unterminated quoted attribute value for %s starting at offset %d (expected \
             %c)"
            name
            content_start_byte
            expected_quote))
  | exception Malformed Error.Invalid_range -> assert false
;;

let complete_with_ranges ~source ~start_byte ~end_byte ~checkpoint =
  if start_byte < 0 || end_byte < start_byte || end_byte > String.length source
  then Error Error.Invalid_range
  else (
    match scan source ~start_byte ~end_byte ~checkpoint ~prefix:false with
    | result -> Ok (Prefix.completed result)
    | exception Malformed error -> Error error)
;;
