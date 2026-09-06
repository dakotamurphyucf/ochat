open Core

module Cursor = struct
  type t = string [@@deriving compare, equal, sexp]

  let maximum_length = 2048

  let is_allowed_character char =
    let code = Char.to_int char in
    (not (Char.is_whitespace char)) && code >= 0x20 && code <> 0x7f
  ;;

  let is_valid_utf8 encoded =
    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String encoded) in
    let rec loop () =
      match Uutf.decode decoder with
      | `Uchar _ -> loop ()
      | `End -> true
      | `Malformed _ -> false
      | `Await -> assert false
    in
    loop ()
  ;;

  let of_string encoded =
    if String.is_empty encoded
    then Error (Protocol_error.invalid_request "page cursor must be nonempty")
    else if String.length encoded > maximum_length
    then Error (Protocol_error.invalid_request "page cursor exceeds the maximum length")
    else if not (is_valid_utf8 encoded)
    then Error (Protocol_error.invalid_request "page cursor must be valid UTF-8")
    else if not (String.for_all encoded ~f:is_allowed_character)
    then Error (Protocol_error.invalid_request "page cursor contains invalid characters")
    else Ok encoded
  ;;

  let to_string t = t
  let to_json t = `String t

  let of_json = function
    | `String encoded -> of_string encoded
    | _ -> Error (Protocol_error.invalid_request "page cursor must be a JSON string")
  ;;
end

module Request = struct
  type t =
    { limit : int
    ; cursor : Cursor.t option
    }
  [@@deriving sexp]

  let maximum_encoded_limit = 1_000_000

  let create ~limit ?cursor () =
    if limit <= 0
    then Error (Protocol_error.invalid_request "page limit must be positive")
    else if limit > maximum_encoded_limit
    then Error (Protocol_error.invalid_request "page limit exceeds the protocol maximum")
    else Ok { limit; cursor }
  ;;

  let to_fields t =
    [ Some ("limit", `Number (Int.to_string t.limit))
    ; Option.map t.cursor ~f:(fun cursor -> "cursor", Cursor.to_json cursor)
    ]
    |> List.filter_opt
  ;;

  let of_fields fields =
    let open Result.Let_syntax in
    let%bind limit =
      Json_codec.required_as
        fields
        "limit"
        (Json_codec.bounded_int ~min:1 ~max:maximum_encoded_limit)
    in
    let%bind cursor = Json_codec.optional_as fields "cursor" Cursor.of_json in
    create ~limit ?cursor ()
  ;;

  let to_json t = `Object (to_fields t)

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    of_fields fields
  ;;
end

type 'a t =
  { items : 'a list
  ; next_cursor : Cursor.t option
  }
[@@deriving sexp]

let to_json encode_item t =
  let fields = [ "items", `Array (List.map t.items ~f:encode_item) ] in
  let fields =
    match t.next_cursor with
    | None -> fields
    | Some cursor -> fields @ [ "next_cursor", Cursor.to_json cursor ]
  in
  `Object fields
;;

let of_json decode_item json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind items = Json_codec.required_as fields "items" (Json_codec.list decode_item) in
  let%map next_cursor = Json_codec.optional_as fields "next_cursor" Cursor.of_json in
  { items; next_cursor }
;;
