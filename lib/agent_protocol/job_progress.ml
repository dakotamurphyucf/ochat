open Core

type channel =
  | Assistant
  | Reasoning
  | Stdout
  | Stderr
  | Activity
[@@deriving compare, equal, sexp]

type item =
  { channel : channel
  ; text : string
  ; truncated : bool
  }
[@@deriving sexp]

type t =
  { sequence : int
  ; channels : item list
  }
[@@deriving sexp]

let max_update_bytes = 4096
let max_channel_bytes = 8192
let max_updates = 4096

let names =
  [ "assistant", Assistant
  ; "reasoning", Reasoning
  ; "stdout", Stdout
  ; "stderr", Stderr
  ; "activity", Activity
  ]
;;

let to_json t =
  `Object
    [ "version", `Number "1"
    ; "sequence", `Number (Int.to_string t.sequence)
    ; ( "channels"
      , `Array
          (List.map t.channels ~f:(fun item ->
             `Object
               [ ( "channel"
                 , `String
                     (List.Assoc.find_exn
                        (List.map names ~f:(fun (name, channel) -> channel, name))
                        item.channel
                        ~equal:equal_channel) )
               ; "text", `String item.text
               ; ("truncated", if item.truncated then `True else `False)
               ])) )
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = Json_codec.validate_limits ~max_bytes:300_000 ~max_depth:8 json in
  let%bind fields = Json_codec.fields json in
  let%bind () = Extension_codec.closed fields [ "version"; "sequence"; "channels" ] in
  let%bind version =
    Json_codec.required_as
      fields
      "version"
      (Json_codec.bounded_int ~min:1 ~max:Int.max_value)
  in
  let%bind () =
    match version with
    | 1 -> Ok ()
    | _ -> Error (Protocol_error.invalid_request "unsupported job progress version")
  in
  let%bind sequence =
    Json_codec.required_as
      fields
      "sequence"
      (Json_codec.bounded_int ~min:1 ~max:max_updates)
  in
  let%bind values =
    Json_codec.required_as fields "channels" (function
      | `Array values -> Ok values
      | _ -> Error (Protocol_error.invalid_request "progress channels must be an array"))
  in
  let%bind () =
    match List.length values <= 5 with
    | true -> Ok ()
    | false -> Error (Protocol_error.invalid_request "too many progress channels")
  in
  let%bind channels =
    List.map values ~f:(fun value ->
      let%bind fields = Json_codec.fields value in
      let%bind () = Extension_codec.closed fields [ "channel"; "text"; "truncated" ] in
      let%bind channel =
        Json_codec.required_as
          fields
          "channel"
          (Json_codec.enum ~name:"progress channel" names)
      in
      let%bind text = Json_codec.required_as fields "text" Json_codec.string in
      let%bind () =
        match
          String.length text <= max_channel_bytes && Stdlib.String.is_valid_utf_8 text
        with
        | true -> Ok ()
        | false -> Error (Protocol_error.invalid_request "invalid progress text")
      in
      let%map truncated =
        Json_codec.required_as fields "truncated" (function
          | `True -> Ok true
          | `False -> Ok false
          | _ -> Error (Protocol_error.invalid_request "invalid progress truncation flag"))
      in
      { channel; text; truncated })
    |> Result.all
  in
  match
    List.contains_dup
      (List.map channels ~f:(fun item -> item.channel))
      ~compare:compare_channel
  with
  | true -> Error (Protocol_error.invalid_request "duplicate progress channel")
  | false -> Ok { sequence; channels }
;;
