open Core

type t =
  { revision : int64
  ; latest_event_sequence : int64
  }
[@@deriving sexp]

let nonnegative_int64 = Json_codec.bounded_int64 ~min:Int64.zero ~max:Int64.max_value

let to_fields t =
  [ "revision", `Number (Int64.to_string t.revision)
  ; "latest_event_sequence", `Number (Int64.to_string t.latest_event_sequence)
  ]
;;

let of_fields fields =
  let open Result.Let_syntax in
  let%bind revision = Json_codec.required_as fields "revision" nonnegative_int64 in
  let%map latest_event_sequence =
    Json_codec.required_as fields "latest_event_sequence" nonnegative_int64
  in
  { revision; latest_event_sequence }
;;

let to_json t = `Object (to_fields t)

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  of_fields fields
;;
