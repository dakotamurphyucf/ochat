open Core

type delivery_id = Id.Delivery.t [@@deriving sexp]

module Delivery_id = Id.Delivery

module Id = struct
  type t = History_entry.Id.t [@@deriving compare, hash, sexp]

  let of_string encoded =
    Result.map_error (History_entry.Id.of_string encoded) ~f:(fun message ->
      Protocol_error.invalid_request message)
  ;;

  let to_string = History_entry.Id.to_string
  let to_json id = `String (to_string id)

  let of_json = function
    | `String encoded -> of_string encoded
    | _ -> Error (Protocol_error.invalid_request "history ID must be a JSON string")
  ;;
end

type role =
  | System
  | User
  | Assistant
  | Tool
[@@deriving compare, equal, sexp]

type kind =
  | Message
  | Reasoning
  | Tool_call
  | Tool_output
  | Other
[@@deriving compare, equal, sexp]

type provenance =
  | Canonical
  | Moderator_inserted
  | Moderator_replaced of Id.t
  | Runtime_notification of delivery_id
[@@deriving sexp]

type entry =
  { id : Id.t
  ; role : role
  ; kind : kind
  ; payload : Jsonaf.t
  ; provenance : provenance
  ; redacted : bool
  }
[@@deriving sexp]

let role_to_string = function
  | System -> "system"
  | User -> "user"
  | Assistant -> "assistant"
  | Tool -> "tool"
;;

let role_of_json =
  Json_codec.enum
    ~name:"history role"
    [ "system", System; "user", User; "assistant", Assistant; "tool", Tool ]
;;

let kind_to_string = function
  | Message -> "message"
  | Reasoning -> "reasoning"
  | Tool_call -> "tool_call"
  | Tool_output -> "tool_output"
  | Other -> "other"
;;

let kind_of_json =
  Json_codec.enum
    ~name:"history entry kind"
    [ "message", Message
    ; "reasoning", Reasoning
    ; "tool_call", Tool_call
    ; "tool_output", Tool_output
    ; "other", Other
    ]
;;

let provenance_to_json = function
  | Canonical -> `Object [ "type", `String "canonical" ]
  | Moderator_inserted -> `Object [ "type", `String "moderator_inserted" ]
  | Runtime_notification id ->
    `Object
      [ "type", `String "runtime_notification"; "delivery_id", Delivery_id.to_json id ]
  | Moderator_replaced id ->
    `Object [ "type", `String "moderator_replaced"; "canonical_id", Id.to_json id ]
;;

let provenance_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind encoded = Json_codec.required_as fields "type" Json_codec.string in
  match encoded with
  | "canonical" -> Ok Canonical
  | "moderator_inserted" -> Ok Moderator_inserted
  | "runtime_notification" ->
    Result.map
      (Json_codec.required_as fields "delivery_id" Delivery_id.of_json)
      ~f:(fun id -> Runtime_notification id)
  | "moderator_replaced" ->
    Result.map (Json_codec.required_as fields "canonical_id" Id.of_json) ~f:(fun id ->
      Moderator_replaced id)
  | _ -> Error (Protocol_error.invalid_request "unknown history provenance")
;;

let entry_to_json entry =
  `Object
    [ "id", Id.to_json entry.id
    ; "role", `String (role_to_string entry.role)
    ; "kind", `String (kind_to_string entry.kind)
    ; "payload", entry.payload
    ; "provenance", provenance_to_json entry.provenance
    ; ("redacted", if entry.redacted then `True else `False)
    ]
;;

let entry_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind id = Json_codec.required_as fields "id" Id.of_json in
  let%bind role = Json_codec.required_as fields "role" role_of_json in
  let%bind kind = Json_codec.required_as fields "kind" kind_of_json in
  let%bind payload = Json_codec.required fields "payload" in
  let%bind provenance = Json_codec.required_as fields "provenance" provenance_of_json in
  let%map redacted = Json_codec.required_as fields "redacted" Json_codec.bool in
  { id; role; kind; payload; provenance; redacted }
;;

module Window_request = struct
  type position =
    | Tail of int
    | After of Id.t
    | Before of Id.t
    | Cursor of Page.Cursor.t
  [@@deriving sexp]

  type t =
    { position : position
    ; limit : int
    ; effective : bool
    }
  [@@deriving sexp]

  let position_fields = function
    | Tail count ->
      [ "position", `String "tail"; "tail_count", `Number (Int.to_string count) ]
    | After id -> [ "position", `String "after"; "history_id", Id.to_json id ]
    | Before id -> [ "position", `String "before"; "history_id", Id.to_json id ]
    | Cursor cursor ->
      [ "position", `String "cursor"; "window_cursor", Page.Cursor.to_json cursor ]
  ;;

  let to_json t =
    `Object
      (position_fields t.position
       @ [ "limit", `Number (Int.to_string t.limit)
         ; ("effective", if t.effective then `True else `False)
         ])
  ;;

  let decode_position fields =
    let open Result.Let_syntax in
    let%bind encoded = Json_codec.required_as fields "position" Json_codec.string in
    match encoded with
    | "tail" ->
      Result.map
        (Json_codec.required_as
           fields
           "tail_count"
           (Json_codec.bounded_int ~min:1 ~max:1_000_000))
        ~f:(fun count -> Tail count)
    | "after" ->
      Result.map (Json_codec.required_as fields "history_id" Id.of_json) ~f:(fun id ->
        After id)
    | "before" ->
      Result.map (Json_codec.required_as fields "history_id" Id.of_json) ~f:(fun id ->
        Before id)
    | "cursor" ->
      Result.map
        (Json_codec.required_as fields "window_cursor" Page.Cursor.of_json)
        ~f:(fun cursor -> Cursor cursor)
    | _ -> Error (Protocol_error.invalid_request "unknown history window position")
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind position = decode_position fields in
    let%bind limit =
      Json_codec.required_as fields "limit" (Json_codec.bounded_int ~min:1 ~max:1_000_000)
    in
    let%map effective = Json_codec.required_as fields "effective" Json_codec.bool in
    { position; limit; effective }
  ;;
end

module Window = struct
  type t =
    { entries : entry list
    ; previous_cursor : Page.Cursor.t option
    ; next_cursor : Page.Cursor.t option
    ; reached_start : bool
    ; reached_end : bool
    ; structurally_complete : bool
    }
  [@@deriving sexp]

  let optional_field name value encode =
    Option.map value ~f:(fun value -> name, encode value)
  ;;

  let to_json t =
    let fields =
      [ Some ("entries", `Array (List.map t.entries ~f:entry_to_json))
      ; optional_field "previous_cursor" t.previous_cursor Page.Cursor.to_json
      ; optional_field "next_cursor" t.next_cursor Page.Cursor.to_json
      ; Some ("reached_start", if t.reached_start then `True else `False)
      ; Some ("reached_end", if t.reached_end then `True else `False)
      ; Some ("structurally_complete", if t.structurally_complete then `True else `False)
      ]
      |> List.filter_opt
    in
    `Object fields
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind entries =
      Json_codec.required_as fields "entries" (Json_codec.list entry_of_json)
    in
    let%bind previous_cursor =
      Json_codec.optional_as fields "previous_cursor" Page.Cursor.of_json
    in
    let%bind next_cursor =
      Json_codec.optional_as fields "next_cursor" Page.Cursor.of_json
    in
    let%bind reached_start =
      Json_codec.required_as fields "reached_start" Json_codec.bool
    in
    let%bind reached_end = Json_codec.required_as fields "reached_end" Json_codec.bool in
    let%map structurally_complete =
      Json_codec.required_as fields "structurally_complete" Json_codec.bool
    in
    { entries
    ; previous_cursor
    ; next_cursor
    ; reached_start
    ; reached_end
    ; structurally_complete
    }
  ;;
end
