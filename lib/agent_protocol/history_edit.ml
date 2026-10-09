open! Core
module Error = Protocol_error

module Mode = struct
  type t =
    | Save_only
    | Edit_and_continue
  [@@deriving equal, sexp]
end

module Unsupported_target = struct
  type t =
    | Not_plain_user_text
    | Overlay_override
    | Tool_pair_crosses_boundary
    | Initial_instruction
  [@@deriving equal, sexp]

  let to_json = function
    | Not_plain_user_text -> `String "not_plain_user_text"
    | Overlay_override -> `String "overlay_override"
    | Tool_pair_crosses_boundary -> `String "tool_pair_crosses_boundary"
    | Initial_instruction -> `String "initial_instruction"
  ;;

  let of_json =
    Json_codec.enum
      ~name:"unsupported history edit target"
      [ "not_plain_user_text", Not_plain_user_text
      ; "overlay_override", Overlay_override
      ; "tool_pair_crosses_boundary", Tool_pair_crosses_boundary
      ; "initial_instruction", Initial_instruction
      ]
  ;;
end

type t =
  { history_id : History.Id.t
  ; expected_content_revision : History.Content_revision.t
  ; text : string
  ; mode : Mode.t
  }
[@@deriving sexp]

let create ~history_id ~expected_content_revision ~text ~mode =
  if String.length text > 1_048_576 || not (Stdlib.String.is_valid_utf_8 text)
  then Error (Error.invalid_request "history replacement must be bounded UTF-8 text")
  else Ok { history_id; expected_content_revision; text; mode }
;;

let unchecked_of_sexp = t_of_sexp

let t_of_sexp sexp =
  let value = unchecked_of_sexp sexp in
  match
    create
      ~history_id:value.history_id
      ~expected_content_revision:value.expected_content_revision
      ~text:value.text
      ~mode:value.mode
  with
  | Ok value -> value
  | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
;;

let history_id t = t.history_id
let expected_content_revision t = t.expected_content_revision
let text t = t.text
let mode t = t.mode

let to_json t =
  `Object
    [ "history_id", History.Id.to_json t.history_id
    ; ( "expected_content_revision"
      , History.Content_revision.to_json t.expected_content_revision )
    ; "text", `String t.text
    ; ( "mode"
      , `String
          (match t.mode with
           | Save_only -> "save_only"
           | Edit_and_continue -> "edit_and_continue") )
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = Json_codec.validate_limits ~max_depth:8 ~max_bytes:1_049_600 json in
  let%bind fields = Json_codec.fields json in
  let%bind history_id = Json_codec.required_as fields "history_id" History.Id.of_json in
  let%bind expected_content_revision =
    Json_codec.required_as
      fields
      "expected_content_revision"
      History.Content_revision.of_json
  in
  let%bind text = Json_codec.required_as fields "text" Json_codec.string in
  let%bind mode =
    Json_codec.required_as
      fields
      "mode"
      (Json_codec.enum
         ~name:"history edit mode"
         [ "save_only", Mode.Save_only; "edit_and_continue", Edit_and_continue ])
  in
  create ~history_id ~expected_content_revision ~text ~mode
;;

module Continuation = struct
  type unavailable =
    | Stopped
    | Runtime_unavailable
  [@@deriving equal, sexp]

  type t =
    | Not_requested
    | Started of Id.Operation.t
    | Not_started of unavailable
  [@@deriving equal, sexp]

  let to_json = function
    | Not_requested -> `Object [ "type", `String "not_requested" ]
    | Started id ->
      `Object [ "type", `String "started"; "operation_id", Id.Operation.to_json id ]
    | Not_started reason ->
      `Object
        [ "type", `String "not_started"
        ; ( "reason"
          , `String
              (match reason with
               | Stopped -> "stopped"
               | Runtime_unavailable -> "runtime_unavailable") )
        ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind kind = Json_codec.required_as fields "type" Json_codec.string in
    match kind with
    | "not_requested" -> Ok Not_requested
    | "started" ->
      Json_codec.required_as fields "operation_id" Id.Operation.of_json
      |> Result.map ~f:(fun id -> Started id)
    | "not_started" ->
      Json_codec.required_as
        fields
        "reason"
        (Json_codec.enum
           ~name:"continuation unavailable reason"
           [ "stopped", Stopped; "runtime_unavailable", Runtime_unavailable ])
      |> Result.map ~f:(fun reason -> Not_started reason)
    | _ -> Error (Error.invalid_request "unknown history continuation disposition")
  ;;
end

module Continue_request = struct
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_generation : int
    ; expected_revision : int64
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "session_id", Id.Session.to_json t.session_id
      ; "attachment_id", Id.Attachment.to_json t.attachment_id
      ; "expected_generation", `Number (Int.to_string t.expected_generation)
      ; "expected_revision", `Number (Int64.to_string t.expected_revision)
      ; "idempotency_key", Idempotency_key.to_json t.idempotency_key
      ]
  ;;

  let of_fields fields =
    let open Result.Let_syntax in
    let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
    let%bind attachment_id =
      Json_codec.required_as fields "attachment_id" Id.Attachment.of_json
    in
    let%bind expected_generation =
      Json_codec.required_as
        fields
        "expected_generation"
        (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
    in
    let%bind expected_revision =
      Json_codec.required_as
        fields
        "expected_revision"
        (Json_codec.bounded_int64 ~min:0L ~max:Int64.max_value)
    in
    let%map idempotency_key =
      Json_codec.required_as fields "idempotency_key" Idempotency_key.of_json
    in
    { session_id; attachment_id; expected_generation; expected_revision; idempotency_key }
  ;;

  let of_json json =
    let%bind.Result fields = Json_codec.fields json in
    of_fields fields
  ;;

  let unchecked_of_sexp = t_of_sexp

  let t_of_sexp sexp =
    let value = unchecked_of_sexp sexp in
    match of_json (to_json value) with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end

module Edit_request = struct
  type intent = t [@@deriving sexp]

  let intent_to_json = to_json
  let intent_of_json = of_json

  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_generation : int
    ; expected_revision : int64
    ; edit : intent
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    let basis : Continue_request.t =
      { session_id = t.session_id
      ; attachment_id = t.attachment_id
      ; expected_generation = t.expected_generation
      ; expected_revision = t.expected_revision
      ; idempotency_key = t.idempotency_key
      }
    in
    match Continue_request.to_json basis with
    | `Object fields -> `Object (fields @ [ "edit", intent_to_json t.edit ])
    | _ -> assert false
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = Json_codec.validate_limits ~max_depth:12 ~max_bytes:1_051_000 json in
    let%bind fields = Json_codec.fields json in
    let%bind basis = Continue_request.of_fields fields in
    let%map edit = Json_codec.required_as fields "edit" intent_of_json in
    { session_id = basis.session_id
    ; attachment_id = basis.attachment_id
    ; expected_generation = basis.expected_generation
    ; expected_revision = basis.expected_revision
    ; idempotency_key = basis.idempotency_key
    ; edit
    }
  ;;

  let unchecked_of_sexp = t_of_sexp

  let t_of_sexp sexp =
    let value = unchecked_of_sexp sexp in
    match of_json (to_json value) with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end
