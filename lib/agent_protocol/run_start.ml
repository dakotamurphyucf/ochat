open! Core
module J = Json_codec

module Input = struct
  type t =
    | User_submission of Session.Message_content.t
    | Authored_start

  let to_json = function
    | Authored_start -> `Object [ "kind", `String "authored_start" ]
    | User_submission content ->
      `Object
        [ "kind", `String "user_submission"
        ; "content", Session.Message_content.to_json content
        ]
  ;;

  let validate = function
    | Authored_start -> Ok ()
    | User_submission content ->
      let open Result.Let_syntax in
      let%bind () = Run_limits.check_count (List.length content.attachments) in
      if String.length content.text > Run_limits.max_document_bytes
      then Error (Protocol_error.invalid_request "run submission exceeds byte bound")
      else if String.is_empty content.text && List.is_empty content.attachments
      then Error (Protocol_error.invalid_request "user run submission requires input")
      else (
        let json = Session.Message_content.to_json content in
        let%bind () =
          Extension_codec.validate_json
            ~max_bytes:Run_limits.max_document_bytes
            ~max_depth:Run_limits.max_depth
            json
        in
        Result.map (Session.Message_content.of_json json) ~f:(fun _ -> ()))
  ;;

  let bounded_content json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind () =
      J.required_as fields "attachments" (function
        | `Array items -> Run_limits.check_count (List.length items)
        | _ -> Error (Protocol_error.invalid_request "run attachments must be an array"))
    in
    Session.Message_content.of_json json
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () =
      Extension_codec.validate_json
        ~max_bytes:Run_limits.max_document_bytes
        ~max_depth:Run_limits.max_depth
        json
    in
    let%bind fields = J.fields json in
    let%bind kind = J.required_as fields "kind" J.string in
    let%bind value =
      match kind with
      | "authored_start" -> Ok Authored_start
      | "user_submission" ->
        Result.map (J.required_as fields "content" bounded_content) ~f:(fun content ->
          User_submission content)
      | _ -> Error (Protocol_error.invalid_request "unsupported run start input")
    in
    Result.map (validate value) ~f:(fun () -> value)
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

  let t_of_sexp sexp =
    match of_json (Jsonaf.t_of_sexp sexp) with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end

type t =
  { session_id : Id.Session.t
  ; attachment_id : Id.Attachment.t
  ; generation : int
  ; expected_revision : int64
  ; mode : Run.Mode.t
  ; input : Input.t
  ; key : Idempotency_key.t
  }

let create
      ~session_id
      ~attachment_id
      ~generation
      ~expected_revision
      ~(mode : Run.Mode.t)
      ~input
      ~key
  =
  let open Result.Let_syntax in
  let%bind () =
    Extension_codec.validate_id Id.Session.to_json Id.Session.of_json session_id
  in
  let%bind () =
    Extension_codec.validate_id Id.Attachment.to_json Id.Attachment.of_json attachment_id
  in
  let%bind _ = Idempotency_key.of_string (Idempotency_key.to_string key) in
  let%bind () = Input.validate input in
  if generation < 0 || Int64.(expected_revision < 0L)
  then Error (Protocol_error.invalid_request "invalid run start generation or revision")
  else (
    match mode, input with
    | Single_turn, Authored_start ->
      Error (Protocol_error.invalid_request "authored start requires workflow mode")
    | Single_turn, User_submission _ | Workflow, (Authored_start | User_submission _) ->
      Ok { session_id; attachment_id; generation; expected_revision; mode; input; key })
;;

let to_json t =
  `Object
    [ "session_id", Id.Session.to_json t.session_id
    ; "attachment_id", Id.Attachment.to_json t.attachment_id
    ; "generation", `Number (Int.to_string t.generation)
    ; "expected_revision", `String (Int64.to_string t.expected_revision)
    ; "mode", Run.Mode.to_json t.mode
    ; "input", Input.to_json t.input
    ; "key", Idempotency_key.to_json t.key
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () =
    Extension_codec.validate_json
      ~max_bytes:Run_limits.max_document_bytes
      ~max_depth:Run_limits.max_depth
      json
  in
  let%bind fields = J.fields json in
  let%bind session_id = J.required_as fields "session_id" Id.Session.of_json in
  let%bind attachment_id = J.required_as fields "attachment_id" Id.Attachment.of_json in
  let%bind generation =
    J.required_as fields "generation" (J.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind expected_revision =
    J.required_as fields "expected_revision" History.Content_revision.of_json
  in
  let%bind mode = J.required_as fields "mode" Run.Mode.of_json in
  let%bind input = J.required_as fields "input" Input.of_json in
  let%bind key = J.required_as fields "key" Idempotency_key.of_json in
  create
    ~session_id
    ~attachment_id
    ~generation
    ~expected_revision:(History.Content_revision.to_int64 expected_revision)
    ~mode
    ~input
    ~key
;;

let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

let t_of_sexp sexp =
  match of_json (Jsonaf.t_of_sexp sexp) with
  | Ok value -> value
  | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
;;
