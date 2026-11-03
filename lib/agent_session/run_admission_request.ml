open! Core
module P = Agent_protocol

module Caller = struct
  type t =
    | Writer of P.Id.Attachment.t
    | Headless_command
  [@@deriving equal, sexp]
end

module Headless = struct
  type t =
    { session_id : P.Id.Session.t
    ; generation : int
    ; expected_revision : int64
    ; mode : P.Run.Mode.t
    ; input : P.Run_start.Input.t
    ; key : P.Idempotency_key.t
    }

  let create ~session_id ~generation ~expected_revision ~mode ~input ~key =
    let open Result.Let_syntax in
    let%bind _ = P.Id.Session.of_json (P.Id.Session.to_json session_id) in
    let%bind _ = P.Idempotency_key.of_string (P.Idempotency_key.to_string key) in
    let%bind _ = P.Run_start.Input.of_json (P.Run_start.Input.to_json input) in
    if generation < 0 || Int64.(expected_revision < 0L)
    then Error (P.Error.invalid_request "invalid headless run generation or revision")
    else Ok { session_id; generation; expected_revision; mode; input; key }
  ;;
end

type t =
  { session_id : P.Id.Session.t
  ; generation : int
  ; expected_revision : int64
  ; mode : P.Run.Mode.t
  ; input : P.Run_start.Input.t
  ; key : P.Idempotency_key.t
  ; caller : Caller.t
  }

let of_writer (request : P.Run_start.t) =
  { session_id = request.session_id
  ; generation = request.generation
  ; expected_revision = request.expected_revision
  ; mode = request.mode
  ; input = request.input
  ; key = request.key
  ; caller = Writer request.attachment_id
  }
;;

let of_headless (request : Headless.t) =
  { session_id = request.session_id
  ; generation = request.generation
  ; expected_revision = request.expected_revision
  ; mode = request.mode
  ; input = request.input
  ; key = request.key
  ; caller = Headless_command
  }
;;

let request_sha256 t =
  let open Result.Let_syntax in
  let%bind json =
    match t.caller with
    | Writer attachment_id ->
      let%map request =
        P.Run_start.create
          ~session_id:t.session_id
          ~attachment_id
          ~generation:t.generation
          ~expected_revision:t.expected_revision
          ~mode:t.mode
          ~input:t.input
          ~key:t.key
      in
      P.Run_start.to_json request
    | Headless_command ->
      Ok
        (`Object
            [ "kind", `String "headless_command"
            ; "session_id", P.Id.Session.to_json t.session_id
            ; "generation", `Number (Int.to_string t.generation)
            ; "expected_revision", `String (Int64.to_string t.expected_revision)
            ; "mode", P.Run.Mode.to_json t.mode
            ; "input", P.Run_start.Input.to_json t.input
            ; "key", `String (P.Idempotency_key.to_string t.key)
            ])
  in
  let%map encoded = P.Json_codec.canonical_string json in
  Digestif.SHA256.(digest_string encoded |> to_hex)
;;
