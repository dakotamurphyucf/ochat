open! Core
module Request = Chat_response.Model_spawn_request
module Schema = Document_schema

let ok result =
  Result.map_error result ~f:(fun _ -> "model request fixture invalid")
  |> Result.ok_or_failwith
;;

let%expect_test "shared model recipe grammar and bounded developer admission" =
  let valid =
    `Object
      [ "prompt", `String "guide"
      ; "input", `String "hello"
      ; "is_local", `True
      ; "history_compaction", `False
      ; "session_id", `String "nested"
      ; "future", `Object [ "nested", `String "preserve" ]
      ]
  in
  let decoded = Request.decode valid |> ok in
  let limits =
    Schema.Limits.create ~max_bytes:1024 ~max_depth:16 ~max_fields:32 ~max_nodes:32 |> ok
  in
  let admitted = Request.admit valid ~limits |> ok in
  assert (Schema.Json.equal (Request.original decoded) (Request.original admitted));
  assert (Schema.Json.equal valid (Request.original admitted));
  print_s
    [%sexp
      ((Request.prompt decoded, Request.input decoded, Request.is_local decoded)
       : string * string * bool)];
  print_s
    [%sexp
      ((Request.history_compaction decoded, Request.nested_prompt_id decoded)
       : bool option * string option)];
  let defaulted =
    Request.decode
      (`Object
          [ "prompt", `String "first"; "prompt", `String "second"; "input", `String "x" ])
    |> ok
  in
  print_s [%sexp ((Request.prompt defaulted, Request.is_local defaulted) : string * bool)];
  let fixtures =
    [ `Null
    ; `Object [ "input", `String "x" ]
    ; `Object [ "prompt", `True; "input", `String "x" ]
    ; `Object [ "prompt", `String "p"; "input", `String "x"; "is_local", `Null ]
    ; `Object [ "prompt", `String "p"; "input", `String "x"; "history_compaction", `Null ]
    ; `Object [ "prompt", `String "p"; "input", `String "x"; "session_id", `Null ]
    ]
  in
  List.iter fixtures ~f:(fun value ->
    match Request.decode value with
    | Error message -> print_endline message
    | Ok _ -> failwith "invalid recipe payload accepted");
  let too_small =
    Schema.Limits.create ~max_bytes:24 ~max_depth:16 ~max_fields:32 ~max_nodes:32 |> ok
  in
  (match Request.admit valid ~limits:too_small with
   | Error error -> print_endline (Agent_protocol.Error.code_to_string error.code)
   | Ok _ -> failwith "over-limit recipe admitted");
  [%expect
    {|
    (guide hello true)
    ((false) (nested))
    (first false)
    agent_prompt_v1: payload must be an object
    agent_prompt_v1: missing field "prompt"
    agent_prompt_v1: field "prompt" must be a string
    agent_prompt_v1: field "is_local" must be a bool
    field "history_compaction" must be a bool when present
    field "session_id" must be a string when present
    resource_limit |}]
;;
