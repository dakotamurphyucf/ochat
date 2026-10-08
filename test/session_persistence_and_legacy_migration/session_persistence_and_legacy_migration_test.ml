open! Core
module D = Document_schema

let schema_exn = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

let session () = Session.create ~id:"session" ~prompt_file:"prompt.chatmd" ()
let json session = Session.Document.encode session |> schema_exn |> D.Document.json

let decode json =
  D.Document.inspect ~limits:D.Limits.default json
  |> schema_exn
  |> Session.Document.decode
  |> schema_exn
;;

let update json field ~f =
  match json with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (name, value) ->
         name, if String.equal field name then f value else value))
  | _ -> failwith "expected object"
;;

let add json field value =
  match json with
  | `Object fields -> `Object (fields @ [ field, value ])
  | _ -> failwith "expected object"
;;

let field json name =
  match D.Json.field json ~name with
  | D.Json.Value value -> value
  | D.Json.Null -> `Null
  | D.Json.Absent -> failwith ("missing " ^ name)
;;

let payload json = field json "payload"

let%expect_test "complete standalone document has explicit null and decimal watermark" =
  let encoded = json (session ()) in
  print_s
    [%sexp
      { kind =
          (D.Json.field encoded ~name:"kind"
           |> function
           | D.Json.Value (`String value) -> value
           | _ -> "invalid"
           : string)
      ; version = ((decode encoded).version : int)
      ; null_copy =
          (D.Json.equal (field (payload encoded) "local_prompt_copy") `Null : bool)
      ; watermark =
          (D.Json.equal (field (payload encoded) "next_history_sequence") (`String "0")
           : bool)
      }];
  [%expect
    {| ((kind standalone.session) (version 1) (null_copy true) (watermark true)) |}]
;;

let%expect_test
    "outer, payload and keyed task unknowns survive unrelated edits and reorder"
  =
  let tasks =
    [ Session.Task.create ~id:"a" ~title:"first" ()
    ; Session.Task.create ~id:"b" ~title:"second" ()
    ]
  in
  let initial = Session.create ~id:"session" ~prompt_file:"prompt" ~tasks () in
  let encoded =
    json initial
    |> fun json ->
    add json "future_envelope" (`String "kept")
    |> fun json ->
    update json "payload" ~f:(fun json ->
      add json "future_payload" (`Object [ "nested", `Null ])
      |> fun json ->
      update json "tasks" ~f:(function
        | `Array (first :: rest) ->
          `Array (add first "future_task" (`String "belongs to a") :: rest)
        | _ -> failwith "tasks"))
  in
  let restored = decode encoded in
  let edited =
    { restored with prompt_file = "edited"; tasks = List.rev restored.tasks }
  in
  let output = json edited in
  let task_a =
    field (payload output) "tasks"
    |> function
    | `Array [ _; a ] -> a
    | _ -> failwith "order"
  in
  print_s
    [%sexp
      { envelope = (D.Json.equal (field output "future_envelope") (`String "kept") : bool)
      ; payload =
          (D.Json.equal
             (field (payload output) "future_payload")
             (`Object [ "nested", `Null ])
           : bool)
      ; identity =
          (D.Json.equal (field task_a "future_task") (`String "belongs to a") : bool)
      }];
  [%expect {| ((envelope true) (payload true) (identity true)) |}]
;;

let%expect_test "deleting an unknown-bearing task fails closed" =
  let initial =
    Session.create
      ~id:"session"
      ~prompt_file:"prompt"
      ~tasks:[ Session.Task.create ~id:"a" ~title:"first" () ]
      ()
  in
  let restored =
    json initial
    |> fun json ->
    update json "payload" ~f:(fun json ->
      update json "tasks" ~f:(function
        | `Array [ task ] -> `Array [ add task "future" `True ]
        | _ -> assert false))
    |> decode
  in
  print_s
    [%sexp
      (Session.Document.encode { restored with tasks = [] } |> Result.map ~f:(fun _ -> ())
       : (unit, D.Error.t) Result.t)];
  [%expect {| (Error (Extension_conflict (payload tasks a))) |}]
;;

let%expect_test "nested ChatML record field extensions survive state edits" =
  let snapshot =
    Session.Moderator_snapshot.create
      ~script_id:"mod"
      ~script_source_hash:"hash"
      ~current_state:(Session.Snapshot.Record [ "count", Int 1; "label", String "one" ])
      ()
  in
  let initial =
    Session.create ~id:"session" ~prompt_file:"prompt" ~moderator_snapshot:snapshot ()
  in
  let original =
    json initial
    |> fun json ->
    update json "payload" ~f:(fun json ->
      update json "moderator_state" ~f:(fun json ->
        update json "legacy_snapshot" ~f:(fun json ->
          update json "current_state" ~f:(fun json ->
            update json "fields" ~f:(function
              | `Array (first :: rest) ->
                `Array (add first "future" (`String "retained") :: rest)
              | _ -> assert false)))))
  in
  let restored = decode original in
  let old = Option.value_exn restored.moderator_state.legacy_snapshot in
  let edited =
    { restored with
      moderator_state =
        { restored.moderator_state with
          legacy_snapshot =
            Some
              { old with
                current_state =
                  Session.Snapshot.Record [ "label", String "two"; "count", Int 2 ]
              }
        }
    }
  in
  let state =
    json edited
    |> payload
    |> fun x ->
    field x "moderator_state"
    |> fun x -> field x "legacy_snapshot" |> fun x -> field x "current_state"
  in
  let retained =
    match field state "fields" with
    | `Array [ _; count ] -> D.Json.equal (field count "future") (`String "retained")
    | _ -> false
  in
  print_s [%sexp (retained : bool)];
  [%expect {| true |}]
;;

let%expect_test "tagged ownership does not consume fields from another case" =
  let initial =
    Session.create
      ~id:"session"
      ~prompt_file:"prompt"
      ~moderator_snapshot:
        (Session.Moderator_snapshot.create ~script_id:"mod" ~script_source_hash:"hash" ())
      ()
  in
  let encoded =
    json initial
    |> fun json ->
    update json "payload" ~f:(fun json ->
      update json "moderator_state" ~f:(fun json ->
        update json "legacy_snapshot" ~f:(fun json ->
          update json "current_state" ~f:(fun json ->
            add json "values" (`String "future unit extension")))))
  in
  let restored = decode encoded in
  let same = D.Json.equal encoded (json restored) in
  let old = Option.value_exn restored.moderator_state.legacy_snapshot in
  let changed =
    { restored with
      moderator_state =
        { restored.moderator_state with
          legacy_snapshot = Some { old with current_state = Session.Snapshot.Array [] }
        }
    }
  in
  print_s
    [%sexp
      { same : bool
      ; rejects_kind_change = (Result.is_error (Session.Document.encode changed) : bool)
      }];
  [%expect {| ((same true) (rejects_kind_change true)) |}]
;;

let%expect_test "missing differs from null; malformed and old beta input is rejected" =
  let without_copy =
    json (session ())
    |> fun json ->
    update json "payload" ~f:(function
      | `Object fields ->
        `Object
          (List.filter fields ~f:(fun (name, _) ->
             not (String.equal name "local_prompt_copy")))
      | _ -> assert false)
  in
  let missing =
    D.Document.inspect ~limits:D.Limits.default without_copy
    |> schema_exn
    |> Session.Document.decode
    |> Result.is_error
  in
  let unsupported = Session.Document.of_string "\000\001old-beta" in
  print_s
    [%sexp
      { missing : bool
      ; explicit_null =
          (Result.is_ok
             (Session.Document.of_string (Jsonaf.to_string (json (session ()))))
           : bool)
      }];
  print_s [%sexp (Result.map unsupported ~f:(fun _ -> ()) : (unit, D.Error.t) Result.t)];
  [%expect
    {|
    ((missing true) (explicit_null true))
    (Error Unsupported_beta_format)
    |}]
;;

let%expect_test "invalid allocator, duplicate fields and nonfinite script state fail" =
  let bad = { (session ()) with next_history_sequence = -1 } in
  let duplicate =
    json (session ()) |> fun json -> add json "kind" (`String "standalone.session")
  in
  let finite =
    Session.Snapshot.of_jsonaf
      (`Object [ "kind", `String "float"; "value", `String "nan" ])
  in
  print_s
    [%sexp
      { allocator = (Result.is_error (Session.Document.encode bad) : bool)
      ; duplicate =
          (Result.is_error (D.Document.inspect ~limits:D.Limits.default duplicate) : bool)
      ; nonfinite = (Result.is_error finite : bool)
      }];
  [%expect {| ((allocator true) (duplicate true) (nonfinite true)) |}]
;;

let%expect_test "empty string dictionary keys remain valid and retain extensions" =
  let initial =
    Session.create
      ~id:"session"
      ~prompt_file:"prompt"
      ~kv_store:[ "", "value"; "named", "other" ]
      ~moderator_snapshot:
        (Session.Moderator_snapshot.create
           ~script_id:"mod"
           ~script_source_hash:"hash"
           ~current_state:(Session.Snapshot.Record [ "", String "empty-key" ])
           ())
      ()
  in
  let encoded =
    json initial
    |> fun json ->
    update json "payload" ~f:(fun json ->
      update json "kv_store" ~f:(function
        | `Array (first :: rest) -> `Array (add first "future" `True :: rest)
        | _ -> assert false))
  in
  let restored = decode encoded in
  let output = json { restored with kv_store = List.rev restored.kv_store } in
  let retained =
    match field (payload output) "kv_store" with
    | `Array [ _; empty ] -> D.Json.equal (field empty "future") `True
    | _ -> false
  in
  print_s [%sexp { retained : bool; count = (List.length restored.kv_store : int) }];
  [%expect {| ((retained true) (count 2)) |}]
;;

let%expect_test "decoding does not consume randomness" =
  let encoded = json (session ()) in
  let before = Random.State.bits (Random.State.copy Random.State.default) in
  ignore (decode encoded : Session.t);
  let after = Random.State.bits (Random.State.copy Random.State.default) in
  print_s [%sexp (Int.equal before after : bool)];
  [%expect {| true |}]
;;

let%expect_test "pure session validation checks every moderator value" =
  let initial = session () in
  let invalid extensions legacy_snapshot =
    Session.validate
      { initial with
        moderator_state = { initial.moderator_state with extensions; legacy_snapshot }
      }
    |> Result.is_error
  in
  let legacy =
    Session.Moderator_snapshot.create
      ~script_id:"mod"
      ~script_source_hash:"hash"
      ~queued_internal_events:[ Array [ Float Float.infinity ] ]
      ()
  in
  print_s
    [%sexp
      { duplicate_names = (invalid [ "x", Unit; "x", Unit ] None : bool)
      ; invalid_extension =
          (invalid [ "x", Record [ "nested", Float Float.nan ] ] None : bool)
      ; invalid_legacy = (invalid [] (Some legacy) : bool)
      }];
  [%expect {| ((duplicate_names true) (invalid_extension true) (invalid_legacy true)) |}]
;;

let%expect_test "moderator counters and shell audit watermarks validate on admission" =
  let base : Session.Moderator_state.Identity_snapshot.t =
    { script_id = "mod"
    ; script_source_hash = "hash"
    ; current_state = Unit
    ; queued_internal_events = []
    ; halted = false
    ; revision = 0
    ; next_change_id = 0
    ; prepended_items = []
    ; appended_items = []
    ; replacements = []
    ; tombstones = []
    ; halted_reason = None
    }
  in
  let negative =
    Session.Moderator_state.Identity_snapshot.to_jsonaf { base with revision = -1 }
    |> Session.Moderator_state.Identity_snapshot.of_jsonaf
    |> Result.is_error
  in
  let id =
    History_entry.Id.create ~namespace:"session" ~sequence:0 |> Result.ok_or_failwith
  in
  let reused_watermark =
    Session.Moderator_state.Identity_snapshot.to_jsonaf
      { base with
        prepended_items =
          [ { entry_id = id
            ; change_id = 0
            ; value =
                History_entry.Payload.Semantic.create
                  (Reasoning { readable_summary = [] })
                  ~metadata:History_entry.Payload.Metadata.empty
                |> Result.ok_or_failwith
                |> History_entry.Payload.authored
            ; script_label = None
            }
          ]
      }
    |> Session.Moderator_state.Identity_snapshot.of_jsonaf
    |> Result.is_error
  in
  let shell =
    Session.Shell_state.to_jsonaf
      { Session.Shell_state.empty with last_audit_sequence = Some (-1L) }
    |> Session.Shell_state.of_jsonaf
    |> Result.is_error
  in
  print_s [%sexp { negative : bool; reused_watermark : bool; shell : bool }];
  [%expect {| ((negative true) (reused_watermark true) (shell true)) |}]
;;
