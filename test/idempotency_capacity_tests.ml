open! Core
module D = Document_schema
module C = Agent_store.Idempotency_capacity

let ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "unexpected capacity fixture error", (error : D.Error.t)]
;;

let put json name value =
  match json with
  | `Object fields ->
    `Object
      ((name, value)
       :: List.filter fields ~f:(fun (key, _) -> not (String.equal key name)))
  | _ -> failwith "fixture expected object"
;;

let pending = `Object [ "tag", `String "pending" ]

let terminal =
  `Object
    [ "tag", `String "terminal"
    ; "digest", `String (String.make 64 'f')
    ; "encoded_bytes", `String "16777216"
    ]
;;

let row ?(omitted = false) ?(created = "2026-10-09T00:00:00.000000000Z") outcome =
  let key =
    `Object
      ([ "principal_id", `String "prn_capacity"
       ; "method_name", `String "session.attach"
       ; "idempotency_key", `String "capacity-key"
       ]
       @ if omitted then [] else [ "session_id", `Null ])
  in
  `Object
    ([ "record_id", `String (String.make 64 'a')
     ; "key", key
     ; "request_digest", `String (String.make 64 'b')
     ; "outcome", outcome
     ; "created_at", `String created
     ; "retention", `String "protected"
     ]
     @
     if omitted
     then []
     else
       [ "accepted_transaction_sequence", `String (Int64.to_string Int64.max_value)
       ; "expires_at", `Null
       ])
;;

let document rows =
  D.Document.create
    ~limits:(C.limits C.default ~mode:Existing)
    ~kind:"store.idempotency_cache"
    ~version:3
    ~payload:(`Object [ "records", `Array rows ])
  |> ok
  |> D.Document.json
;;

let bytes json = String.length (Jsonaf.to_string json)

let profile ?(fields = 1000) ?(nodes = 1000) byte_limit =
  C.create ~max_bytes:byte_limit ~max_fields:fields ~max_nodes:nodes |> ok
;;

let show name = function
  | Ok () -> printf "%s=ok\n" name
  | Error (D.Error.Limit_exceeded dimension) -> printf "%s=%s\n" name dimension
  | Error (D.Error.Invalid_configuration _) -> printf "%s=configuration\n" name
  | Error error -> raise_s [%sexp "unexpected capacity rejection", (error : D.Error.t)]
;;

let%expect_test "remaining reservation survives completion and acceptance in either order"
  =
  let pending_row = row ~omitted:true ~created:"2026-10-09T00:00:00Z" pending in
  let complete_row = row terminal in
  let capacity = profile (bytes (document [ complete_row ])) in
  List.iter
    [ "pending", pending_row
    ; "accepted-pending", put pending_row "accepted_transaction_sequence" (`String "17")
    ; "completed-unaccepted", put pending_row "outcome" terminal
    ; "completed-accepted", complete_row
    ]
    ~f:(fun (name, current) ->
      show name (C.check capacity (document [ current ]) ~mode:Fresh));
  show
    "one-byte-short"
    (C.check
       (profile (bytes (document [ complete_row ]) - 1))
       (document [ pending_row ])
       ~mode:Fresh);
  let expiring =
    row ~omitted:true ~created:"2026-10-09T00:00:00Z" pending
    |> fun value -> put value "expires_at" (`String "2026-10-10T00:00:00Z")
  in
  let expired_terminal =
    row terminal
    |> fun value -> put value "expires_at" (`String "2026-10-10T00:00:00.000000000Z")
  in
  show
    "short-expiry"
    (C.check
       (profile (bytes (document [ expired_terminal ])))
       (document [ expiring ])
       ~mode:Fresh);
  [%expect
    {|
    pending=ok
    accepted-pending=ok
    completed-unaccepted=ok
    completed-accepted=ok
    one-byte-short=bytes
    short-expiry=ok
    |}]
;;

let%expect_test
    "reservation covers mixed independent completions with retained extensions"
  =
  let large = put pending "future" (`String (String.make 4000 'x')) in
  let actual = document [ row large; row pending ] in
  let completed = document [ row terminal; row terminal ] in
  let mixed = document [ row large; row terminal ] in
  let capacity = profile (Int.max (bytes actual) (bytes completed)) in
  show "actual-only" (D.Json.validate ~limits:(C.limits capacity ~mode:Fresh) actual);
  show
    "all-completed-only"
    (D.Json.validate ~limits:(C.limits capacity ~mode:Fresh) completed);
  show "mixed-state" (D.Json.validate ~limits:(C.limits capacity ~mode:Fresh) mixed);
  show "reserve-before-effects" (C.check capacity actual ~mode:Fresh);
  let fitting = profile (bytes mixed) in
  show "reserved-fit" (C.check fitting actual ~mode:Fresh);
  show "mixed-fit" (C.check fitting mixed ~mode:Fresh);
  [%expect
    {|
    actual-only=ok
    all-completed-only=ok
    mixed-state=bytes
    reserve-before-effects=bytes
    reserved-fit=ok
    mixed-fit=ok
    |}]
;;

let%expect_test "mixed retained extensions reserve fields and nodes independently" =
  let field_heavy =
    put
      pending
      "future"
      (`Object (List.init 50 ~f:(fun index -> Int.to_string index, `Null)))
  in
  let field_actual = document [ row field_heavy; row pending ] in
  let field_capacity = profile ~fields:82 100_000 in
  show
    "field-actual"
    (D.Json.validate ~limits:(C.limits field_capacity ~mode:Fresh) field_actual);
  show "field-reserved" (C.check field_capacity field_actual ~mode:Fresh);
  let node_heavy = put pending "future" (`Array (List.init 64 ~f:(fun _ -> `Null))) in
  let node_actual = document [ row node_heavy; row pending ] in
  let node_capacity = profile ~nodes:99 100_000 in
  show
    "node-actual"
    (D.Json.validate ~limits:(C.limits node_capacity ~mode:Fresh) node_actual);
  show "node-reserved" (C.check node_capacity node_actual ~mode:Fresh);
  [%expect
    {|
    field-actual=ok
    field-reserved=fields
    node-actual=ok
    node-reserved=nodes
    |}]
;;

let%expect_test "field node depth and escaped byte budgets are independent" =
  let current = document [ row pending ] in
  show
    "field-boundary"
    (C.check (profile ~fields:20 ~nodes:1000 4096) current ~mode:Fresh);
  show "field-short" (C.check (profile ~fields:19 ~nodes:1000 4096) current ~mode:Fresh);
  show "node-boundary" (C.check (profile ~fields:1000 ~nodes:22 4096) current ~mode:Fresh);
  show "node-short" (C.check (profile ~fields:1000 ~nodes:21 4096) current ~mode:Fresh);
  let escaped = `String (String.make 80 '"') in
  let current = put current "future" escaped in
  let completed = put (document [ row terminal ]) "future" escaped in
  show "escaped-exact" (C.check (profile (bytes completed)) current ~mode:Fresh);
  show "escaped-short" (C.check (profile (bytes completed - 1)) current ~mode:Fresh);
  let deep =
    List.init 256 ~f:Fn.id |> List.fold ~init:`Null ~f:(fun value _ -> `Array [ value ])
  in
  show
    "depth"
    (C.check (profile 4096) (put (document [ row pending ]) "future" deep) ~mode:Fresh);
  [%expect
    {|
    field-boundary=ok
    field-short=fields
    node-boundary=ok
    node-short=nodes
    escaped-exact=ok
    escaped-short=bytes
    depth=depth
    |}]
;;

let%expect_test "compatibility headroom never grants new admission capacity" =
  let legacy = document [ row ~omitted:true ~created:"2026-10-09T00:00:00Z" pending ] in
  let capacity = profile ~fields:15 ~nodes:17 (bytes legacy + 221) in
  show "legacy-actual" (D.Json.validate ~limits:(C.limits capacity ~mode:Fresh) legacy);
  show "legacy-existing" (C.check capacity legacy ~mode:Existing);
  show "legacy-fresh" (C.check capacity legacy ~mode:Fresh);
  show "terminal-existing" (C.check capacity (document [ row terminal ]) ~mode:Existing);
  let default_bytes = D.Limits.max_bytes (C.limits C.default ~mode:Existing) in
  printf "derived-existing-bytes=%d\n" default_bytes;
  show
    "overflow"
    (C.create ~max_bytes:Int.max_value ~max_fields:Int.max_value ~max_nodes:Int.max_value
     |> Result.map ~f:ignore);
  show
    "field-overflow"
    (C.create ~max_bytes:1_000_000 ~max_fields:Int.max_value ~max_nodes:1_000_000
     |> Result.map ~f:ignore);
  show
    "node-overflow"
    (C.create ~max_bytes:1_000_000 ~max_fields:1_000_000 ~max_nodes:Int.max_value
     |> Result.map ~f:ignore);
  show
    "nonpositive"
    (C.create ~max_bytes:0 ~max_fields:10 ~max_nodes:11 |> Result.map ~f:ignore);
  [%expect
    {|
    legacy-actual=ok
    legacy-existing=ok
    legacy-fresh=fields
    terminal-existing=ok
    derived-existing-bytes=38877216
    overflow=configuration
    field-overflow=configuration
    node-overflow=configuration
    nonpositive=configuration
    |}]
;;

let%expect_test "all independent completion and acceptance subsets fit admitted capacity" =
  let outcomes =
    [ put pending "future" (`String (String.make 140 '"'))
    ; put pending "future" (`Object [ "null", `Null; "number", `Number "1e0" ])
    ; put pending "future" (`Array (List.init 8 ~f:(fun _ -> `Null)))
    ]
  in
  let selected mask index = not (Int.equal (mask land (1 lsl index)) 0) in
  let state completed accepted =
    List.mapi outcomes ~f:(fun index outcome ->
      let current = row (if selected completed index then terminal else outcome) in
      let key = D.Json.field current ~name:"key" in
      let key =
        match key with
        | Value key -> key
        | Absent | Null -> failwith "fixture key missing"
      in
      let key = put key "idempotency_key" (`String (sprintf "capacity-%d" index)) in
      current
      |> fun current ->
      put current "key" key
      |> fun current ->
      put
        current
        "record_id"
        (`String (Agent_store.Document_record.digest (Jsonaf.to_string key)))
      |> fun current ->
      put
        current
        "accepted_transaction_sequence"
        (if selected accepted index
         then `String (Int64.to_string Int64.max_value)
         else `Null))
    |> document
  in
  let states =
    List.concat_map (List.init 8 ~f:Fn.id) ~f:(fun completed ->
      List.init 8 ~f:(fun accepted -> state completed accepted))
  in
  let maximum =
    List.fold states ~init:0 ~f:(fun largest json -> Int.max largest (bytes json))
  in
  let capacity = profile maximum in
  let admitted = C.check capacity (state 0 0) ~mode:Fresh in
  show "admitted" admitted;
  let valid =
    List.count states ~f:(fun json ->
      Result.is_ok (D.Json.validate ~limits:(C.limits capacity ~mode:Fresh) json))
  in
  printf "future-subsets=%d/%d\n" valid (List.length states);
  [%expect
    {|
    admitted=ok
    future-subsets=64/64
    |}]
;;

let%expect_test "composite reference size does not enlarge fresh metadata admission" =
  let composite = put terminal "encoded_bytes" (`String "33554432") in
  show "composite-reference" (C.check C.default (document [ row composite ]) ~mode:Fresh);
  let oversized = put terminal "encoded_bytes" (`String "33554433") in
  (match C.check C.default (document [ row oversized ]) ~mode:Existing with
   | Error (D.Error.Invalid_field { path; _ }) ->
     printf "above-composite=invalid:%s\n" (List.last_exn path)
   | result -> show "above-composite" result);
  printf
    "fresh-bytes=%d existing-bytes=%d\n"
    (D.Limits.max_bytes (C.limits C.default ~mode:Fresh))
    (D.Limits.max_bytes (C.limits C.default ~mode:Existing));
  [%expect
    {|
    composite-reference=ok
    above-composite=invalid:encoded_bytes
    fresh-bytes=16777216 existing-bytes=38877216
    |}]
;;
