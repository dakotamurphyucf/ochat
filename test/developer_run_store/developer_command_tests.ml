open! Core
module D = Document_schema
module P = Agent_protocol
module R = Agent_store.Developer_command_receipt
module Row = Agent_store.Developer_command_row

let ok = function
  | Ok value -> value
  | Error _ -> failwith "valid developer command fixture rejected"
;;

let limits =
  D.Limits.create ~max_bytes:(64 * 1024) ~max_depth:32 ~max_fields:512 ~max_nodes:1024
  |> ok
;;

let poll () = ()
let receipt value = R.create value ~limits ~poll |> ok
let id = R.Id.of_string "dcr_test_receipt" |> ok
let key = P.Idempotency_key.of_string "developer-command-test" |> ok
let server = P.Id.Server.of_string "srv_command_receipt" |> ok
let run = P.Id.Developer_run.of_string "dvr_command_receipt" |> ok
let principal = P.Id.Principal.of_string "pri_command_receipt" |> ok
let capture_fingerprint = Chatml_source.Digest.of_contents "capture" ~checkpoint:poll
let request_fingerprint = Chatml_source.Digest.of_contents "request" ~checkpoint:poll

let owner =
  match
    P.Execution_owner.developer
      ~host_id:server
      ~run_id:run
      ~epoch:1
      ~principal_id:principal
    |> ok
  with
  | P.Execution_owner.Developer owner -> owner
  | Conversation _ -> failwith "developer owner fixture changed kind"
;;

let row kind =
  Row.create ~id ~key ~owner ~capture_fingerprint ~request_fingerprint ~kind ~limits ~poll
  |> ok
;;

let update row change = Row.apply row change ~limits ~poll
let roundtrip receipt = R.of_json (R.to_json receipt) ~limits ~poll |> ok

let replace object_ name value =
  match object_ with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (field, old) ->
         field, if String.equal field name then value else old))
  | _ -> failwith "expected named receipt fixture"
;;

let%expect_test "command receipt nested values roundtrip and strict optional location" =
  let diagnostic : R.Diagnostic.t =
    { code = "chatml.invalid"; message = ""; location = None }
  in
  let check = receipt (R.Check (R.Check.Rejected diagnostic)) in
  let fixture =
    receipt
      (R.Fixture
         (R.Fixture.Completed
            { R.Fixture.graph_fingerprint = capture_fingerprint
            ; fixture_digest = request_fingerprint
            ; matched_steps = 1
            ; logs =
                [ { R.Fixture.Log.level = R.Fixture.Log.Level.Info
                  ; message = String.make 17_000 'l'
                  }
                ]
            ; outcome = R.Fixture.Succeeded `Null
            }))
  in
  let live = receipt (R.Live (R.Live.Succeeded `Null)) in
  printf
    "check=%b fixture=%b live=%b\n"
    (R.equal check (roundtrip check))
    (R.equal fixture (roundtrip fixture))
    (R.equal live (roundtrip live));
  let tighter =
    D.Limits.create ~max_bytes:8192 ~max_depth:32 ~max_fields:512 ~max_nodes:1024 |> ok
  in
  printf
    "long_log_aggregate_limited=%b\n"
    (Result.is_error (R.create (R.value fixture) ~limits:tighter ~poll));
  let failed error =
    R.Fixture
      (R.Fixture.Completed
         { graph_fingerprint = capture_fingerprint
         ; fixture_digest = request_fingerprint
         ; matched_steps = 0
         ; logs = []
         ; outcome = R.Fixture.Failed error
         })
  in
  let protocol_error =
    P.Error.create P.Error.Resource_limit ~message:"" ~retryable:false ()
  in
  let alternatives =
    [ R.Check R.Check.Ready
    ; R.Check (R.Check.Rejected diagnostic)
    ; R.Check R.Check.Interrupted
    ; R.Check R.Check.Cancelled
    ; R.value fixture
    ; failed (R.Fixture.Failure.Compile diagnostic)
    ; failed
        (R.Fixture.Failure.Fixture_mismatch { expected_step = 1; actual_tool = "tool" })
    ; failed (R.Fixture.Failure.Fixture_missing { next_step = 2 })
    ; failed (R.Fixture.Failure.Unsupported_effect { operation = "network" })
    ; failed (R.Fixture.Failure.Runtime diagnostic)
    ; failed (R.Fixture.Failure.Invalid_result diagnostic)
    ; failed R.Fixture.Failure.Output_limit
    ; R.Fixture R.Fixture.Interrupted
    ; R.Fixture R.Fixture.Cancelled
    ; R.Live (R.Live.Succeeded `Null)
    ; R.Live (R.Live.Failed protocol_error)
    ; R.Live (R.Live.Limited protocol_error)
    ; R.Live R.Live.Interrupted
    ; R.Live R.Live.Cancelled
    ]
  in
  printf
    "alternatives=%d:%b\n"
    (List.length alternatives)
    (List.for_all alternatives ~f:(fun value ->
       let original = receipt value in
       R.equal original (roundtrip original)));
  let invalid_null =
    match R.to_json check with
    | `Object fields ->
      let diagnostic =
        List.Assoc.find_exn fields "diagnostic" ~equal:String.equal
        |> fun json ->
        match json with
        | `Object fields -> `Object (fields @ [ "location", `Null ])
        | _ -> failwith "missing diagnostic"
      in
      replace (R.to_json check) "diagnostic" diagnostic
    | _ -> failwith "missing receipt fields"
  in
  printf
    "null_location_rejected=%b\n"
    (Result.is_error (R.of_json invalid_null ~limits ~poll));
  let duplicate =
    match R.to_json check with
    | `Object fields -> `Object (fields @ [ "state", `String "ready" ])
    | _ -> failwith "missing receipt fields"
  in
  printf "duplicate_rejected=%b\n" (Result.is_error (R.of_json duplicate ~limits ~poll));
  [%expect
    {|
    check=true fixture=true live=true
    long_log_aggregate_limited=true
    alternatives=19:true
    null_location_rejected=true
    duplicate_rejected=true |}]
;;

let%expect_test "command phases preserve one-shot execution and terminal identity" =
  let ready = receipt (R.Check R.Check.Ready) in
  let fixture = receipt (R.Fixture R.Fixture.Interrupted) in
  let success = receipt (R.Live (R.Live.Succeeded (`String "done"))) in
  let failure =
    receipt
      (R.Live
         (R.Live.Failed
            (P.Error.create
               P.Error.Invalid_request
               ~message:"compile"
               ~retryable:false
               ())))
  in
  let started = Row.Change.Start_execution in
  let waiting = Row.Change.Wait_permission in
  let finish receipt = Row.Change.Finish receipt in
  let cases =
    [ "check finish", R.Kind.Check, [ finish ready ], true
    ; "check wait", R.Kind.Check, [ waiting ], false
    ; "check start", R.Kind.Check, [ started ], false
    ; "check wrong kind", R.Kind.Check, [ finish fixture ], false
    ; "fixture finish", R.Kind.Fixture_run, [ finish fixture ], true
    ; "fixture wait", R.Kind.Fixture_run, [ waiting ], false
    ; "fixture start", R.Kind.Fixture_run, [ started ], false
    ; "fixture wrong kind", R.Kind.Fixture_run, [ finish ready ], false
    ; ( "live wait start success"
      , R.Kind.Live_run
      , [ waiting; started; finish success ]
      , true )
    ; "live direct start success", R.Kind.Live_run, [ started; finish success ], true
    ; "live early success", R.Kind.Live_run, [ finish success ], false
    ; "live early failure", R.Kind.Live_run, [ finish failure ], true
    ; "live repeat start", R.Kind.Live_run, [ started; started ], false
    ; ( "live repeat terminal"
      , R.Kind.Live_run
      , [ started; finish success; finish success ]
      , true )
    ; ( "live changed terminal"
      , R.Kind.Live_run
      , [ started; finish success; finish failure ]
      , false )
    ; ( "live reopened terminal"
      , R.Kind.Live_run
      , [ started; finish success; started ]
      , false )
    ]
  in
  List.iter cases ~f:(fun (name, kind, changes, expected) ->
    let result =
      List.fold
        changes
        ~init:(Ok (row kind))
        ~f:(fun result change -> Result.bind result ~f:(fun row -> update row change))
      |> Result.is_ok
    in
    printf "%s=%b\n" name result;
    assert (Bool.equal result expected));
  let check_done = update (row R.Kind.Check) (finish ready) |> ok in
  let check_roundtrip = Row.of_json (Row.to_json check_done) ~limits ~poll |> ok in
  printf
    "check terminal roundtrip=%b\n"
    (D.Json.equal (Row.to_json check_done) (Row.to_json check_roundtrip));
  let terminal =
    update (update (row R.Kind.Live_run) started |> ok) (finish success) |> ok
  in
  let corrupt =
    match Row.to_json terminal with
    | `Object fields ->
      let phase = List.Assoc.find_exn fields "phase" ~equal:String.equal in
      replace (Row.to_json terminal) "phase" (replace phase "execution_started" `False)
    | _ -> failwith "missing row fields"
  in
  printf
    "unstarted_success_rejected=%b\n"
    (Result.is_error (Row.of_json corrupt ~limits ~poll));
  [%expect
    {|
    check finish=true
    check wait=false
    check start=false
    check wrong kind=false
    fixture finish=true
    fixture wait=false
    fixture start=false
    fixture wrong kind=false
    live wait start success=true
    live direct start success=true
    live early success=false
    live early failure=true
    live repeat start=false
    live repeat terminal=true
    live changed terminal=false
    live reopened terminal=false
    check terminal roundtrip=true
    unstarted_success_rejected=true |}]
;;
