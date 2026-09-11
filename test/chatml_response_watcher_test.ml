open Core
module L = Chatml.Chatml_lang
module V = Chatml.Chatml_value_codec
module R = Chatml_host_runtime

let source = [%blob "chatml_extensibility_fixtures/x06-response-watcher/watcher.chatml"]

let probe_source =
  [%blob "chatml_extensibility_fixtures/x06-response-watcher/probe.chatml"]
;;

let ok = Result.ok_or_failwith
let json = V.import_json
let record fields = L.VRecord (String.Map.of_alist_exn fields)

let get value name =
  match value with
  | L.VRecord fields -> Map.find_exn fields name
  | _ -> failwith "expected record"
;;

let string value = V.expect_string "watcher test" value |> ok
let int value = V.expect_int "watcher test" value |> ok

let update value name data =
  match value with
  | L.VRecord fields -> L.VRecord (Map.set fields ~key:name ~data)
  | _ -> failwith "expected record"
;;

let same a b =
  [%test_eq: V.Snapshot.t] (V.Snapshot.of_value_exn a) (V.Snapshot.of_value_exn b)
;;

let item =
  record
    [ "id", VString "watch-1"
    ; "invocation", VString "call-1"
    ; ( "query"
      , json
          (`Object [ "session_id", `String "child-1"; "receipt_id", `String "receipt-1" ])
      )
    ; "epoch", VInt 1
    ; "job", VString "job-1"
    ; "timer", VString ""
    ; "status", VString "watching"
    ; "deadline", VInt 10000
    ; "delay", VInt 25
    ; "retries", VInt 0
    ; "last_identity", VString ""
    ]
;;

let succeeded state =
  json
    (`Object
        [ "type", `String "succeeded"
        ; ( "value"
          , `Object
              [ "state", `String state
              ; "value", `Object [ "next_cursor", `String "next" ]
              ] )
        ])
;;

let failure ?(retryable = false) code =
  json
    (`Object
        [ "type", `String "failed"
        ; "code", `String code
        ; "message", `String "probe failed"
        ; ("retryable", if retryable then `True else `False)
        ; "details", `Null
        ])
;;

(* Exercise the exact authored algorithm against a recording host. Daemon tests
   separately prove durable authorization, ownership and notification commits. *)
let fixture ?(source = source) () =
  let surface = Chatml.Chatml_extension_surface.moderator_v1 in
  let compiled = R.compile_script ~surface ~source () |> ok in
  let effects = ref [] in
  let next = ref 0 in
  let probe = ref (succeeded "pending") in
  let fresh prefix =
    Int.incr next;
    L.VString (prefix ^ Int.to_string !next)
  in
  let receipt tag value = L.VVariant (tag, [ VInt 0; value ]) in
  let terminal kind payload =
    let fields =
      match kind with
      | "succeeded" -> [ "value", V.value_to_jsonaf_exn payload ]
      | "cancelled" -> [ "reason", `String (string payload) ]
      | _ ->
        [ "code", `String (get payload "code" |> string)
        ; "message", `String (get payload "message" |> string)
        ; "retryable", `False
        ; "details", `Null
        ]
    in
    receipt
      "Subscription_receipt"
      (json (`Object [ "result", `Object (("type", `String kind) :: fields) ]))
  in
  let perform name _ (args : L.value list) =
    effects := (name, args) :: !effects;
    Ok
      (match name, args with
       | "Job.start_tool", [ VString "watch_probe"; _ ] -> fresh "probe-"
       | "Job.read_result", [ VString _ ] -> !probe
       | ( "Schedule.after_ms_with_policy"
         , [ VInt _; _; VVariant ("Deliver_once_immediately", []) ] ) ->
         receipt "Schedule_receipt" (fresh "timer-")
       | "Schedule.cancel", [ VString _ ] -> receipt "Schedule_receipt" VUnit
       | "Subscription.arm", [ VString _; VInt _; _; _ ] ->
         receipt "Subscription_receipt" (json `Null)
       | "Subscription.complete", [ VString _; VInt _; value ] ->
         terminal "succeeded" value
       | "Subscription.fail", [ VString _; VInt _; value ] -> terminal "failed" value
       | "Subscription.cancel", [ VString _; VInt _; value ] -> terminal "cancelled" value
       | "Notification.publish", [ _; _; _ ] ->
         receipt "Notification_receipt" (fresh "delivery-")
       | "Invocation.resolve", [ VString _; _ ] -> VUnit
       | _ -> failwith ("unexpected watcher operation: " ^ name))
  in
  let operations =
    List.map
      [ "Job.start_tool"
      ; "Job.read_result"
      ; "Schedule.after_ms_with_policy"
      ; "Schedule.cancel"
      ; "Subscription.arm"
      ; "Subscription.complete"
      ; "Subscription.fail"
      ; "Subscription.cancel"
      ; "Notification.publish"
      ; "Invocation.resolve"
      ]
      ~f:(fun name ->
        R.
          { name
          ; kind = External_sync
          ; perform = perform name
          ; phase_check = allow_all_phases
          })
  in
  let config = R.{ surface; operations } in
  let run entrypoint arguments =
    effects := [];
    let value = R.run_entrypoint config compiled ~entrypoint ~arguments () |> ok in
    value, List.rev !effects
  in
  run, probe
;;

let delay effects =
  List.find_map_exn effects ~f:(function
    | "Schedule.after_ms_with_policy", L.VInt delay :: _ -> Some delay
    | _ -> None)
;;

let names effects = List.map effects ~f:fst

let event ?(now = 0) run state payload =
  run
    "watch_on_event"
    [ record [ "now_ms", VInt now ]
    ; state
    ; VVariant ("Internal_event", [ json payload ])
    ]
;;

let tick epoch =
  `Object
    [ "kind", `String "response_watch_tick"
    ; "subscription_id", `String "watch-1"
    ; "epoch", `String (Int.to_string epoch)
    ]
;;

let completed id =
  `Object [ "kind", `String "background_job_completed"; "job_id", `String id ]
;;

let%expect_test
    "watcher backoff, retry exhaustion and author policies use the compiled source"
  =
  let run, _ = fixture () in
  let current = ref item in
  let delays =
    List.map (List.range 0 8) ~f:(fun _ ->
      let updated, effects = run "watch_after_probe" [ !current; succeeded "pending" ] in
      [%test_eq: int]
        (get !current "epoch" |> int |> Int.succ)
        (get updated "epoch" |> int);
      current := updated;
      delay effects)
  in
  print_s [%sexp (delays : int list)];
  List.iter [ "agent.helper.transport"; "background.interrupted" ] ~f:(fun code ->
    let current = ref item in
    let states =
      List.map (List.range 0 3) ~f:(fun _ ->
        let updated, effects =
          run
            "watch_after_probe"
            [ !current
            ; failure ~retryable:(String.equal code "agent.helper.transport") code
            ]
        in
        current := updated;
        get updated "status" |> string, names effects)
    in
    print_s [%sexp (code : string), (states : (string * string list) list)]);
  let fail_permission =
    String.substr_replace_all
      source
      ~pattern:"let watch_permission_policy = \"wait\""
      ~with_:"let watch_permission_policy = \"fail\""
  in
  let wait_stopped =
    String.substr_replace_all
      source
      ~pattern:"let watch_stopped_policy = \"fail\""
      ~with_:"let watch_stopped_policy = \"wait\""
  in
  List.iter
    [ "default permission", source, "permission_wait"
    ; "fail permission", fail_permission, "permission_wait"
    ; "default stopped", source, "stopped"
    ; "wait stopped", wait_stopped, "stopped"
    ]
    ~f:(fun (label, source, state) ->
      let run, _ = fixture ~source () in
      let updated, effects = run "watch_after_probe" [ item; succeeded state ] in
      print_s
        [%sexp
          (label : string)
        , (get updated "status" |> string : string)
        , (names effects : string list)]);
  [%expect
    {|
    (25 50 100 200 400 800 1000 1000)
    (agent.helper.transport
     ((watching (Schedule.after_ms_with_policy Subscription.arm))
      (watching (Schedule.after_ms_with_policy Subscription.arm))
      (failed (Schedule.cancel Subscription.fail Notification.publish))))
    (background.interrupted
     ((watching (Schedule.after_ms_with_policy Subscription.arm))
      (watching (Schedule.after_ms_with_policy Subscription.arm))
      (failed (Schedule.cancel Subscription.fail Notification.publish))))
    ("default permission" watching
     (Schedule.after_ms_with_policy Subscription.arm))
    ("fail permission" failed (Subscription.fail Notification.publish))
    ("default stopped" failed (Subscription.fail Notification.publish))
    ("wait stopped" watching (Schedule.after_ms_with_policy Subscription.arm))
    |}]
;;

let%expect_test
    "stale callbacks, terminal duplicates, deadlines and active probe cancellation"
  =
  let run, probe = fixture () in
  let initial =
    L.VArray
      [| (update item "job" (VString "")
          |> fun v -> update v "timer" (VString "timer-old"))
      |]
  in
  let stale, effects = event run initial (tick 0) in
  same initial stale;
  [%test_eq: string list] [] (names effects);
  let malformed, effects = event run initial (completed "") in
  same initial malformed;
  [%test_eq: string list] [] (names effects);
  let probing, effects = event run initial (tick 1) in
  [%test_eq: string list] [ "Job.start_tool"; "Subscription.arm" ] (names effects);
  let duplicate, effects = event run probing (tick 1) in
  same probing duplicate;
  [%test_eq: string list] [] (names effects);
  let wrong, effects = event run probing (completed "unrelated") in
  same probing wrong;
  [%test_eq: string list] [] (names effects);
  let watch =
    match probing with
    | VArray values -> values.(0)
    | _ -> assert false
  in
  let job = get watch "job" |> string in
  probe := succeeded "ready";
  let delivered, effects = event run probing (completed job) in
  [%test_eq: string list]
    [ "Job.read_result"; "Subscription.complete"; "Notification.publish" ]
    (names effects);
  let final =
    match delivered with
    | L.VArray values -> values.(0)
    | _ -> assert false
  in
  [%test_eq: string] "delivered" (get final "status" |> string);
  [%test_eq: string] "next" (get final "last_identity" |> string);
  let duplicate, effects = event run delivered (completed job) in
  same delivered duplicate;
  [%test_eq: string list] [] (names effects);
  let _, effects = event ~now:10000 run probing (completed job) in
  [%test_eq: string list] [ "Subscription.fail"; "Notification.publish" ] (names effects);
  let context =
    record
      [ "tool_name", VString "cancel_response_watch"
      ; "invocation_id", VString "cancel-call"
      ]
  in
  let cancelled, effects =
    run
      "watch_on_event"
      [ record [ "now_ms", VInt 0 ]
      ; probing
      ; VVariant
          ( "Tool_invoked"
          , [ record
                [ "context", context
                ; "input", json (`Object [ "subscription_id", `String "watch-1" ])
                ]
            ] )
      ]
  in
  [%test_eq: string list]
    [ "Subscription.cancel"; "Notification.publish"; "Invocation.resolve" ]
    (names effects);
  assert (
    List.exists effects ~f:(function
      | "Notification.publish", [ _; VVariant ("Cancelled", _); VVariant ("No_wake", []) ]
        -> true
      | _ -> false));
  let late, effects = event run cancelled (completed job) in
  same cancelled late;
  [%test_eq: string list] [] (names effects);
  print_endline
    "stale epochs and duplicate results ignored; deadline beats late success; cancelled \
     probe cannot notify twice";
  [%expect
    {| stale epochs and duplicate results ignored; deadline beats late success; cancelled probe cannot notify twice |}]
;;

let%expect_test
    "read-only probes distinguish terminal output, live operations and invalid cursors"
  =
  let surface = Chatml.Chatml_extension_surface.tool_v1 in
  let compiled = R.compile_script ~surface ~source:probe_source () |> ok in
  let status ?(generation = "1") ?(operation = `Null) ?(desired = "running") state =
    `Object
      [ "generation", `Number generation
      ; "operation", operation
      ; "state", `String state
      ; "desired_state", `String desired
      ]
  in
  let page =
    `Object
      [ "generation", `Number "1"
      ; "items", `Array [ `String "answer" ]
      ; "next_cursor", `String "next"
      ]
  in
  let receipt state =
    `Object
      [ "reason", `String "receipt_terminal"
      ; "receipt", `Object [ "status", `String state ]
      ]
  in
  let waiting status = `Object [ "reason", `String "timeout"; "status", status ] in
  let scenarios =
    [ ( "completed receipt"
      , "receipt_probe"
      , [ "wait", Ok (receipt "completed"); "read", Ok page ] )
    ; "cancelled receipt", "receipt_probe", [ "wait", Ok (receipt "cancelled") ]
    ; ( "approval needed"
      , "receipt_probe"
      , [ "wait", Ok (waiting (status "waiting_for_permission")) ] )
    ; ( "stopped child"
      , "receipt_probe"
      , [ "wait", Ok (waiting (status ~desired:"stopped" "stopped")) ] )
    ; ( "available cursor"
      , "cursor_probe"
      , [ "read", Ok page; "status", Ok (status "idle") ] )
    ; ( "output before completion"
      , "cursor_probe"
      , [ "read", Ok page; "status", Ok (status ~operation:(`String "active") "running") ]
      )
    ; ( "generation changed"
      , "cursor_probe"
      , [ "read", Ok page; "status", Ok (status ~generation:"2" "idle") ] )
    ; "expired cursor", "cursor_probe", [ "read", Error "agent.read.cursor_expired" ]
    ; "transport failed", "receipt_probe", [ "wait", Error "agent.helper.transport" ]
    ; "foreign child", "receipt_probe", [ "wait", Error "agent.management.denied" ]
    ]
  in
  List.iter scenarios ~f:(fun (label, entrypoint, responses) ->
    let pending = ref responses in
    let operation : R.op_def =
      { name = "Tool.call"
      ; kind = External_sync
      ; phase_check = R.allow_all_phases
      ; perform =
          (fun _ -> function
             | [ L.VString "watch_session_request"; request ] ->
               let request = V.value_to_jsonaf_exn request in
               let requested =
                 Jsonaf.member_exn "operation" request |> Jsonaf.string_exn
               in
               let args = Jsonaf.member_exn "arguments" request in
               [%test_eq: string]
                 "child"
                 (Jsonaf.member_exn "session_id" args |> Jsonaf.string_exn);
               (match requested with
                | "wait" ->
                  [%test_eq: float]
                    0.
                    (Jsonaf.member_exn "timeout_ms" args |> Jsonaf.float_exn)
                | "read" ->
                  [%test_eq: float]
                    16.
                    (Jsonaf.member_exn "limit" args |> Jsonaf.float_exn)
                | "status" -> ()
                | _ -> failwith "probe attempted a mutating operation");
               (match !pending with
                | (expected, response) :: rest ->
                  [%test_eq: string] expected requested;
                  pending := rest;
                  Ok
                    (match response with
                     | Ok value -> L.VVariant ("Ok", [ json value ])
                     | Error code -> L.VVariant ("Error", [ VString code ]))
                | [] -> failwith "unexpected extra probe request")
             | _ -> Error "unexpected probe tool")
      }
    in
    let query =
      json
        (`Object
            [ "session_id", `String "child"
            ; "receipt_id", `String "receipt"
            ; "cursor", `String "cursor"
            ])
    in
    let output =
      R.run_entrypoint
        { surface; operations = [ operation ] }
        compiled
        ~entrypoint
        ~arguments:[ query ]
        ()
      |> ok
    in
    assert (List.is_empty !pending);
    let result =
      match output with
      | VVariant ("Complete", [ value ]) ->
        Jsonaf.member_exn "state" (V.value_to_jsonaf_exn value) |> Jsonaf.string_exn
      | VVariant ("Fail", [ error ]) ->
        get error "code"
        |> string
        |> fun code ->
        code
        ^
          (match get error "retryable" with
          | VBool true -> " (retryable)"
          | VBool false -> ""
          | _ -> assert false)
      | _ -> failwith "invalid probe outcome"
    in
    print_s [%sexp (label : string), (result : string)]);
  [%expect
    {|
    ("completed receipt" ready)
    ("cancelled receipt" watcher.target_failed)
    ("approval needed" permission_wait)
    ("stopped child" stopped)
    ("available cursor" ready)
    ("output before completion" pending)
    ("generation changed" watcher.snapshot_required)
    ("expired cursor" agent.read.cursor_expired)
    ("transport failed" "agent.helper.transport (retryable)")
    ("foreign child" agent.management.denied)
    |}]
;;
