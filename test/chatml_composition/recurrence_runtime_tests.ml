open! Core
open Agent_server_test_support
module A = Agent_session
module P = Agent_protocol
module Native = Instruction_runtime_tests

let object_ fields = `Object fields
let member = Native.member
let result value = member "result" value
let entry value = member "entry" (result value)
let basis value = member "basis" value
let call id name value = [ id, name, value ]
let id = P.Id.Recurrence.of_string "rec_native_weekly" |> protocol_ok
let existing_id = P.Id.Recurrence.of_string "rec_native_existing" |> protocol_ok
let lookup id = object_ [ "id", P.Id.Recurrence.to_json id ]

let request_basis read ~control ~source ~key =
  let observed = basis read in
  object_
    [ "generation", member "generation" observed
    ; "expected_catalog_revision", member "catalog_revision" observed
    ; "expected_control_revision", control
    ; ("source", if source then member "source" observed else `Null)
    ; ( "expected_lifecycle_revision"
      , if source then member "lifecycle_revision" observed else `Null )
    ; "key", `String key
    ]
;;

let mutation id read ~source ~key =
  object_
    [ "id", P.Id.Recurrence.to_json id
    ; ( "basis"
      , request_basis read ~control:(member "control_revision" (entry read)) ~source ~key
      )
    ]
;;

let schedule =
  let period =
    Recurrence_rules.Rule.Period.create_ms 600_000L
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum [%sexp (error : Recurrence_rules.Timing_error.t)])
    |> Result.ok_or_failwith
  in
  A.Recurrence_definition.Schedule.to_json
    (Interval
       { rule = Recurrence_rules.Rule.Interval.create ~mode:Fixed_rate ~period
       ; first_due = Time_ns.of_string_with_utc_offset "2100-01-01 00:00:00Z"
       })
;;

let capture target =
  object_
    [ "schedule", schedule
    ; "target", target
    ; "policy", A.Recurrence_policy.to_json A.Recurrence_policy.default
    ]
;;

let authored mode =
  object_
    [ "kind", `String "authored_child"
    ; "name", `String "weekly_worker"
    ; "mode", `String mode
    ]
;;

let create id read ~target ~key =
  object_
    [ "id", P.Id.Recurrence.to_json id
    ; "basis", request_basis read ~control:`Null ~source:true ~key
    ; "capture", capture target
    ]
;;

let prompt =
  {|<developer>Exercise selected recurring automation metadata; never run the future schedule.</developer>
<tool name="recurrence_create"/>
<tool name="recurrence_edit"/>
<tool name="recurrence_list"/>
<tool name="recurrence_get"/>
<tool name="recurrence_status"/>
<tool name="recurrence_history"/>
<tool name="recurrence_resume"/>
<tool name="recurrence_pause"/>
<tool name="recurrence_cancel"/>
<tool name="weekly_worker" agent="weekly.chatmd" local persistence="persistent"/>
<script id="owner" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
;;

let%expect_test
    "selected native recurrence workflow keeps original retry and explicit CAS"
  =
  let inputs = ref [] in
  let original = ref (`Object []) in
  let receipt = ref (`Object []) in
  let complete id = Native.complete !inputs id in
  Fixtures.with_daemon
    ~config_file:"server.sexp"
    ~sources:
      [ "agent.chatmd", prompt
      ; "server.sexp", Native.configuration
      ; ( "weekly.chatmd"
        , {|<developer>Retained weekly source.</developer>
<script id="worker" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
        )
      ]
    ~connect:(fun ~sw:_ ~env:_ ~root:_ daemon -> Native.authenticated_connection daemon)
    ~calls:(call "list" "recurrence_list" (object_ []))
    ~completion_timeout:60.
    ~initial_requests:16
    ~expected_requests:16
    ~expect_moderator:true
    ~inspect_request:(fun _ observed -> inputs := observed)
    ~followup_calls:(function
      | 2 ->
        let listed = complete "list" in
        assert (List.is_empty (Native.array "entries" (result listed)));
        original := create id listed ~target:(authored "workflow") ~key:"native-create";
        call "create" "recurrence_create" !original
      | 3 ->
        receipt := complete "create";
        call "get" "recurrence_get" (lookup id)
      | 4 ->
        let read = complete "get" in
        [%test_eq: string] "paused" (Native.string "kind" (member "control" (entry read)));
        call
          "edit"
          "recurrence_edit"
          (object_
             (Native.fields (mutation id read ~source:true ~key:"native-edit")
              @ [ "capture", capture (authored "single_turn") ]))
      | 5 ->
        ignore (complete "edit" : Jsonaf.t);
        call "retry" "recurrence_create" !original
      | 6 ->
        assert (Document_schema.Json.equal !receipt (complete "retry"));
        let original_basis = member "basis" !original in
        let changed_source =
          object_
            (List.map
               (Native.fields (member "source" original_basis))
               ~f:(fun (name, value) ->
                 if String.equal name "generation" then name, `Number "1" else name, value))
        in
        let changed =
          object_
            (List.map (Native.fields original_basis) ~f:(fun (name, value) ->
               if String.equal name "generation"
               then name, `Number "1"
               else if String.equal name "source"
               then name, changed_source
               else name, value))
        in
        call
          "changed-generation"
          "recurrence_create"
          (object_
             (List.map (Native.fields !original) ~f:(fun (name, value) ->
                if String.equal name "basis" then name, changed else name, value)))
      | 7 ->
        (match Native.response !inputs "changed-generation" with
         | Fail { code = "idempotency_conflict"; retryable = false; _ } -> ()
         | outcome ->
           raise_s
             [%sexp
               "changed original request was accepted", (outcome : P.Invocation.outcome)]);
        call "after-edit" "recurrence_get" (lookup id)
      | 8 ->
        let read = complete "after-edit" in
        [%test_eq: string] "1" (Native.string "definition_revision" (entry read));
        call
          "resume"
          "recurrence_resume"
          (mutation id read ~source:true ~key:"native-resume")
      | 9 ->
        ignore (complete "resume" : Jsonaf.t);
        call "status" "recurrence_status" (lookup id)
      | 10 ->
        let read = complete "status" in
        [%test_eq: string]
          "enabled"
          (Native.string "kind" (member "control" (entry read)));
        call
          "pause"
          "recurrence_pause"
          (mutation id read ~source:false ~key:"native-pause")
      | 11 ->
        ignore (complete "pause" : Jsonaf.t);
        call "paused" "recurrence_get" (lookup id)
      | 12 ->
        let read = complete "paused" in
        [%test_eq: string] "paused" (Native.string "kind" (member "control" (entry read)));
        call
          "cancel"
          "recurrence_cancel"
          (mutation id read ~source:false ~key:"native-cancel")
      | 13 ->
        ignore (complete "cancel" : Jsonaf.t);
        call "retained" "recurrence_list" (object_ [])
      | 14 ->
        let read = complete "retained" in
        let existing =
          object_
            [ "kind", `String "existing_session"
            ; "session", member "session" (basis read)
            ; ( "input"
              , P.Session.Message_content.to_json
                  { kind = Plain_text; text = "Explicit future input"; attachments = [] }
              )
            ]
        in
        call
          "existing"
          "recurrence_create"
          (create existing_id read ~target:existing ~key:"native-existing")
      | 15 ->
        ignore (complete "existing" : Jsonaf.t);
        call "history" "recurrence_history" (lookup existing_id)
      | 16 ->
        let history = complete "history" |> result in
        assert (List.is_empty (Native.array "occurrences" history));
        assert (List.is_empty (Native.array "protected_creations" history));
        []
      | _ -> failwith "unexpected native recurrence transcript stage")
    (fun state ->
       let catalog = Option.value_exn state.A.Session_state.recurrence_catalog in
       let cancelled = A.Recurrence_catalog.find catalog id |> Option.value_exn in
       let existing = A.Recurrence_catalog.find catalog existing_id |> Option.value_exn in
       [%test_eq: int] 2 (List.length (A.Recurrence_entry.definitions cancelled));
       assert (
         A.Recurrence_entry.equal_control
           (A.Recurrence_entry.control cancelled)
           (Retired Operator_retire));
       assert (
         A.Recurrence_entry.equal_control
           (A.Recurrence_entry.control existing)
           (Paused Operator_pause));
       [%test_eq: int] 6 (List.length (A.Recurrence_catalog.receipts catalog));
       assert (List.is_empty state.jobs);
       assert (List.is_empty state.schedules);
       print_endline
         "nine selected operations; exact historical create retry after edit; both \
          actual targets; no scheduled execution");
  [%expect
    {| nine selected operations; exact historical create retry after edit; both actual targets; no scheduled execution |}]
;;
