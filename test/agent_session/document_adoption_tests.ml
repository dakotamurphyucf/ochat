open! Core
open Fixtures
module A = Agent_session
module D = Document_schema
module P = Agent_protocol
module Delta = A.Session_delta

let limits = document_limits

let member json name =
  match D.Json.field json ~name with
  | Value value -> value
  | _ -> raise_s [%sexp "missing fixture field", (name : string)]
;;

let set json name value =
  match json with
  | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal name value)
  | _ -> raise_s [%sexp "expected fixture object"]
;;

let map_field json name ~f = set json name (f (member json name))

let map_array_first = function
  | `Array (first :: rest) -> fun ~f -> `Array (f first :: rest)
  | _ -> raise_s [%sexp "expected nonempty fixture array"]
;;

let edit_document document ~f =
  D.Document.inspect ~limits (f (D.Document.json document)) |> document_ok
;;

let edit_first_change document ~f =
  edit_document document ~f:(fun json ->
    map_field json "payload" ~f:(fun payload ->
      map_field payload "changes" ~f:(fun changes -> map_array_first changes ~f)))
;;

let capture delta =
  A.Session_delta_document.create
    delta
    ~limits
    ~state_document:A.Session_state_document.authored
  |> document_ok
;;

let apply delta initial =
  A.Session_delta_document.apply delta ~limits (A.Session_state_document.authored initial)
;;

let state_json state =
  A.Session_state_document.encode state ~limits |> document_ok |> D.Document.to_string
;;

let has json key = String.is_substring json ~substring:("\"" ^ key ^ "\"")

let%expect_test
    "created journal child carries envelope and nested future fields into checkpoint"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let delta = capture (Created initial) in
    let document =
      edit_first_change (A.Session_delta_document.document delta) ~f:(fun change ->
        map_field change "state" ~f:(fun state ->
          set state "future_envelope" (`String "retained")
          |> fun state ->
          map_field state "payload" ~f:(fun payload ->
            map_field payload "identity" ~f:(fun identity ->
              set identity "future_identity" `True))))
    in
    let delta = A.Session_delta_document.decode ~limits document |> document_ok in
    let state = apply delta initial |> document_ok in
    let json = state_json state in
    print_s
      [%sexp (has json "future_envelope" : bool), (has json "future_identity" : bool)]);
  [%expect {| (true true) |}]
;;

let history_delta ~retained ~future =
  let delta =
    capture
      (Batch
         [ Canonical_entries_appended [ actor_entry ]
         ; Canonical_history_replaced retained
         ])
  in
  if not future
  then delta
  else (
    let document =
      edit_first_change (A.Session_delta_document.document delta) ~f:(fun change ->
        map_field change "entries" ~f:(fun entries ->
          map_array_first entries ~f:(fun entry -> set entry "future_host" `True)))
    in
    A.Session_delta_document.decode ~limits document |> document_ok)
;;

let%expect_test
    "batch preserves earlier same-id fields and permits only known intermediate \
     retirement"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let retained =
      apply (history_delta ~retained:[ actor_entry ] ~future:true) initial |> document_ok
    in
    let clean_retirement = apply (history_delta ~retained:[] ~future:false) initial in
    let unknown_retirement = apply (history_delta ~retained:[] ~future:true) initial in
    print_s
      [%sexp
        (has (state_json retained) "future_host" : bool)
      , (Result.is_ok clean_retirement : bool)
      , (Result.is_error unknown_retirement : bool)]);
  [%expect {| (true true true) |}]
;;

let%expect_test
    "strict invocation projection admits future fields and adopts them across clean \
     updates"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let invocation = invocation_fixture () in
    let dispatched = P.Invocation.dispatch invocation |> protocol_ok in
    let delta =
      capture (Batch [ Invocation_changed invocation; Invocation_changed dispatched ])
    in
    let document =
      edit_first_change (A.Session_delta_document.document delta) ~f:(fun change ->
        map_field change "value" ~f:(fun value ->
          map_field value "context" ~f:(fun context ->
            set context "future_context" (`Object [ "opaque", `True ]))))
    in
    let delta = A.Session_delta_document.decode ~limits document |> document_ok in
    let state = apply delta initial |> document_ok in
    print_s [%sexp (has (state_json state) "future_context" : bool)]);
  [%expect {| true |}]
;;

let%expect_test
    "conflicting unknown values fail without discarding the prior state carrier"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let previous =
      apply (history_delta ~retained:[ actor_entry ] ~future:true) initial |> document_ok
    in
    let delta = capture (Canonical_history_replaced [ actor_entry ]) in
    let document =
      edit_first_change (A.Session_delta_document.document delta) ~f:(fun change ->
        map_field change "entries" ~f:(fun entries ->
          map_array_first entries ~f:(fun entry -> set entry "future_host" `False)))
    in
    let delta = A.Session_delta_document.decode ~limits document |> document_ok in
    let result = A.Session_delta_document.apply delta ~limits previous in
    print_s
      [%sexp
        (Result.is_error result : bool), (has (state_json previous) "future_host" : bool)]);
  [%expect {| (true true) |}]
;;

let%expect_test "empty named dictionary keys roundtrip" =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let initial =
      { initial with
        conversation = { initial.conversation with kv_store = [ "", "value" ] }
      }
    in
    let document = state_document initial in
    let restored =
      A.Session_state_document.decode ~limits document
      |> document_ok
      |> A.Session_state_document.value
    in
    print_s [%sexp (restored.conversation.kv_store : (string * string) list)]);
  [%expect {| (("" value)) |}]
;;

let%expect_test "immutable event retains future fields in strict receipt records" =
  let event =
    P.Event.Durable.of_payload
      ~session_id
      ~sequence:1L
      ~revision:1L
      ~timestamp
      (History_appended [ actor_entry ])
  in
  let authored = A.Durable_event_document.create event ~limits |> document_ok in
  let document =
    edit_document (A.Durable_event_document.document authored) ~f:(fun json ->
      set json "future_event_envelope" (`String "blob_future")
      |> fun json ->
      map_field json "payload" ~f:(fun payload ->
        map_field payload "payload" ~f:(fun entries ->
          map_array_first entries ~f:(fun entry -> set entry "future_receipt" `True))))
  in
  let restored = A.Durable_event_document.decode ~limits document |> document_ok in
  let value = A.Durable_event_document.value restored in
  print_s
    [%sexp
      (D.Json.equal
         (D.Document.json document)
         (D.Document.json (A.Durable_event_document.document restored))
       : bool)
    , (has (Jsonaf.to_string value.payload) "future_receipt" : bool)
    , (Result.is_ok (A.Durable_event_document.validate value) : bool)];
  [%expect {| (true true true) |}]
;;

let%expect_test
    "event replay retains captured document references and rejects mismatched \
     associations"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let snapshot =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
      |> A.Session_state.snapshot ~now:timestamp
    in
    let invocation = invocation_fixture () in
    let blob = P.Id.Blob.create () in
    let event =
      P.Event.Durable.of_payload
        ~session_id
        ~sequence:1L
        ~revision:1L
        ~timestamp
        (Session_updated snapshot.session)
      |> fun event ->
      P.Event.Durable.with_extension_status
        event
        [ P.Extension_status.invocation invocation ]
    in
    let authored = A.Durable_event_document.create event ~limits |> document_ok in
    let document =
      edit_document (A.Durable_event_document.document authored) ~f:(fun json ->
        set json "future_replay_reference" (`String (P.Id.Blob.to_string blob))
        |> fun json ->
        map_field json "payload" ~f:(fun payload ->
          map_field payload "payload" ~f:(fun payload ->
            map_field payload "extension_status" ~f:(fun statuses ->
              map_array_first statuses ~f:(fun status -> set status "future_status" `True)))))
    in
    let captured = A.Durable_event_document.decode ~limits document |> document_ok in
    let event = A.Durable_event_document.value captured in
    let log =
      A.Durable_event_log.create ~documents:[ captured ] ~capacity:1 [ event ]
      |> protocol_ok
    in
    let refs =
      A.Durable_event_log.retained_references
        log
        ~session_id
        ~candidates:[ blob ]
        ~max_events:2
        ~max_bytes:65536
      |> protocol_ok
    in
    let mismatch =
      { event with
        timestamp = P.Timestamp.of_string "2026-08-15T13:00:00Z" |> protocol_ok
      }
    in
    let bad_create =
      A.Durable_event_log.create ~documents:[ captured ] ~capacity:1 [ mismatch ]
    in
    let bad_append =
      A.Durable_event_log.append ~documents:[ captured ] log [ mismatch ]
    in
    let replay_unchanged =
      match A.Durable_event_log.replay log ~after_sequence:0L ~through_sequence:1L with
      | Available [ retained ] -> D.Json.equal retained.payload event.payload
      | _ -> false
    in
    print_s
      [%sexp
        (List.equal P.Id.Blob.equal [ blob ] refs : bool)
      , (Result.is_error bad_create : bool)
      , (Result.is_error bad_append : bool)
      , (replay_unchanged : bool)];
    let next =
      P.Event.Durable.of_payload
        ~session_id
        ~sequence:2L
        ~revision:2L
        ~timestamp
        (Moderator_notification `Null)
    in
    A.Durable_event_log.append log [ next ] |> protocol_ok;
    let refs =
      A.Durable_event_log.retained_references
        log
        ~session_id
        ~candidates:[ blob ]
        ~max_events:2
        ~max_bytes:65536
      |> protocol_ok
    in
    print_s [%sexp (List.is_empty refs : bool)]);
  [%expect
    {|
    (true true true true)
    true
    |}]
;;

let%expect_test "waiting job null completion schema preserves the absent domain value" =
  let dependency =
    P.Job.
      { invocation_id = (invocation_fixture ()).context.id
      ; work = Job (P.Id.Job.create ())
      ; deadline = timestamp
      ; completion_schema = None
      ; max_output_bytes = 1024
      ; max_output_depth = 8
      }
  in
  let job =
    P.Job.
      { id = P.Id.Job.create ()
      ; session_id
      ; generation = 0
      ; kind = Async_tool
      ; payload = `Null
      ; status = Waiting_completion dependency
      ; retry_policy = Never
      ; attempt = 1
      ; created_at = timestamp
      ; started_at = Some timestamp
      ; next_run_at = None
      ; completed_at = None
      ; result = None
      ; delivery = Pending
      ; launch = None
      ; progress = None
      }
  in
  let restored = P.Job.of_json (P.Job.to_json job) |> protocol_ok in
  (match A.Session_delta_document.value (capture (Job_changed job)) with
   | Batch [ Job_changed { status = Waiting_completion restored; _ } ] ->
     assert (P.Job.equal_dependency dependency restored)
   | _ -> assert false);
  let invalid =
    { job with
      status = Waiting_completion { dependency with completion_schema = Some `Null }
    }
  in
  List.iter
    [ Delta.Job_changed invalid; Batch [ Job_changed job; Job_changed invalid ] ]
    ~f:(fun value ->
      assert (
        Result.is_error
          (A.Session_delta_document.create
             value
             ~limits
             ~state_document:A.Session_state_document.authored)));
  (match restored.status with
   | Waiting_completion restored ->
     print_s
       [%sexp
         (Option.is_none restored.completion_schema : bool)
       , (P.Job.equal_dependency dependency restored : bool)]
   | _ -> raise_s [%sexp "waiting dependency changed status"]);
  [%expect {| (true true) |}]
;;

let%expect_test "authoring reference nested topics roundtrip and retain future fields" =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let hash = String.make 64 '0' in
    let response = `Object [ "answer", `True ] in
    let reference =
      P.Authoring_reference.create
        ~query_identity:hash
        ~host_identity:hash
        ~capability_fingerprint:hash
        ~scope:(P.Authoring_reference.scope_for ~session_id ~generation:0)
        ~surface_id:"one_off_v1"
        ~corpus_identity:hash
        ~response_sha256:
          Digestif.SHA256.(digest_string (Jsonaf.to_string response) |> to_hex)
        ~topics:
          [ { topic =
                { id = "runtime.example"
                ; document_sha256 = hash
                ; source = Installed hash
                ; complete = true
                }
            ; total_parts = 1
            ; parts = [ { index = 0; item_sha256 = hash } ]
            }
          ]
      |> protocol_ok
    in
    let admitted = invocation_fixture () in
    let dispatched = P.Invocation.dispatch admitted |> protocol_ok in
    let resolved =
      P.Invocation.resolve
        ~authoring_reference:reference
        dispatched
        ~session_id
        ~generation:0
        (Complete (`String (Jsonaf.to_string response)))
      |> protocol_ok
    in
    let delta =
      capture
        (Batch
           [ Invocation_changed admitted
           ; Invocation_changed dispatched
           ; Invocation_changed resolved
           ])
    in
    let document =
      edit_document (A.Session_delta_document.document delta) ~f:(fun json ->
        map_field json "payload" ~f:(fun payload ->
          map_field payload "changes" ~f:(function
            | `Array [ admitted; dispatched; resolved ] ->
              let resolved =
                map_field resolved "value" ~f:(fun value ->
                  map_field value "authoring_reference" ~f:(fun reference ->
                    map_field reference "topics" ~f:(fun topics ->
                      map_array_first topics ~f:(fun topic ->
                        map_field topic "topic" ~f:(fun topic ->
                          set topic "future_topic" `True)))))
              in
              `Array [ admitted; dispatched; resolved ]
            | _ -> failwith "expected admitted, dispatched, and resolved changes")))
    in
    let delta = A.Session_delta_document.decode ~limits document |> document_ok in
    let state = apply delta initial |> document_ok in
    let restored =
      A.Session_state_document.encode state ~limits
      |> document_ok
      |> A.Session_state_document.decode ~limits
      |> document_ok
    in
    let published = P.Invocation.publish resolved |> protocol_ok in
    let updated =
      A.Session_delta_document.apply
        (capture (Invocation_changed published))
        ~limits
        restored
      |> document_ok
    in
    let restored_invocation =
      List.hd_exn (A.Session_state_document.value updated).invocations
    in
    print_s
      [%sexp
        (P.Invocation.equal published restored_invocation : bool)
      , (has (state_json updated) "future_topic" : bool)]);
  [%expect {| (true true) |}]
;;

let extension_conflict = function
  | Error (D.Error.Extension_conflict _) -> true
  | Ok _ | Error _ -> false
;;

let%expect_test "prior checkpoint carrier blocks unknown-bearing history retirement" =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let known =
      apply (capture (Canonical_entries_appended [ actor_entry ])) initial |> document_ok
    in
    let checkpoint =
      A.Session_state_document.encode known ~limits
      |> document_ok
      |> fun document ->
      edit_document document ~f:(fun json ->
        set json "future_checkpoint" (`Number "1e+00")
        |> fun json ->
        map_field json "payload" ~f:(fun payload ->
          map_field payload "conversation" ~f:(fun conversation ->
            map_field conversation "canonical_history" ~f:(fun entries ->
              map_array_first entries ~f:(fun entry ->
                set entry "future_host" (`Object [ "opaque", `Number "1e+00" ]))))))
    in
    let previous = A.Session_state_document.decode ~limits checkpoint |> document_ok in
    let before = state_json previous in
    let updated =
      A.Session_delta_document.apply
        (capture (Canonical_history_replaced [ actor_entry ]))
        ~limits
        previous
      |> document_ok
    in
    let retired =
      A.Session_delta_document.apply
        (capture (Canonical_history_replaced []))
        ~limits
        previous
    in
    let clean_retired =
      A.Session_delta_document.apply
        (capture (Canonical_history_replaced []))
        ~limits
        known
    in
    let updated_document =
      A.Session_state_document.encode updated ~limits |> document_ok
    in
    print_s
      [%sexp
        (D.Json.equal (D.Document.json checkpoint) (D.Document.json updated_document)
         : bool)
      , (extension_conflict retired : bool)
      , (String.equal before (state_json previous) : bool)
      , (Result.is_ok clean_retired : bool)]);
  [%expect {| (true true true true) |}]
;;

let%expect_test "nullable scalar preservation rejects prior and intermediate retirement" =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let failure =
      P.Error.create Invalid_state ~message:"preservation fixture" ~retryable:false ()
    in
    let known = apply (capture (Failure_changed (Some failure))) initial |> document_ok in
    let checkpoint =
      A.Session_state_document.encode known ~limits
      |> document_ok
      |> fun document ->
      edit_document document ~f:(fun json ->
        map_field json "payload" ~f:(fun payload ->
          map_field payload "failure" ~f:(fun failure ->
            set failure "future_failure" (`Object [ "opaque", `Number "1e+00" ]))))
    in
    let previous = A.Session_state_document.decode ~limits checkpoint |> document_ok in
    let before = state_json previous in
    let retained =
      A.Session_delta_document.apply
        (capture (Failure_changed (Some failure)))
        ~limits
        previous
      |> document_ok
    in
    let retired =
      A.Session_delta_document.apply (capture (Failure_changed None)) ~limits previous
    in
    let clean_batch =
      capture (Batch [ Failure_changed (Some failure); Failure_changed None ])
    in
    let captured_batch =
      edit_first_change (A.Session_delta_document.document clean_batch) ~f:(fun change ->
        map_field change "failure" ~f:(fun failure ->
          set failure "future_failure" (`Object [ "opaque", `Number "1e+00" ])))
      |> A.Session_delta_document.decode ~limits
      |> document_ok
    in
    let retained_document =
      A.Session_state_document.encode retained ~limits |> document_ok
    in
    print_s
      [%sexp
        (D.Json.equal (D.Document.json checkpoint) (D.Document.json retained_document)
         : bool)
      , (extension_conflict retired : bool)
      , (String.equal before (state_json previous) : bool)
      , (Result.is_ok (apply clean_batch initial) : bool)
      , (extension_conflict (apply captured_batch initial) : bool)]);
  [%expect {| (true true true true true) |}]
;;

let reference_transaction_metadata
      carrier
      ~updated_at
      ~revision
      ~transaction_sequence
      ~last_event_sequence
  =
  let state = A.Session_state_document.value carrier in
  let state =
    { state with
      identity = { state.identity with updated_at }
    ; counters =
        { state.counters with
          revision
        ; transaction_sequence
        ; event_sequence =
            Option.value last_event_sequence ~default:state.counters.event_sequence
        }
    }
  in
  A.Session_state_document.with_value carrier state
  |> A.Session_state_document.encode ~limits
  |> document_ok
;;

let limits_with_max_bytes max_bytes =
  D.Limits.create ~max_bytes ~max_depth:128 ~max_fields:100_000 ~max_nodes:1_000_000
  |> document_ok
;;

let byte_limit = function
  | Error (D.Error.Limit_exceeded bound) -> String.equal bound "bytes"
  | Ok _ | Error _ -> false
;;

let%expect_test "transaction metadata validates all counter inputs" =
  let create revision transaction_sequence last_event_sequence =
    A.Session_delta_document.Transaction_metadata.create
      ~updated_at:timestamp
      ~revision
      ~transaction_sequence
      ~last_event_sequence
  in
  print_s
    [%sexp
      (Result.is_error (create (-1L) 0L None) : bool)
    , (Result.is_error (create 0L (-1L) None) : bool)
    , (Result.is_error (create 0L 0L (Some (-1L))) : bool)
    , (Result.is_ok (create Int64.max_value Int64.max_value (Some Int64.max_value))
       : bool)];
  [%expect {| (true true true true) |}]
;;

let%expect_test "transaction metadata matches carried reference with and without events" =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let initial =
      { initial with
        counters = { initial.counters with revision = 2L; event_sequence = 3L }
      }
    in
    let delta = history_delta ~retained:[ actor_entry ] ~future:true in
    let unstamped = apply delta initial |> document_ok in
    let checkpoint =
      A.Session_state_document.encode unstamped ~limits
      |> document_ok
      |> fun document ->
      edit_document document ~f:(fun json ->
        set json "future_checkpoint" (`Number "1e+00")
        |> fun json ->
        map_field json "payload" ~f:(fun payload ->
          map_field payload "identity" ~f:(fun identity ->
            set identity "future_identity" (`Object [ "opaque", `Number "1e+00" ]))
          |> fun payload ->
          map_field payload "counters" ~f:(fun counters ->
            set counters "future_counters" (`Object [ "opaque", `Number "1e+00" ]))))
    in
    let previous = A.Session_state_document.decode ~limits checkpoint |> document_ok in
    let before = state_json previous in
    let delta = capture (Canonical_history_replaced [ actor_entry ]) in
    let created = A.Session_state_document.value previous in
    let created =
      { created with counters = { created.counters with event_sequence = 21L } }
    in
    let updated_at = P.Timestamp.of_string "2026-08-15T14:00:00Z" |> protocol_ok in
    List.iter
      [ delta, Some 42L; delta, None; capture (Created created), None ]
      ~f:(fun (delta, last_event_sequence) ->
        let unstamped =
          A.Session_delta_document.apply delta ~limits previous |> document_ok
        in
        let metadata =
          A.Session_delta_document.Transaction_metadata.create
            ~updated_at
            ~revision:7L
            ~transaction_sequence:9L
            ~last_event_sequence
          |> document_ok
        in
        let actual =
          A.Session_delta_document.apply
            delta
            ~transaction_metadata:metadata
            ~limits
            previous
          |> document_ok
        in
        let actual_document =
          A.Session_state_document.encode actual ~limits |> document_ok
        in
        let expected =
          reference_transaction_metadata
            unstamped
            ~updated_at
            ~revision:7L
            ~transaction_sequence:9L
            ~last_event_sequence
        in
        let retired =
          A.Session_delta_document.apply
            (capture (Canonical_history_replaced []))
            ~transaction_metadata:metadata
            ~limits
            previous
        in
        print_s
          [%sexp
            (D.Json.equal (D.Document.json expected) (D.Document.json actual_document)
             : bool)
          , ((A.Session_state_document.value actual).counters.event_sequence : int64)
          , (extension_conflict retired : bool)
          , (String.equal before (state_json previous) : bool)]));
  [%expect
    {|
    (true 42 true true)
    (true 3 true true)
    (true 21 true true)
    |}]
;;

let%expect_test
    "transaction metadata cannot repair native counters or intermediate bounds"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let metadata =
      A.Session_delta_document.Transaction_metadata.create
        ~updated_at:timestamp
        ~revision:0L
        ~transaction_sequence:1L
        ~last_event_sequence:None
      |> document_ok
    in
    let delta = capture (Failure_changed None) in
    let invalid =
      { initial with counters = { initial.counters with revision = -1L } }
      |> A.Session_state_document.authored
    in
    let repaired =
      A.Session_delta_document.apply delta ~transaction_metadata:metadata ~limits invalid
    in
    let large =
      { initial with counters = { initial.counters with revision = Int64.max_value } }
      |> A.Session_state_document.authored
    in
    let smaller =
      reference_transaction_metadata
        large
        ~updated_at:timestamp
        ~revision:0L
        ~transaction_sequence:1L
        ~last_event_sequence:None
    in
    let tight = limits_with_max_bytes (String.length (D.Document.to_string smaller)) in
    let before = state_json large in
    let too_large =
      A.Session_delta_document.apply
        delta
        ~transaction_metadata:metadata
        ~limits:tight
        large
    in
    print_s
      [%sexp
        (Result.is_error repaired : bool)
      , (byte_limit too_large : bool)
      , (String.equal before (state_json large) : bool)]);
  [%expect {| (true true true) |}]
;;

let%expect_test "transaction metadata enforces final bounds including retained unknowns" =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let checkpoint =
      state_document initial
      |> fun document ->
      edit_document document ~f:(fun json ->
        set json "future_checkpoint" (`Object [ "opaque", `Number "1e+00" ]))
    in
    let previous = A.Session_state_document.decode ~limits checkpoint |> document_ok in
    let before = state_json previous in
    let tight = limits_with_max_bytes (String.length before) in
    let metadata =
      A.Session_delta_document.Transaction_metadata.create
        ~updated_at:timestamp
        ~revision:Int64.max_value
        ~transaction_sequence:Int64.max_value
        ~last_event_sequence:(Some Int64.max_value)
      |> document_ok
    in
    let delta = capture (Failure_changed None) in
    let unstamped = A.Session_delta_document.apply delta ~limits:tight previous in
    let stamped =
      A.Session_delta_document.apply
        delta
        ~transaction_metadata:metadata
        ~limits:tight
        previous
    in
    print_s
      [%sexp
        (Result.is_ok unstamped : bool)
      , (byte_limit stamped : bool)
      , (String.equal before (state_json previous) : bool)]);
  [%expect {| (true true true) |}]
;;

let%expect_test
    "document-only replay matches restored replay and preserves future carriers"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let initial =
      { initial with
        conversation = { initial.conversation with canonical_history = [ actor_entry ] }
      }
    in
    let document =
      state_document initial
      |> fun document ->
      edit_document document ~f:(fun json ->
        set json "future_envelope" (`Number "1e+00")
        |> fun json ->
        map_field json "payload" ~f:(fun payload ->
          map_field payload "conversation" ~f:(fun conversation ->
            map_field conversation "canonical_history" ~f:(fun entries ->
              map_array_first entries ~f:(fun entry ->
                set entry "future_host" (`Object [ "raw", `Number "1.00" ]))))))
    in
    let snapshot =
      Agent_store.Snapshot.create
        ~limits
        ~session_id
        ~transaction_sequence:0L
        ~transaction_hash:None
        ~event_sequence:0L
        ~created_at:initial.identity.updated_at
        ~prompt_artifact:(P.Id.Prompt_revision.to_string initial.spec.prompt_revision_id)
        ~workspace_identity:initial.spec.workspace_instance.conflict_domain
        ~payload:document
      |> store_ok
    in
    let restored = A.Session_persistence.restore_snapshot ~limits snapshot |> store_ok in
    let original = A.Session_persistence.Restored.state_document restored in
    let before = state_json original in
    let transaction delta =
      Agent_store.Transaction.create
        ~limits
        ~session_id
        ~generation:0
        ~transaction_sequence:1L
        ~previous_transaction_hash:None
        ~session_revision:1L
        ~first_event_sequence:None
        ~last_event_sequence:None
        ~accepted_at_ns:
          (P.Timestamp.to_time_ns timestamp
           |> Time_ns.to_int_ns_since_epoch
           |> Int64.of_int)
        ~command_audit:None
        ~delta:(delta_document delta)
        ~durable_events:[]
      |> store_ok
    in
    let update = transaction (Canonical_history_replaced [ actor_entry ]) in
    let pure = A.Session_persistence.apply_document original ~limits update |> store_ok in
    let wrapped =
      A.Session_persistence.apply_transaction ~limits restored update
      |> store_ok
      |> A.Session_persistence.Restored.state_document
    in
    [%test_eq: string] (state_json pure) (state_json wrapped);
    assert (String.is_substring (state_json pure) ~substring:"1e+00");
    assert (String.is_substring (state_json pure) ~substring:"1.00");
    let deletion = transaction (Canonical_history_replaced []) in
    let refuses = function
      | Error (Agent_store.Store_error.Document (D.Error.Extension_conflict _)) -> true
      | Ok _ | Error _ -> false
    in
    assert (refuses (A.Session_persistence.apply_document original ~limits deletion));
    assert (refuses (A.Session_persistence.apply_transaction ~limits restored deletion));
    [%test_eq: string] before (state_json original));
  [%expect {| |}]
;;

let%expect_test "delta replay preserves exact carried object order versus native snapshot"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let unusual_order json =
      match json with
      | `Object (first :: rest) ->
        `Object
          (first
           :: ("future_between", `Object [ "raw", `Number "1e+00" ])
           :: List.rev rest)
      | _ -> raise_s [%sexp "expected fixture object"]
    in
    let original =
      A.Session_state_document.encode (A.Session_state_document.authored initial) ~limits
      |> document_ok
      |> fun document ->
      edit_document document ~f:(fun json ->
        map_field json "payload" ~f:(fun payload ->
          map_field payload "conversation" ~f:unusual_order
          |> fun payload -> map_field payload "identity" ~f:unusual_order |> unusual_order)
        |> unusual_order)
      |> A.Session_state_document.decode ~limits
      |> document_ok
    in
    let before = state_json original in
    let native_delta : Delta.t = Canonical_entries_appended [ actor_entry ] in
    let changed =
      Delta.apply (A.Session_state_document.value original) native_delta |> protocol_ok
    in
    let expected = A.Session_state_document.with_value original changed in
    let replayed =
      A.Session_delta_document.apply (capture native_delta) ~limits original
      |> document_ok
    in
    [%test_eq: string] (state_json expected) (state_json replayed);
    [%test_eq: string] before (state_json original);
    assert (String.is_substring (state_json replayed) ~substring:"1e+00"));
  [%expect {| |}]
;;

let%expect_test "live commits and snapshots retain the exact admitted replay basis" =
  let module Store = Agent_store in
  let module Persistence = A.Session_persistence in
  let run basis =
    with_actor_workspace (fun env workspace_instance ->
      Eio.Switch.run (fun sw ->
        let queued : P.Job.t =
          { id = P.Id.Job.create ()
          ; session_id
          ; generation = 0
          ; kind = Model_call
          ; payload = `Object [ "raw", `Number "1e+00" ]
          ; status = Queued
          ; retry_policy = Never
          ; attempt = 0
          ; created_at = timestamp
          ; started_at = None
          ; next_run_at = None
          ; completed_at = None
          ; result = None
          ; delivery = Pending
          ; launch = None
          ; progress = None
          }
        in
        let initial =
          actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
          |> fun (state : A.Session_state.t) ->
          { state with
            jobs = (if Poly.equal basis `New_job then [] else [ queued ])
          ; model_job_targets =
              (if Poly.equal basis `New_job
               then []
               else [ model_job_binding state queued ])
          }
        in
        let document =
          A.Session_state_document.encode
            (A.Session_state_document.authored initial)
            ~limits
          |> document_ok
          |> fun document ->
          edit_document document ~f:(fun json ->
            set json "future_checkpoint" (`Number "1e+00")
            |> fun json ->
            if not (Poly.equal basis `Captured)
            then json
            else
              map_field json "payload" ~f:(fun payload ->
                map_field payload "jobs" ~f:(fun jobs ->
                  map_array_first jobs ~f:(function
                    | `Object (first :: rest) ->
                      `Object
                        (first
                         :: ("future_job", `Object [ "raw", `Number "1e+00" ])
                         :: List.rev rest)
                    | _ -> raise_s [%sexp "expected job fixture object"]))))
        in
        let restored =
          if Poly.equal basis `Authored
          then Persistence.Restored.authored initial
          else
            Store.Snapshot.create
              ~limits
              ~session_id
              ~transaction_sequence:0L
              ~transaction_hash:None
              ~event_sequence:0L
              ~created_at:initial.identity.updated_at
              ~prompt_artifact:
                (P.Id.Prompt_revision.to_string initial.spec.prompt_revision_id)
              ~workspace_identity:initial.spec.workspace_instance.conflict_domain
              ~payload:document
            |> store_ok
            |> Persistence.restore_snapshot ~limits
            |> store_ok
        in
        let storage = Job_artifact_fixtures.create env sw initial in
        let handle = storage.session in
        let journal =
          Store.Journal.create
            ~env
            ~directory:(Store.Session_store.Handle.journal_directory handle)
            ~max_payload_length:1048576
            ~max_segment_bytes:4194304L
            ~max_segment_frames:16
          |> store_ok
        in
        let writer =
          Store.Commit_writer.create
            ~sw
            ~journal
            ~session_id
            ~next_transaction_sequence:1L
            ~previous_transaction_hash:None
            ~queue_capacity:8
          |> store_ok
        in
        Exn.protect
          ~finally:(fun () -> Store.Commit_writer.close writer)
          ~f:(fun () ->
            let persistence =
              Persistence.create
                ~retention_preflight:None
                ~writer
                ~durability:Flush
                ~limits
                ~archive_limits:limits
                ~restored
                ~previous_transaction_hash:None
                ~command_accepted:(fun _ _ -> ())
                ~archive:(fun _ _ -> failwith "job fixture unexpectedly archived history")
            in
            let state = ref initial in
            let snapshot () =
              Persistence.install_snapshot
                persistence
                ~env
                ~handle
                ~max_payload_length:1048576
                ~transaction_hash:(Persistence.transaction_hash persistence)
                !state
              |> store_ok
            in
            let initial_snapshot = snapshot () in
            let original =
              Persistence.restore_snapshot ~limits initial_snapshot.snapshot
              |> store_ok
              |> Persistence.Restored.state_document
            in
            let live_document () =
              Persistence.restored persistence |> Persistence.Restored.state_document
            in
            [%test_eq: string] (state_json original) (state_json (live_document ()));
            let replay () =
              let scan = Store.Journal.scan journal |> store_ok in
              List.fold scan.entries ~init:original ~f:(fun document entry ->
                let record =
                  Store.Document_record.of_frame entry.frame ~limits ~expected_digest:None
                  |> Result.map_error ~f:(fun error ->
                    Sexp.to_string_hum (Store.Document_record.Error.sexp_of_t error))
                  |> Result.ok_or_failwith
                in
                let transaction =
                  Store.Transaction.decode_record record ~limits |> store_ok
                in
                Persistence.apply_document document ~limits transaction |> store_ok)
            in
            let check_snapshot ~install =
              let replayed = replay () in
              let live = state_json (live_document ()) in
              [%test_eq: string] live (state_json replayed);
              assert (String.is_substring live ~substring:"1e+00");
              if install
              then (
                let installed = snapshot () in
                [%test_eq: string] live (D.Document.to_string installed.snapshot.payload);
                [%test_eq: string] live (state_json (live_document ())))
            in
            let running =
              { queued with status = Running; attempt = 1; started_at = Some timestamp }
            in
            let succeeded =
              { running with
                status = Succeeded
              ; completed_at = Some timestamp
              ; result = Some (`String "finished")
              }
            in
            let phases =
              if Poly.equal basis `New_job
              then [ queued; running; succeeded ]
              else [ running; succeeded ]
            in
            List.iter phases ~f:(fun job ->
              let transition =
                A.Session_transition.apply
                  ~now:timestamp
                  !state
                  ~delta:(Job_changed job)
                  ~payloads:[]
                |> protocol_ok
              in
              Persistence.commit
                persistence
                ~command_audit:None
                ~previous:!state
                transition
              |> protocol_ok;
              state := transition.state;
              (* Leave the newly committed queued job unsnapshotted: only the live
               commit can establish its basis before the Running transition. *)
              check_snapshot
                ~install:
                  (match job.status with
                   | Queued -> false
                   | _ -> true));
            (* An acknowledgement failure cannot publish a newly prepared basis. *)
            Store.Commit_writer.close writer;
            let before = state_json (live_document ()) in
            let before_hash = Persistence.transaction_hash persistence in
            let before_snapshot =
              Store.Snapshot.load_current
                ~env
                ~directory:(Store.Session_store.Handle.snapshot_directory handle)
                ~max_payload_length:1048576
              |> store_ok
              |> Option.value_exn
            in
            let transition =
              A.Session_transition.apply
                ~now:timestamp
                !state
                ~delta:(Canonical_entries_appended [ actor_entry ])
                ~payloads:[]
              |> protocol_ok
            in
            assert (
              Result.is_error
                (Persistence.commit
                   persistence
                   ~command_audit:None
                   ~previous:!state
                   transition));
            [%test_eq: string] before (state_json (live_document ()));
            [%test_eq: string option]
              before_hash
              (Persistence.transaction_hash persistence);
            let after_snapshot =
              Store.Snapshot.load_current
                ~env
                ~directory:(Store.Session_store.Handle.snapshot_directory handle)
                ~max_payload_length:1048576
              |> store_ok
              |> Option.value_exn
            in
            [%test_eq: string] before_snapshot.filename after_snapshot.filename)))
  in
  List.iter [ `Authored; `New_job; `Captured ] ~f:run;
  [%expect {| |}]
;;
