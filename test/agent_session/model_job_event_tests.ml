open! Core
module P = Agent_protocol
module Event = Agent_session.Model_job_event
module Value = Chatml.Chatml_value_codec
module L = Chatml.Chatml_lang

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let timestamp = P.Timestamp.of_string "2026-11-14T00:00:00Z" |> protocol_ok
let id = P.Id.Job.of_string "job_model_completion_fixture" |> protocol_ok
let session_id = P.Id.Session.of_string "ses_model_completion_fixture" |> protocol_ok
let recipe = "agent_prompt_v1"

let job status result : P.Job.t =
  { id
  ; session_id
  ; generation = 0
  ; kind = Model_call
  ; payload = `Object [ "recipe", `String recipe ]
  ; status
  ; retry_policy = Never
  ; attempt = 2
  ; created_at = timestamp
  ; started_at = Some timestamp
  ; next_run_at = None
  ; completed_at = Some timestamp
  ; result
  ; delivery = Pending
  ; launch = None
  ; inference_origin = Some Unbound
  ; progress = None
  }
;;

let same_value actual expected =
  let snapshot value = Value.Snapshot.of_value value |> Result.ok_or_failwith in
  assert (
    Sexp.equal
      (Value.Snapshot.sexp_of_t (snapshot actual))
      (Value.Snapshot.sexp_of_t (snapshot expected)))
;;

let json = function
  | L.VVariant ("Internal_event", [ value ]) ->
    Value.value_to_jsonaf_result value |> Result.ok_or_failwith
  | _ -> failwith "modern Model completion is not an Internal_event"
;;

let%expect_test "Model completion preserves legacy bytes and projects bounded v1 JSON" =
  let error =
    P.Error.create Internal_error ~message:"fixture provider failed" ~retryable:true ()
  in
  let result = `Object [ "answer", `String "actual Model result" ] in
  List.iter
    [ ( P.Job.Succeeded
      , Some result
      , L.VVariant
          ( "Model_job_succeeded"
          , [ VString "job_model_completion_fixture"
            ; VString recipe
            ; Value.jsonaf_to_value result
            ] )
      , P.Completion.Succeeded result )
    ; ( Failed error
      , None
      , VVariant
          ( "Model_job_failed"
          , [ VString "job_model_completion_fixture"
            ; VString recipe
            ; VString error.message
            ] )
      , Failed
          { code = "internal_error"
          ; message = error.message
          ; retryable = true
          ; details = error.data
          } )
    ; ( Cancelled
      , None
      , VVariant
          ( "Model_job_failed"
          , [ VString "job_model_completion_fixture"
            ; VString recipe
            ; VString "job was cancelled"
            ] )
      , Cancelled "job cancelled" )
    ; ( Interrupted "fixture interrupted"
      , None
      , VVariant
          ( "Model_job_failed"
          , [ VString "job_model_completion_fixture"
            ; VString recipe
            ; VString "fixture interrupted"
            ] )
      , Failed
          { code = "interrupted"
          ; message = "fixture interrupted"
          ; retryable = false
          ; details = `Null
          } )
    ]
    ~f:(fun (status, result, legacy, completion) ->
      let job = job status result in
      List.iter
        [ Event.legacy job
        ; Event.render job
        ; Agent_session.Runtime_builder.model_job_event job
        ]
        ~f:(fun actual -> same_value (protocol_ok actual) legacy);
      let modern = Event.render ~format:Extensibility_v1 job |> protocol_ok |> json in
      let expected =
        `Object
          [ "kind", `String "model_job_completed"
          ; "job_id", `String "job_model_completion_fixture"
          ; "attempt", `String "2"
          ; "recipe", `String recipe
          ; "result", P.Stored_completion.to_json (Inline completion)
          ]
      in
      assert (Jsonaf.exactly_equal modern expected));
  let succeeded = job Succeeded (Some result) in
  let dependency : P.Job.dependency =
    { invocation_id =
        P.Id.Invocation.of_string "inv_model_completion_fixture" |> protocol_ok
    ; work = Job id
    ; deadline = timestamp
    ; completion_schema = None
    ; max_output_bytes = 1024
    ; max_output_depth = 8
    }
  in
  let wrong_kind =
    List.map
      [ P.Job.Async_tool; Nested_agent; Scheduled_event; Shell_process; Compaction ]
      ~f:(fun kind -> { succeeded with kind })
  in
  let nonterminal =
    List.map
      [ P.Job.Queued
      ; Running
      ; Waiting_permission
          (P.Id.Permission.of_string "per_model_completion_fixture" |> protocol_ok)
      ; Waiting_completion dependency
      ]
      ~f:(fun status -> { succeeded with status })
  in
  List.iter
    (wrong_kind
     @ nonterminal
     @ [ { succeeded with payload = `Null }
       ; { succeeded with result = None }
       ; { succeeded with result = Some (`String (String.make ((1024 * 1024) + 1) 'x')) }
       ])
    ~f:(fun rejected ->
      match Event.render ~format:Extensibility_v1 rejected with
      | Error error ->
        [%test_eq: P.Error.code] Invalid_state error.code;
        [%test_eq: bool] false error.retryable
      | Ok _ -> failwith "invalid or oversized modern Model completion rendered");
  print_endline
    "legacy terminal variants unchanged; v1 terminal JSON exact; invalid and oversized \
     reject";
  [%expect
    {| legacy terminal variants unchanged; v1 terminal JSON exact; invalid and oversized reject |}]
;;
