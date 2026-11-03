open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module D = Document_schema
module SD = A.Session_state_document

let set = Goal_catalog_tests.set
let member = Goal_catalog_tests.member

let job ?(attempt = 0) ?inference_origin () : P.Job.t =
  { id = P.Id.Job.of_string "job_inference_origin" |> protocol_ok
  ; session_id
  ; generation = 0
  ; kind = Async_tool
  ; payload = `Null
  ; status = Queued
  ; retry_policy = Safe_retry { max_attempts = 3; backoff_ms = 0 }
  ; attempt
  ; created_at = timestamp
  ; started_at = None
  ; next_run_at = None
  ; completed_at = None
  ; result = None
  ; delivery = Pending
  ; launch = None
  ; inference_origin
  ; progress = None
  }
;;

let transition state delta =
  A.Session_transition.apply ~now:timestamp state ~delta ~payloads:[] |> protocol_ok
;;

(* Imported storage history. It grants no callback, completion or inference
   permission. The ordinary admission fixture supplies the real retained index. *)
let associated state ~attempt =
  let index = Option.value_exn state.A.Session_state.run_state in
  let previous = List.hd_exn (A.Run_state.runs index) in
  let job = job ~attempt ~inference_origin:(Run previous.id) () in
  let work = A.Run_job_occurrence.work job |> protocol_ok in
  let revision = Int64.succ previous.revision in
  let history, terminal =
    if Int.equal attempt 0
    then [], []
    else (
      let old =
        P.Run_work.create
          ~key:(Retained (Job { id = job.id; attempt }))
          ~generation:job.generation
        |> protocol_ok
      in
      ( [ old ]
      , [ P.Run_work.Terminal.create ~work:old ~outcome:Failed ~revision |> protocol_ok ]
      ))
  in
  let run =
    P.Run.create
      ~id:previous.id
      ~session:previous.session
      ~principal_id:previous.principal_id
      ~source:previous.source
      ~mode:previous.mode
      ~lifecycle:Active
      ~revision
      ~owned_work:(previous.owned_work @ history @ [ work ])
      ~relinquished_work:previous.relinquished_work
      ~terminal_work:(previous.terminal_work @ terminal)
      ~created_at:previous.created_at
      ~updated_at:timestamp
    |> protocol_ok
  in
  let index = A.Run_state.advance index ~run ~intents:[] |> protocol_ok in
  let delta = A.Session_delta.Batch [ Job_changed job; Run_state_changed index ] in
  (transition state delta).state, job
;;

let%expect_test
    "origin codecs distinguish unavailable, unbound and immutable Run evidence"
  =
  let run_id = P.Id.Run.of_string "run_origin" |> protocol_ok in
  let historical = job () in
  List.iter
    [ None; Some P.Job.Inference_origin.Unbound; Some (Run run_id) ]
    ~f:(fun origin ->
      let value = { historical with inference_origin = origin } in
      let decoded = P.Job.to_json value |> P.Job.of_json |> protocol_ok in
      assert (Option.equal P.Job.Inference_origin.equal origin decoded.inference_origin));
  let bad = P.Id.Run.t_of_sexp (Sexp.Atom "invalid") in
  assert (Result.is_error (P.Job.Inference_origin.validate (Run bad)));
  assert (
    Exn.does_raise (fun () ->
      P.Job.Inference_origin.t_of_sexp (List [ Atom "Run"; Atom "invalid" ]) |> ignore));
  assert (
    Result.is_error
      (P.Job.of_json (set (P.Job.to_json historical) "inference_origin" `Null)));
  let bound = { historical with inference_origin = Some (Run run_id) } in
  List.iter
    [ historical, bound
    ; bound, historical
    ; bound, { bound with inference_origin = Some Unbound }
    ]
    ~f:(fun (previous, incoming) ->
      assert (
        Result.is_error
          (P.Job.validate_inference_origin_transition ~previous:(Some previous) incoming)));
  print_endline "strict optional codec and lifetime identity";
  [%expect {| strict optional codec and lifetime identity |}]
;;

let%expect_test "final carriers preserve origin unknowns and queued cancellation history" =
  Run_admission_tests.with_startup_actor (fun actor _ _ request start _ _ _ _ ->
    start (request "origin-carriers") |> protocol_ok |> ignore;
    let basis = A.Session_actor.state actor |> protocol_ok in
    List.iter [ 0; 1 ] ~f:(fun attempt ->
      let state, job = associated basis ~attempt in
      let reverse =
        transition
          basis
          (Batch [ Run_state_changed (Option.value_exn state.run_state); Job_changed job ])
      in
      assert (
        Option.equal
          P.Job.Inference_origin.equal
          (List.hd_exn reverse.state.jobs).inference_origin
          job.inference_origin);
      let document =
        SD.authored state |> SD.encode ~limits:document_limits |> document_ok
      in
      let json = D.Document.json document in
      let payload = member json "payload" in
      let raw_job = P.Job.to_json job in
      let raw_job = set raw_job "future_job" (`Object [ "opaque", `String "exact" ]) in
      let raw_job =
        set
          raw_job
          "inference_origin"
          (set (member raw_job "inference_origin") "future_origin" (`String "retained"))
      in
      let raw = set json "payload" (set payload "jobs" (`Array [ raw_job ])) in
      let document = D.Document.inspect ~limits:document_limits raw |> document_ok in
      let carrier = SD.decode ~limits:document_limits document |> document_ok in
      let encoded = SD.encode carrier ~limits:document_limits |> document_ok in
      assert (D.Json.equal raw (D.Document.json encoded));
      let conflicting_job =
        set
          raw_job
          "inference_origin"
          (set
             (member raw_job "inference_origin")
             "future_origin"
             (`String "conflicting"))
      in
      let conflicting =
        set raw "payload" (set (member raw "payload") "jobs" (`Array [ conflicting_job ]))
        |> D.Document.inspect ~limits:document_limits
        |> document_ok
        |> SD.decode ~limits:document_limits
        |> document_ok
      in
      assert (Result.is_error (SD.adopt carrier ~limits:document_limits conflicting));
      let changed_job = { job with inference_origin = None } in
      let changed = { state with jobs = [ changed_job ] } in
      assert (
        Result.is_error
          (SD.encode (SD.with_value carrier changed) ~limits:document_limits));
      assert (
        Result.is_error (SD.adopt carrier ~limits:document_limits (SD.authored changed)));
      assert (Result.is_error (A.Session_delta.apply state (Job_changed changed_job)));
      assert (Result.is_error (A.Session_delta.apply state (Created changed)));
      let rewritten =
        SD.authored changed |> SD.encode ~limits:document_limits |> document_ok
      in
      assert (Result.is_error (SD.decode_after carrier ~limits:document_limits rewritten));
      List.iter
        [ { job with
            inference_origin =
              Some (Run (P.Id.Run.of_string "run_foreign_origin" |> protocol_ok))
          }
        ; { job with generation = Int.succ job.generation }
        ; { job with
            session_id = P.Id.Session.of_string "ses_foreign_origin" |> protocol_ok
          }
        ]
        ~f:(fun invalid ->
          assert (
            Result.is_error
              (A.Job_inference_provenance.validate
                 ~jobs:[ invalid ]
                 ~runs:state.run_state)));
      assert (
        Result.is_error
          (A.Session_state.validate
             { state with jobs = [ { job with inference_origin = Some Unbound } ] }));
      assert (Result.is_error (A.Session_state.validate { state with run_state = None }));
      let deltas =
        A.Run_job_completion.cancel_queued state ~job ~now:timestamp |> protocol_ok
      in
      let cancelled =
        { job with
          status = Cancelled
        ; completed_at = Some timestamp
        ; result = Some (P.Completion.to_json (Cancelled "cancelled before claim"))
        }
      in
      let final = transition state (Batch (Job_changed cancelled :: deltas)) in
      [%test_eq: int] (List.hd_exn final.state.jobs).attempt attempt;
      let final_carrier = SD.with_value carrier final.state in
      let final_document =
        SD.encode final_carrier ~limits:document_limits |> document_ok
      in
      let reopened =
        SD.decode ~limits:document_limits final_document |> document_ok |> SD.value
      in
      assert (
        Option.equal
          P.Job.Inference_origin.equal
          (List.hd_exn reopened.jobs).inference_origin
          job.inference_origin);
      let preserved_job =
        member (D.Document.json final_document) "payload"
        |> fun p ->
        member p "jobs"
        |> function
        | `Array [ j ] -> j
        | _ -> failwith "jobs"
      in
      assert (
        D.Json.equal (member preserved_job "future_job") (member raw_job "future_job"));
      assert (
        D.Json.equal
          (member (member preserved_job "inference_origin") "future_origin")
          (`String "retained"))));
  print_endline
    "whole-state association, unknown custody and queued first/retry cancellation";
  [%expect
    {| whole-state association, unknown custody and queued first/retry cancellation |}]
;;
