open! Core
open Agent_server_test_support
open Retry_support
module S = Caused_async_support
module PC = S.PC
module Cause = S.Cause
module Ledger = S.Ledger
module Model = S.Model

let marker = "597_CAUSED_DIRECT_MODEL_RETRY"
let recipe = "597-caused-direct-retry.chatmd"
let effect_file = "597-caused-retry-worker.txt"
let output = "actual same Job second Model attempt"

let parent_prompt =
  {|<developer>Actual caused Async owner for direct host Model retry.</developer>
<tool name="retry_recipe" agent="597-caused-direct-retry.chatmd" local/>
<script id="parent-async-policy" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Pre_tool_call(p) ->
  if (if state == 0 then p.name == "read_file" else false) then
    let* job = Tool.spawn("read_file", `Object([
      {key = "root"; value = `String("data")},
      {key = "file"; value = `String("597-caused-retry-worker.txt")}
    ])) in Task.pure(state + 1)
  else Task.pure(state)
| _ -> Task.pure(state)
</script>|}
;;

exception First_provider_failure of string

let first_failure = First_provider_failure "597 actual first Model provider failed"
let first_error = Exn.to_string first_failure

type attempt =
  { observed : S.observation
  ; job : P.Job.t
  ; target : Jsonaf.t
  }

type pair =
  { born : P.Job.t
  ; first : attempt
  }

type phase =
  | Awaiting_admission
  | Awaiting_first of P.Job.t
  | First_observed of pair
  | Awaiting_second of pair
  | Second_observed of pair * attempt

type provider_action =
  | Fail_first
  | Return_second

let find_job (current : A.Session_state.t) id =
  List.find_exn current.jobs ~f:(fun job -> P.Id.Job.equal job.id id)
;;

let current_job (subject : S.probe) id = find_job (state subject.parent) id

let immutable (before : P.Job.t) (after : P.Job.t) =
  assert (P.Id.Job.equal before.id after.id);
  assert (P.Id.Session.equal before.session_id after.session_id);
  [%test_eq: int] before.generation after.generation;
  [%test_eq: P.Job.kind] before.kind after.kind;
  assert (Jsonaf.exactly_equal before.payload after.payload);
  assert (
    Sexp.equal
      (P.Job.sexp_of_retry_policy before.retry_policy)
      (P.Job.sexp_of_retry_policy after.retry_policy));
  assert (P.Timestamp.equal before.created_at after.created_at);
  assert (Option.equal P.Job.equal_launch before.launch after.launch);
  assert (
    Option.equal P.Inference_origin.equal before.inference_origin after.inference_origin)
;;

let direct_parent (subject : S.probe) (job : P.Job.t) =
  let launch = Option.value_exn job.launch in
  (match P.Job.Birth_parent.view launch.owner with
   | Job_attempt { id; attempt } ->
     assert (P.Id.Job.equal subject.job.id id);
     [%test_eq: int] subject.job.attempt attempt
   | Callback _ -> failwith "direct Model borrowed an ancestor callback");
  (match launch.parent_job with
   | Some (id, attempt) ->
     assert (P.Id.Job.equal subject.job.id id);
     [%test_eq: int] subject.job.attempt attempt
   | None -> failwith "direct Model lost its actual Async parent");
  let parent_launch = Option.value_exn subject.job.launch in
  [%test_eq: int] (parent_launch.nested_depth + 1) launch.nested_depth;
  assert (
    Option.equal
      P.Invocation.equal_observer
      parent_launch.moderator_source
      launch.moderator_source);
  assert (
    Option.equal
      P.Inference_origin.equal
      subject.job.inference_origin
      job.inference_origin)
;;

let target (subject : S.probe) (job : P.Job.t) =
  let binding =
    List.filter (state subject.parent).model_job_targets ~f:(fun binding ->
      P.Id.Job.equal job.id (A.Model_job_target.job_id binding)
      && Int.equal job.generation (A.Model_job_target.generation binding))
    |> Model.only "actual retry Model target"
  in
  List.iter
    [ A.Model_job_target.source binding; A.Model_job_target.execution binding ]
    ~f:(fun selection ->
      match Inference.Selection.view selection with
      | Captured _ -> ()
      | Unresolved -> failwith "Model provider preceded its actual target capture");
  A.Model_job_target.to_json binding
;;

let interrupted current (attempt : attempt) =
  match
    Inference.Observation.Attempt_record.state
      (Ledger.Row.record (S.row current attempt.observed.handle))
  with
  | Interrupted { reason = Host_interrupted; _ } -> ()
  | Prepared | Running | Terminal _ | Interrupted { reason = Cancelled; _ } ->
    failwith "actual first provider failure lost its interrupted H"
;;

let native_failed current pair =
  match A.Goal_work_attribution.run_of_job current pair.born |> protocol_ok with
  | None ->
    assert (
      Option.equal P.Inference_origin.equal (Some Unbound) pair.born.inference_origin)
  | Some run ->
    let work = A.Run_job_occurrence.work pair.first.job |> protocol_ok in
    assert (
      List.exists run.terminal_work ~f:(fun proof ->
        P.Run_work.equal work proof.work
        && P.Run_work.Terminal.equal_outcome Failed proof.outcome))
;;

let native_successor current (job : P.Job.t) =
  match A.Goal_work_attribution.run_of_job current job |> protocol_ok with
  | None ->
    assert (Option.equal P.Inference_origin.equal (Some Unbound) job.inference_origin)
  | Some run ->
    let work = A.Run_job_occurrence.work job |> protocol_ok in
    (match job.status with
     | Queued -> assert (List.mem run.owned_work work ~equal:P.Run_work.equal)
     | Succeeded ->
       assert (
         List.exists run.terminal_work ~f:(fun proof ->
           P.Run_work.equal work proof.work
           && P.Run_work.Terminal.equal_outcome Succeeded proof.outcome))
     | _ -> failwith "native retry proof checked at the wrong actual state")
;;

let await_job (fixture : S.fixture) select =
  S.Failure.restore fixture.failure (fun () ->
    Eio.Fiber.first
      (fun () -> Fixture_phase.await_change fixture.probe.parent select)
      (fun () -> S.Failure.await_and_raise fixture.failure))
;;

let admit fixture =
  let subject = fixture.S.probe in
  Actor.with_job_execution
    subject.parent.actor
    ~job_id:subject.job.id
    ~generation:subject.job.generation
    ~attempt:subject.job.attempt
    ~deadline:None
    (fun execution ->
       let current = state subject.parent in
       let candidate : P.Job.t =
         { id = P.Id.Job.create ()
         ; session_id = current.identity.session_id
         ; generation = current.identity.generation
         ; kind = Model_call
         ; payload =
             `Object
               [ "recipe", `String Chat_response.Model_executor.agent_prompt_v1_name
               ; ( "payload"
                 , `Object
                     [ "prompt", `String recipe
                     ; "input", `String marker
                     ; "is_local", `True
                     ] )
               ]
         ; status = Queued
         ; retry_policy = Safe_retry { max_attempts = 2; backoff_ms = 0 }
         ; attempt = 0
         ; created_at =
             P.Timestamp.of_time_ns
               (Time_ns.of_span_since_epoch (Time_ns.Span.of_sec (fixture.clock_now ())))
         ; started_at = None
         ; next_run_at = None
         ; completed_at = None
         ; result = None
         ; delivery = Pending
         ; launch = None
         ; inference_origin = None
         ; progress = None
         }
       in
       A.Model_job_admission.admit execution.model_jobs candidate)
  |> protocol_ok
;;

let%expect_test
    "actual caused direct Model retry retains failed H and confirms distinct H2"
  =
  let phase = ref Awaiting_admission in
  let pause = ref ignore in
  let providers = ref 0 in
  let at_model failure subject =
    let action =
      S.Failure.guard failure (fun () ->
        Int.incr providers;
        let born, previous =
          match !phase with
          | Awaiting_first born -> born, None
          | Awaiting_second pair -> pair.born, Some pair
          | Awaiting_admission | First_observed _ | Second_observed _ ->
            failwith "unexpected Model provider outside the actual retry phase"
        in
        let attempt_number = if Option.is_none previous then 1 else 2 in
        let job = current_job subject born.id in
        immutable born job;
        direct_parent subject job;
        [%test_eq: int] attempt_number job.attempt;
        assert (P.Job.equal_delivery Pending job.delivery);
        assert (Option.is_some job.started_at);
        assert (Option.is_none job.next_run_at);
        assert (Option.is_none job.completed_at);
        assert (Option.is_none job.result);
        let observed = S.observe_attempt subject job in
        let current = { observed; job; target = target subject job } in
        match previous with
        | None ->
          [%test_eq: int] 1 !providers;
          phase := First_observed { born; first = current };
          Fail_first
        | Some pair ->
          [%test_eq: int] 2 !providers;
          interrupted (state subject.parent) pair.first;
          native_failed (state subject.parent) pair;
          assert (Jsonaf.exactly_equal pair.first.target current.target);
          assert (not (Ledger.Handle.equal pair.first.observed.handle observed.handle));
          [%test_eq: int64]
            Int64.(Ledger.Handle.ordinal pair.first.observed.handle + 1L)
            (Ledger.Handle.ordinal observed.handle);
          assert (
            not
              (A.Goal_descendant_occurrence.equal
                 pair.first.observed.occurrence
                 observed.occurrence));
          assert (PC.Stream.equal pair.first.observed.stream observed.stream);
          [%test_eq: int64] 2L pair.first.observed.prefix;
          [%test_eq: int64] 3L observed.prefix;
          phase := Second_observed (pair, current);
          Return_second)
    in
    match action with
    | Fail_first ->
      !pause ();
      raise first_failure
    | Return_second -> completed_response output
  in
  S.with_claimed_async
    ~scenario:S.Direct_model_retry
    ~parent_prompt_text:parent_prompt
    ~sources:
      [ S.original_file, "real delegated read"
      ; effect_file, "real Async native read"
      ; recipe, "<developer>597 actual direct Model retry recipe.</developer>"
      ]
    ~model_marker:marker
    ~at_model
    (fun env sw daemon fixture ->
       let subject = fixture.probe in
       pause := fixture.pause;
       S.run_direct sw fixture;
       [%test_eq: int] 0 !providers;
       let born = admit fixture in
       direct_parent subject born;
       [%test_eq: int] 0 born.attempt;
       phase := Awaiting_first born;
       fixture.resume ();
       let pair, queued =
         Model.within env "actual failed Model retry and parked scheduler" (fun () ->
           let pair, queued =
             await_job fixture (fun current ->
               match !phase with
               | First_observed pair ->
                 let job = find_job current born.id in
                 (match job.status with
                  | Queued -> Some (pair, job)
                  | _ -> None)
               | Awaiting_admission | Awaiting_first _ -> None
               | Awaiting_second _ | Second_observed _ ->
                 failwith "retry crossed the parked first failure")
           in
           let rec await_park () =
             S.Failure.raise_if_present fixture.failure;
             if not (S.parked daemon fixture.clock_now)
             then (
               Eio.Fiber.yield ();
               await_park ())
           in
           await_park ();
           let parked_job = current_job subject born.id in
           assert (S.parked daemon fixture.clock_now);
           assert (Sexp.equal (P.Job.sexp_of_t queued) (P.Job.sexp_of_t parked_job));
           pair, parked_job)
       in
       Fixture_progress.mark
         (Some fixture.progress)
         "deliver the retry of the same Model Job";
       immutable born queued;
       direct_parent subject queued;
       [%test_eq: int] 1 queued.attempt;
       assert (Option.is_none queued.started_at);
       assert (Option.is_none queued.completed_at);
       assert (Option.is_some queued.next_run_at);
       assert (P.Job.equal_delivery Pending queued.delivery);
       assert (
         Option.equal
           Jsonaf.exactly_equal
           (Some (`Object [ "last_error", `String first_error ]))
           queued.result);
       let daemon_now =
         P.Timestamp.of_time_ns
           (Time_ns.of_span_since_epoch (Time_ns.Span.of_sec (fixture.clock_now ())))
       in
       assert (P.Timestamp.compare (Option.value_exn queued.next_run_at) daemon_now <= 0);
       [%test_eq: int] 1 !providers;
       let failed = state subject.parent in
       interrupted failed pair.first;
       native_failed failed pair;
       native_successor failed queued;
       assert (Jsonaf.exactly_equal pair.first.target (target subject queued));
       [%test_eq: int] 2 (List.length (Model.new_rows subject.before_parent failed));
       [%test_eq: int64]
         Int64.(Model.committed subject.before_parent + 3L)
         (Model.committed failed);
       assert (List.is_empty (S.Book.Subtotal.pending (Model.subtotal failed)));
       (match (current_job subject subject.job.id).status with
        | Running -> ()
        | _ -> failwith "actual retry lost its live Async parent");
       let child = state subject.child in
       [%test_eq: int64]
         2L
         (A.Goal_usage.attempts (S.Book.Subtotal.usage (Model.subtotal child)));
       assert (List.is_empty (S.Book.Subtotal.pending (Model.subtotal child)));
       [%test_eq: int64] Int64.(subject.ceiling - 1L) (Model.committed child);
       phase := Awaiting_second pair;
       fixture.resume ();
       let final_model =
         Model.within env "actual same Model attempt2 success and delivery" (fun () ->
           await_job fixture (fun current ->
             let job = find_job current born.id in
             match !phase, job.status, job.delivery with
             | Second_observed _, Succeeded, Delivered _ -> Some job
             | _, (Failed _ | Cancelled | Interrupted _), _ ->
               failwith "actual second Model attempt failed"
             | _ -> None))
       in
       Fixture_progress.mark
         (Some fixture.progress)
         "finish the Async worker and child after H2 delivery";
       let pair, second =
         match !phase with
         | Second_observed (pair, second) -> pair, second
         | _ -> assert false
       in
       immutable born final_model;
       direct_parent subject final_model;
       [%test_eq: int] 2 final_model.attempt;
       assert (Option.is_some final_model.completed_at);
       assert (Option.is_none final_model.next_run_at);
       let result = Option.value_exn final_model.result in
       List.iter
         [ "recipe", Chat_response.Model_executor.agent_prompt_v1_name
         ; "prompt", recipe
         ; "session_id", recipe
         ; "final_text", output
         ]
         ~f:(fun (field, expected) ->
           assert (
             Option.equal
               String.equal
               (Some expected)
               (Jsonaf.member field result |> Option.bind ~f:Jsonaf.string)));
       List.iter [ "is_local"; "terminated_normally" ] ~f:(fun field ->
         assert (Jsonaf.exactly_equal `True (Jsonaf.member_exn field result)));
       assert (Jsonaf.exactly_equal second.target (target subject final_model));
       let settled = Model.checkpoint subject.parent in
       interrupted settled pair.first;
       S.row settled second.observed.handle |> S.assert_completed;
       native_failed settled pair;
       native_successor settled final_model;
       S.execute_async daemon fixture ~check_completion:(function
         | Succeeded (`String text) ->
           assert (String.is_substring text ~substring:"real Async native read")
         | completion -> raise_s [%sexp (completion : P.Completion.t)]);
       let final_parent = S.finish_child env fixture in
       [%test_eq: int] 2 !providers;
       let retained = current_job subject born.id in
       assert (Sexp.equal (P.Job.sexp_of_t final_model) (P.Job.sexp_of_t retained));
       interrupted final_parent pair.first;
       S.row final_parent second.observed.handle |> S.assert_completed;
       let direct = List.hd_exn subject.observed in
       S.row final_parent direct.handle |> S.assert_completed);
  print_endline
    "same actual direct Model Job retried1→2; failed H retained; distinct H2 confirmed \
     both Goals before provider; actual delivery and native finish";
  [%expect
    {| same actual direct Model Job retried1→2; failed H retained; distinct H2 confirmed both Goals before provider; actual delivery and native finish |}]
;;
